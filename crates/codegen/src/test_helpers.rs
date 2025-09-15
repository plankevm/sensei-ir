//! Test helper functions to reduce boilerplate in tests

use eth_ir_data::{
    BasicBlock, BasicBlockId, Control, DataOffset, EthIRProgram, Function, FunctionId, LocalId,
    LocalIndex, OperationIndex, index::*, operation::*,
};
use evm_glue::assembly::Asm;

/// Creates a simple program with a single function and basic block
pub fn create_simple_program(operations: Vec<Operation>) -> EthIRProgram {
    let num_ops = operations.len();
    let num_locals = calculate_max_local_id(&operations) + 1;

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
        basic_blocks: index_vec![BasicBlock {
            inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(num_ops),
            control: Control::LastOpTerminates,
        }],
        operations: operations.into_iter().collect(),
        locals: (0..num_locals).map(|i| LocalId::new(i as u32)).collect(),
        data_segments_start: index_vec![],
        data_bytes: index_vec![],
        large_consts: index_vec![],
        cases: index_vec![],
    }
}

/// Creates a program with multiple basic blocks for testing control flow
pub fn create_branching_program(
    blocks: Vec<(Vec<Operation>, Control)>,
    num_locals: usize,
) -> EthIRProgram {
    // Pre-calculate total operations count for better allocation
    let total_ops: usize = blocks.iter().map(|(ops, _)| ops.len()).sum();
    let mut all_operations = Vec::with_capacity(total_ops);
    let mut basic_blocks = IndexVec::new();
    let mut current_op_index = 0;

    for (ops, control) in blocks {
        let num_ops = ops.len();
        basic_blocks.push(BasicBlock {
            inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            operations: OperationIndex::from_usize(current_op_index)
                ..OperationIndex::from_usize(current_op_index + num_ops),
            control,
        });
        all_operations.extend(ops);
        current_op_index += num_ops;
    }

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
        basic_blocks,
        operations: all_operations.into_iter().collect(),
        locals: (0..num_locals).map(|i| LocalId::new(i as u32)).collect(),
        data_segments_start: index_vec![],
        data_bytes: index_vec![],
        large_consts: index_vec![],
        cases: index_vec![],
    }
}

/// Creates a program with data segments
pub fn create_program_with_data(
    operations: Vec<Operation>,
    data_segments: Vec<Vec<u8>>,
) -> EthIRProgram {
    let num_ops = operations.len();
    let num_locals = calculate_max_local_id(&operations) + 1;

    let mut data_segments_start = index_vec![];
    let mut data_bytes = index_vec![];
    let mut current_offset = 0;

    for segment in data_segments {
        data_segments_start.push(DataOffset::new(current_offset));
        current_offset += segment.len() as u32;
        data_bytes.extend(segment);
    }

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
        basic_blocks: index_vec![BasicBlock {
            inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(num_ops),
            control: Control::LastOpTerminates,
        }],
        operations: operations.into_iter().collect(),
        locals: (0..num_locals).map(|i| LocalId::new(i as u32)).collect(),
        data_segments_start,
        data_bytes,
        large_consts: index_vec![],
        cases: index_vec![],
    }
}

/// Helper to calculate the maximum local ID used in operations
pub fn calculate_max_local_id(operations: &[Operation]) -> usize {
    let mut max_id = 0;
    for op in operations {
        use Operation::*;
        match op {
            // Operations with result locals
            LocalSetSmallConst(SetSmallConst { local, .. })
            | LocalSetLargeConst(SetLargeConst { local, .. })
            | LocalSetDataOffset(SetDataOffset { local, .. }) => {
                max_id = max_id.max(local.get() as usize);
            }

            AcquireFreePointer(ZeroInOneOut { result })
            | ReturnDataSize(ZeroInOneOut { result })
            | ChainId(ZeroInOneOut { result })
            | BaseFee(ZeroInOneOut { result })
            | BlobBaseFee(ZeroInOneOut { result })
            | Address(ZeroInOneOut { result })
            | Origin(ZeroInOneOut { result })
            | Caller(ZeroInOneOut { result })
            | CallValue(ZeroInOneOut { result })
            | GasPrice(ZeroInOneOut { result })
            | Coinbase(ZeroInOneOut { result })
            | Timestamp(ZeroInOneOut { result })
            | Number(ZeroInOneOut { result })
            | Difficulty(ZeroInOneOut { result })
            | GasLimit(ZeroInOneOut { result })
            | CallDataSize(ZeroInOneOut { result })
            | CodeSize(ZeroInOneOut { result })
            | Gas(ZeroInOneOut { result })
            | SelfBalance(ZeroInOneOut { result }) => {
                max_id = max_id.max(result.get() as usize);
            }

            BlockHash(OneInOneOut { result, arg1 })
            | TLoad(OneInOneOut { result, arg1 })
            | ExtCodeSize(OneInOneOut { result, arg1 })
            | ExtCodeHash(OneInOneOut { result, arg1 })
            | Balance(OneInOneOut { result, arg1 })
            | BlobHash(OneInOneOut { result, arg1 })
            | Not(OneInOneOut { result, arg1 })
            | IsZero(OneInOneOut { result, arg1 })
            | SLoad(OneInOneOut { result, arg1 })
            | DynamicAllocAnyBytes(OneInOneOut { result, arg1 })
            | DynamicAllocZeroed(OneInOneOut { result, arg1 })
            | LocalSet(OneInOneOut { result, arg1 })
            | CallDataLoad(OneInOneOut { result, arg1 }) => {
                max_id = max_id.max(result.get() as usize);
                max_id = max_id.max(arg1.get() as usize);
            }

            MemoryLoad(ml) => {
                max_id = max_id.max(ml.result.get() as usize);
                max_id = max_id.max(ml.address.get() as usize);
            }

            MemoryStore(ms) => {
                max_id = max_id.max(ms.address.get() as usize);
                max_id = max_id.max(ms.value.get() as usize);
            }

            Add(TwoInOneOut { result, arg1, arg2 })
            | Mul(TwoInOneOut { result, arg1, arg2 })
            | Sub(TwoInOneOut { result, arg1, arg2 })
            | Div(TwoInOneOut { result, arg1, arg2 })
            | SDiv(TwoInOneOut { result, arg1, arg2 })
            | Mod(TwoInOneOut { result, arg1, arg2 })
            | SMod(TwoInOneOut { result, arg1, arg2 })
            | Exp(TwoInOneOut { result, arg1, arg2 })
            | SignExtend(TwoInOneOut { result, arg1, arg2 })
            | Lt(TwoInOneOut { result, arg1, arg2 })
            | Gt(TwoInOneOut { result, arg1, arg2 })
            | SLt(TwoInOneOut { result, arg1, arg2 })
            | SGt(TwoInOneOut { result, arg1, arg2 })
            | Eq(TwoInOneOut { result, arg1, arg2 })
            | And(TwoInOneOut { result, arg1, arg2 })
            | Or(TwoInOneOut { result, arg1, arg2 })
            | Xor(TwoInOneOut { result, arg1, arg2 })
            | Byte(TwoInOneOut { result, arg1, arg2 })
            | Shl(TwoInOneOut { result, arg1, arg2 })
            | Shr(TwoInOneOut { result, arg1, arg2 })
            | Sar(TwoInOneOut { result, arg1, arg2 })
            | Keccak256(TwoInOneOut { result, arg1, arg2 }) => {
                max_id = max_id.max(result.get() as usize);
                max_id = max_id.max(arg1.get() as usize);
                max_id = max_id.max(arg2.get() as usize);
            }

            SStore(TwoInZeroOut { arg1, arg2 })
            | TStore(TwoInZeroOut { arg1, arg2 })
            | Return(TwoInZeroOut { arg1, arg2 })
            | Revert(TwoInZeroOut { arg1, arg2 })
            | Log0(TwoInZeroOut { arg1, arg2 }) => {
                max_id = max_id.max(arg1.get() as usize);
                max_id = max_id.max(arg2.get() as usize);
            }

            MCopy(ThreeInZeroOut { arg1, arg2, arg3 })
            | CallDataCopy(ThreeInZeroOut { arg1, arg2, arg3 })
            | ReturnDataCopy(ThreeInZeroOut { arg1, arg2, arg3 })
            | CodeCopy(ThreeInZeroOut { arg1, arg2, arg3 })
            | Log1(ThreeInZeroOut { arg1, arg2, arg3 }) => {
                max_id = max_id.max(arg1.get() as usize);
                max_id = max_id.max(arg2.get() as usize);
                max_id = max_id.max(arg3.get() as usize);
            }

            SelfDestruct(OneInZeroOut { arg1 }) => {
                max_id = max_id.max(arg1.get() as usize);
            }

            AddMod(LargeInOneOut { result, args_start })
            | MulMod(LargeInOneOut { result, args_start })
            | Create(LargeInOneOut { result, args_start })
            | Create2(LargeInOneOut { result, args_start })
            | Call(LargeInOneOut { result, args_start })
            | CallCode(LargeInOneOut { result, args_start })
            | DelegateCall(LargeInOneOut { result, args_start })
            | StaticCall(LargeInOneOut { result, args_start }) => {
                max_id = max_id.max(result.get() as usize);
                // These use a range of locals starting from args_start
                // We'd need to know how many args to calculate max properly
                // For now, assume they use at least 7 locals (max for CALL)
                max_id = max_id.max(args_start.get() as usize + 7);
            }

            Log2(LargeInZeroOut { args_start })
            | Log3(LargeInZeroOut { args_start })
            | Log4(LargeInZeroOut { args_start })
            | ExtCodeCopy(LargeInZeroOut { args_start }) => {
                // These use a range of locals starting from args_start
                // We'd need to know how many args to calculate max properly
                // For now, assume they use at least 6 locals (max for LOG4)
                max_id = max_id.max(args_start.get() as usize + 6);
            }

            Stop | Invalid | NoOp => {
                // No locals referenced
            }

            LocalAllocZeroed(OneInOneOut { result, arg1 })
            | LocalAllocAnyBytes(OneInOneOut { result, arg1 }) => {
                max_id = max_id.max(result.get() as usize);
                max_id = max_id.max(arg1.get() as usize);
            }

            DynamicAllocUsingFreePointer(TwoInZeroOut { arg1, arg2 }) => {
                max_id = max_id.max(arg1.get() as usize);
                max_id = max_id.max(arg2.get() as usize);
            }

            InternalCall(ic) => {
                // InternalCall uses ranges of locals for args and outputs
                // We need to handle the ranges properly
                let args_end = ic.args_start.get() as usize + 10; // Conservative estimate
                let outputs_end = ic.outputs_start.get() as usize + 10; // Conservative estimate
                max_id = max_id.max(args_end);
                max_id = max_id.max(outputs_end);
            }

            RuntimeStartOffset(ZeroInOneOut { result })
            | InitEndOffset(ZeroInOneOut { result })
            | RuntimeLength(ZeroInOneOut { result }) => {
                max_id = max_id.max(result.get() as usize);
            }
        }
    }
    max_id
}

/// Assert that assembly contains specific opcodes with exact counts
pub fn assert_opcode_counts(asm: &[Asm], expected: &[(&str, usize)]) {
    for (opcode_name, expected_count) in expected {
        let actual_count = count_opcode(asm, opcode_name);
        assert_eq!(
            actual_count, *expected_count,
            "Opcode {} count mismatch: expected {}, got {}",
            opcode_name, expected_count, actual_count
        );
    }
}

/// Count occurrences of a specific opcode
pub fn count_opcode(asm: &[Asm], opcode_name: &str) -> usize {
    asm.iter()
        .filter(|instr| {
            if let Asm::Op(opcode) = instr {
                let opcode_str = format!("{:?}", opcode);
                // Extract just the opcode name (before any parentheses)
                if let Some(name) = opcode_str.split('(').next() {
                    name == opcode_name
                } else {
                    false
                }
            } else {
                false
            }
        })
        .count()
}

/// Debug helper to print assembly
pub fn print_assembly(asm: &[Asm]) {
    eprintln!("\n=== Generated Assembly ===");
    for (i, instr) in asm.iter().enumerate() {
        eprintln!("{:4}: {:?}", i, instr);
    }
    eprintln!("==========================\n");
}

/// Create a vector of operations for testing arithmetic
pub fn arithmetic_operations() -> Vec<Operation> {
    vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 10 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 20 }),
        Operation::Add(TwoInOneOut {
            result: LocalId::new(2),
            arg1: LocalId::new(0),
            arg2: LocalId::new(1),
        }),
        Operation::Sub(TwoInOneOut {
            result: LocalId::new(3),
            arg1: LocalId::new(1),
            arg2: LocalId::new(0),
        }),
        Operation::Mul(TwoInOneOut {
            result: LocalId::new(4),
            arg1: LocalId::new(0),
            arg2: LocalId::new(1),
        }),
        Operation::Div(TwoInOneOut {
            result: LocalId::new(5),
            arg1: LocalId::new(1),
            arg2: LocalId::new(0),
        }),
        Operation::Stop,
    ]
}

/// Create a vector of operations for testing storage
pub fn storage_operations() -> Vec<Operation> {
    vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 1 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 42 }),
        Operation::SStore(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
        Operation::SLoad(OneInOneOut { result: LocalId::new(2), arg1: LocalId::new(0) }),
        Operation::Stop,
    ]
}

/// Create extended storage operations including transient storage
pub fn extended_storage_operations() -> Vec<Operation> {
    let mut ops = storage_operations();
    ops.pop(); // Remove Stop
    ops.extend([
        Operation::TStore(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
        Operation::TLoad(OneInOneOut { result: LocalId::new(3), arg1: LocalId::new(0) }),
        Operation::Stop,
    ]);
    ops
}
