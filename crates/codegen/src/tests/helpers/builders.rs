use super::constants;
use crate::translate_program;
use alloy_primitives::U256;
use eth_ir_data::{
    BasicBlock, BasicBlockId, Case, Cases, CasesId, Control, EthIRProgram, Function, FunctionId,
    Idx, IndexVec, LocalId, LocalIndex, Operation, OperationIndex, index_vec, operation::*,
};
use evm_glue::{assembler::assemble_minimized, assembly::Asm};
use revm::{
    Evm, InMemoryDB,
    primitives::{
        AccountInfo, Bytecode, ExecutionResult, Output, SuccessReason, TransactTo, address,
    },
};
use std::borrow::Cow;
use test_utils::parser::parse_e2e;

/// Builder for creating and testing sequences of IR operations
/// This builder simplifies the creation of test cases by:
/// - Managing local ID allocation automatically
/// - Providing fluent API for adding operations
/// - Handling compilation and execution
/// - Supporting both execution testing and opcode verification
pub struct OperationTestBuilder {
    operations: Vec<Operation>,
    locals_counter: u32,
}

impl OperationTestBuilder {
    pub fn new() -> Self {
        Self { operations: Vec::new(), locals_counter: 0 }
    }

    pub fn with_values(mut self, values: &[u64]) -> Self {
        for &value in values {
            self.operations.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(self.locals_counter),
                value,
            }));
            self.locals_counter += 1;
        }
        self
    }

    pub fn add_operation(mut self, op: impl FnOnce(Vec<LocalId>) -> Operation) -> Self {
        let locals: Vec<LocalId> = (0..self.locals_counter).map(LocalId::new).collect();
        self.operations.push(op(locals));
        self
    }

    pub fn add_binary_op(
        mut self,
        create_op: impl FnOnce(LocalId, LocalId, LocalId) -> Operation,
    ) -> Self {
        let result = LocalId::new(self.locals_counter);
        self.locals_counter += 1;
        self.operations.push(create_op(result, LocalId::new(0), LocalId::new(1)));
        self
    }

    pub fn add_unary_op(mut self, create_op: impl FnOnce(LocalId, LocalId) -> Operation) -> Self {
        let result = LocalId::new(self.locals_counter);
        self.locals_counter += 1;
        self.operations.push(create_op(result, LocalId::new(0)));
        self
    }

    pub fn add_zero_input_op(mut self, create_op: impl FnOnce(LocalId) -> Operation) -> Self {
        let result = LocalId::new(self.locals_counter);
        self.locals_counter += 1;
        self.operations.push(create_op(result));
        self
    }

    pub fn with_stop(mut self) -> Self {
        self.operations.push(Operation::Stop);
        self
    }

    pub fn with_operations(mut self, operations: Vec<Operation>) -> Self {
        let max_local = operations.iter().map(|op| op.get_max_local_id()).max().unwrap_or(0);
        self.locals_counter = self.locals_counter.max(max_local + 1);
        self.operations.extend(operations);
        self
    }

    pub fn build(self) -> Vec<Operation> {
        self.operations
    }

    pub fn build_and_translate(self) -> Vec<Asm> {
        self.build_program().translate()
    }

    pub fn build_and_execute(self, return_local: u32) -> Result<U256, String> {
        self.with_return(return_local).execute()
    }

    pub fn with_return(mut self, return_local: u32) -> Self {
        let offset_local = LocalId::new(self.locals_counter);
        let size_local = LocalId::new(self.locals_counter + 1);
        self.locals_counter += 2;

        self.operations.extend([
            Operation::LocalSetSmallConst(SetSmallConst { local: offset_local, value: 0 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: size_local, value: 32 }),
            Operation::MemoryStore(MemoryStore {
                address: offset_local,
                value: LocalId::new(return_local),
                byte_size: 32,
            }),
            Operation::Return(TwoInZeroOut { arg1: offset_local, arg2: size_local }),
        ]);
        self
    }

    pub fn build_program(self) -> TestProgram {
        TestProgram::from_operations(self.operations)
    }

    pub fn execute(self) -> Result<U256, String> {
        self.build_program().execute()
    }
}

/// Wrapper around an EthIRProgram that provides common test operations
pub struct TestProgram {
    program: EthIRProgram,
}

impl TestProgram {
    pub fn from_operations(operations: Vec<Operation>) -> Self {
        Self { program: create_simple_program(operations) }
    }

    pub fn from_program(program: EthIRProgram) -> Self {
        Self { program }
    }

    pub fn translate(self) -> Vec<Asm> {
        translate_program(self.program)
    }

    pub fn into_bytecode(self) -> Result<Vec<u8>, String> {
        let asm = self.translate();
        let (_, bytecode) =
            assemble_minimized(&asm, true).map_err(|e| format!("Assembly failed: {:?}", e))?;
        Ok(bytecode)
    }

    pub fn execute(self) -> Result<U256, String> {
        let bytecode = self.into_bytecode()?;
        execute_bytecode(bytecode)
    }
}

pub struct EvmBuilder {
    db: InMemoryDB,
    contract_address: revm::primitives::Address,
    caller_address: revm::primitives::Address,
    bytecode: Vec<u8>,
    calldata: Vec<u8>,
    gas_limit: u64,
}

impl EvmBuilder {
    pub fn new() -> Self {
        Self {
            db: InMemoryDB::default(),
            contract_address: address!("1000000000000000000000000000000000000000"),
            caller_address: address!("9000000000000000000000000000000000000000"),
            bytecode: Vec::new(),
            calldata: Vec::new(),
            gas_limit: 1_000_000,
        }
    }

    pub fn with_bytecode(mut self, bytecode: Vec<u8>) -> Self {
        self.bytecode = bytecode;
        self
    }

    pub fn with_calldata(mut self, calldata: Vec<u8>) -> Self {
        self.calldata = calldata;
        self
    }

    pub fn with_gas_limit(mut self, gas_limit: u64) -> Self {
        self.gas_limit = gas_limit;
        self
    }

    pub fn build(mut self) -> Evm<'static, (), InMemoryDB> {
        self.db.insert_account_info(
            self.contract_address,
            AccountInfo {
                balance: U256::ZERO,
                nonce: 0,
                code_hash: revm::primitives::keccak256(&self.bytecode),
                code: Some(Bytecode::new_raw(self.bytecode.clone().into())),
            },
        );

        self.db.insert_account_info(
            self.caller_address,
            AccountInfo {
                balance: U256::from(1_000_000_000_000_000_000u64),
                nonce: 0,
                code_hash: revm::primitives::KECCAK_EMPTY,
                code: None,
            },
        );

        Evm::builder()
            .with_db(self.db)
            .modify_tx_env(|tx| {
                tx.caller = self.caller_address;
                tx.transact_to = TransactTo::Call(self.contract_address);
                tx.data = self.calldata.into();
                tx.gas_limit = self.gas_limit;
                tx.gas_price = U256::from(1);
                tx.value = U256::ZERO;
            })
            .build()
    }
}

pub fn create_simple_program(mut operations: Vec<Operation>) -> EthIRProgram {
    // Init code must end with RETURN to deploy runtime code (when there's runtime code)
    // For test helpers, we add a minimal RETURN that deploys empty runtime code
    // BUT: if program explicitly uses Stop/Revert/Invalid, respect that choice
    let has_explicit_terminator = operations.iter().any(|op| op.is_terminator());

    if !has_explicit_terminator {
        let max_local_id = find_max_local_id(&operations);

        // Add minimal deployment RETURN(0, 0) - returns empty runtime code
        let zero_local = LocalId::new(max_local_id + 1);
        operations
            .push(Operation::LocalSetSmallConst(SetSmallConst { local: zero_local, value: 0 }));
        operations.push(Operation::Return(TwoInZeroOut {
            arg1: zero_local,
            arg2: zero_local, // Return 0 bytes (empty deployment)
        }));
    }

    let max_local_id = find_max_local_id(&operations);
    let locals = (0..=max_local_id).map(LocalId::new).collect();
    let ops_vec: eth_ir_data::IndexVec<OperationIndex, Operation> =
        operations.into_iter().collect();
    let ops_range = OperationIndex::from_usize(0)..OperationIndex::from_usize(ops_vec.len());

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
        basic_blocks: index_vec![BasicBlock {
            inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            operations: ops_range,
            control: Control::LastOpTerminates,
        }],
        operations: ops_vec,
        locals,
        data_segments_start: index_vec![],
        data_bytes: index_vec![],
        large_consts: index_vec![],
        cases: index_vec![],
        next_free_local_id: LocalId::new(max_local_id + 1),
    }
}

fn find_max_local_id(operations: &[Operation]) -> u32 {
    operations.iter().map(|op| op.get_max_local_id()).max().unwrap_or(0)
}

/// Parse human-readable IR string into an EthIRProgram
pub fn parse_ir(ir_source: &str) -> Result<EthIRProgram, String> {
    let ast = parse_e2e(ir_source);
    (&ast).try_into().map_err(|e: Cow<'static, str>| e.to_string())
}

pub fn create_branching_program(
    blocks: Vec<(Vec<Operation>, Control)>,
    start_block: usize,
) -> EthIRProgram {
    let (all_operations, basic_blocks) = build_blocks_from_ops(blocks);
    let max_local_id = find_max_local_id(&all_operations);
    let locals = (0..=max_local_id + 200).map(LocalId::new).collect();
    let ops_vec = all_operations.into_iter().collect();

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: index_vec![Function {
            entry: BasicBlockId::new(start_block as u32),
            outputs: 0
        }],
        basic_blocks,
        operations: ops_vec,
        locals,
        data_segments_start: index_vec![],
        data_bytes: index_vec![],
        large_consts: index_vec![],
        cases: index_vec![],
        next_free_local_id: LocalId::new(max_local_id + 201),
    }
}

fn build_blocks_from_ops(
    blocks: Vec<(Vec<Operation>, Control)>,
) -> (Vec<Operation>, eth_ir_data::IndexVec<BasicBlockId, BasicBlock>) {
    let mut all_operations = Vec::new();
    let mut basic_blocks = index_vec![];

    for (block_ops, control) in blocks {
        let start_idx = all_operations.len();
        all_operations.extend(block_ops);
        let end_idx = all_operations.len();

        basic_blocks.push(BasicBlock {
            inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            operations: OperationIndex::from_usize(start_idx)..OperationIndex::from_usize(end_idx),
            control,
        });
    }

    (all_operations, basic_blocks)
}

pub fn create_program_with_switch(
    blocks: Vec<(Vec<Operation>, Control)>,
    switch_cases: Vec<Vec<Case>>,
) -> EthIRProgram {
    let (operations_vec, basic_blocks) = build_blocks_from_ops(blocks);
    let operations: IndexVec<OperationIndex, Operation> = operations_vec.into_iter().collect();

    // Build cases index
    let cases: IndexVec<CasesId, Cases> =
        switch_cases.into_iter().map(|case_vec| Cases { cases: case_vec }).collect();

    // Need to determine the number of locals from operations
    let max_local = operations.iter().map(|op| op.get_max_local_id()).max().unwrap_or(0);
    let locals = (0..=max_local + 10).map(LocalId::new).collect();

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
        basic_blocks,
        operations,
        data_segments_start: index_vec![],
        locals,
        data_bytes: index_vec![],
        large_consts: index_vec![],
        cases,
        next_free_local_id: LocalId::new(max_local + 11),
    }
}

type FunctionBlocks = Vec<(Vec<Operation>, Control)>;

pub fn create_multi_function_program(functions: Vec<(FunctionBlocks, usize)>) -> EthIRProgram {
    let mut all_operations = Vec::new();
    let mut all_blocks = index_vec![];
    let mut all_functions = index_vec![];

    for (function_blocks, entry_block) in functions {
        let function_start_block = all_blocks.len();
        let (ops, blocks) = build_blocks_from_ops(function_blocks);
        all_operations.extend(ops);
        all_blocks.extend(blocks);

        all_functions.push(Function {
            entry: BasicBlockId::new((function_start_block + entry_block) as u32),
            outputs: 0,
        });
    }

    let max_local_id = find_max_local_id(&all_operations);
    let locals = (0..=max_local_id + 200).map(LocalId::new).collect();
    let ops_vec = all_operations.into_iter().collect();

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: all_functions,
        basic_blocks: all_blocks,
        operations: ops_vec,
        locals,
        data_segments_start: index_vec![],
        data_bytes: index_vec![],
        large_consts: index_vec![],
        cases: index_vec![],
        next_free_local_id: LocalId::new(max_local_id + 201),
    }
}

pub fn create_ops_with_return(operations: Vec<Operation>, return_local: u32) -> Vec<Operation> {
    OperationTestBuilder::new().with_operations(operations).with_return(return_local).build()
}

pub fn create_return_for_local(
    local_id: u32,
    offset_local: u32,
    size_local: u32,
) -> Vec<Operation> {
    vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(offset_local),
            value: constants::LOCALS_START as u64 + local_id as u64 * constants::SLOT_SIZE as u64,
        }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(size_local), value: 32 }),
        Operation::Return(TwoInZeroOut {
            arg1: LocalId::new(offset_local),
            arg2: LocalId::new(size_local),
        }),
    ]
}

pub fn build_binary_op_with_return<F>(a: u64, b: u64, create_op: F) -> Vec<Operation>
where
    F: FnOnce(LocalId, LocalId, LocalId) -> Operation,
{
    OperationTestBuilder::new().with_values(&[a, b]).add_binary_op(create_op).with_return(2).build()
}

pub fn local_memory_offset(local_id: u32) -> u64 {
    constants::LOCALS_START as u64 + (local_id as u64) * constants::SLOT_SIZE as u64
}

pub fn compile_to_bytecode(operations: Vec<Operation>) -> Vec<u8> {
    TestProgram::from_operations(operations)
        .into_bytecode()
        .expect("Failed to compile operations to bytecode")
}

pub fn execute_and_get_result(bytecode: Vec<u8>) -> Result<U256, String> {
    execute_bytecode(bytecode)
}

pub fn execute_bytecode(bytecode: Vec<u8>) -> Result<U256, String> {
    execute_bytecode_with_calldata(bytecode, vec![])
}

pub fn execute_bytecode_raw(bytecode: Vec<u8>) -> ExecutionResult {
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();
    evm.transact_commit().expect("EVM transaction failed to commit")
}

pub fn execute_and_verify_revert(bytecode: Vec<u8>) -> Result<(), String> {
    let result = execute_bytecode_raw(bytecode);
    match result {
        ExecutionResult::Revert { .. } => Ok(()),
        _ => Err(format!("Expected revert, got: {:?}", result)),
    }
}

pub fn execute_and_get_result_with_calldata(
    bytecode: Vec<u8>,
    calldata: Vec<u8>,
) -> Result<U256, String> {
    execute_bytecode_with_calldata(bytecode, calldata)
}

pub fn execute_bytecode_with_calldata(
    bytecode: Vec<u8>,
    calldata: Vec<u8>,
) -> Result<U256, String> {
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).with_calldata(calldata).build();
    let result = evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;

    match result {
        ExecutionResult::Success { output, .. } => match output {
            Output::Call(bytes) => {
                if bytes.len() >= 32 {
                    Ok(U256::from_be_bytes::<32>(
                        bytes[0..32].try_into().expect("Failed to convert bytes to array"),
                    ))
                } else {
                    Err("Return data too small".to_string())
                }
            }
            _ => Err("Unexpected output type".to_string()),
        },
        ExecutionResult::Revert { output, .. } => Err(format!("Reverted: {:?}", output)),
        ExecutionResult::Halt { reason, .. } => Err(format!("Halted: {:?}", reason)),
    }
}

pub fn execute_and_verify_stop(bytecode: Vec<u8>) -> Result<(), String> {
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();
    let result = evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;

    match result {
        ExecutionResult::Success { reason, .. } => {
            if reason == SuccessReason::Stop {
                Ok(())
            } else {
                Err(format!("Expected STOP but got {:?}", reason))
            }
        }
        _ => Err(format!("Expected successful STOP but got {:?}", result)),
    }
}

pub fn execute_and_verify_halt(bytecode: Vec<u8>) -> Result<(), String> {
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();
    let result = evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;

    match result {
        ExecutionResult::Halt { .. } => Ok(()),
        _ => Err(format!("Expected HALT but got {:?}", result)),
    }
}

pub fn execute_and_verify_result(operations: Vec<Operation>, expected: U256) -> Result<(), String> {
    let result_local = find_result_local(&operations)
        .ok_or_else(|| "No result-producing operation found".to_string())?;

    let actual =
        TestProgram::from_operations(create_ops_with_return(operations, result_local)).execute()?;

    if actual == expected {
        Ok(())
    } else {
        Err(format!("Expected {} but got {}", expected, actual))
    }
}

pub fn execute_storage_operations(operations: Vec<Operation>) -> Result<(), String> {
    let bytecode = TestProgram::from_operations(operations).into_bytecode()?;
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();
    evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;
    Ok(())
}

pub fn assert_opcode_counts(asm: &[Asm], expected: &[(&str, usize)]) {
    for &(opcode, expected_count) in expected {
        let actual_count = count_opcode(asm, opcode);
        assert_eq!(
            actual_count, expected_count,
            "Expected {} {} opcodes but found {}",
            expected_count, opcode, actual_count
        );
    }
}

pub fn count_opcode(asm: &[Asm], opcode: &str) -> usize {
    asm.iter()
        .filter(|&op| match op {
            Asm::Op(op_enum) => format!("{:?}", op_enum) == opcode,
            _ => false,
        })
        .count()
}

pub fn extract_opcode_sequence(asm: &[Asm]) -> Vec<String> {
    asm.iter()
        .filter_map(|op| match op {
            Asm::Op(opcode) => Some(format!("{:?}", opcode)),
            _ => None,
        })
        .collect()
}

/// Verifies opcodes appear in order, allowing other instructions between them
pub fn assert_opcode_sequence(asm: &[Asm], expected: &[&str]) {
    let opcodes = extract_opcode_sequence(asm);
    let mut expected_idx = 0;
    let mut found_indices = Vec::new();

    for (i, opcode) in opcodes.iter().enumerate() {
        if expected_idx < expected.len() && opcode == expected[expected_idx] {
            found_indices.push(i);
            expected_idx += 1;
        }
    }

    if expected_idx != expected.len() {
        let mut msg = String::from("Expected opcode sequence not found\n");
        msg.push_str("Expected sequence:\n");
        for (i, op) in expected.iter().enumerate() {
            let status = if i < expected_idx { "✓" } else { "✗" };
            msg.push_str(&format!("  {} [{}] {}\n", status, i, op));
        }
        msg.push_str("\nActual opcodes:\n");
        for (i, op) in opcodes.iter().take(30).enumerate() {
            let marker = if found_indices.contains(&i) { " <--" } else { "" };
            msg.push_str(&format!("  [{}] {}{}\n", i, op, marker));
        }
        if opcodes.len() > 30 {
            msg.push_str(&format!("  ... ({} more opcodes)\n", opcodes.len() - 30));
        }
        panic!("{}", msg);
    }
}

pub fn ir_to_bytecode(program: EthIRProgram) -> Vec<u8> {
    TestProgram::from_program(program)
        .into_bytecode()
        .expect("Failed to compile IR program to bytecode")
}

/// Execute blocks and extract result
pub fn execute_blocks_and_extract(
    blocks: Vec<(Vec<Operation>, Control)>,
    entry_block: usize,
) -> U256 {
    let program = create_branching_program(blocks, entry_block);
    let bytecode = ir_to_bytecode(program);
    execute_and_get_result(bytecode).expect("Failed to execute blocks and extract result")
}

/// Find the last operation that produces a result and return its local ID
fn find_result_local(operations: &[Operation]) -> Option<u32> {
    use eth_ir_data::operation::*;

    operations.iter().rev().find_map(|op| match op {
        // Extract result from operations that produce a value
        Operation::Add(TwoInOneOut { result, .. })
        | Operation::Sub(TwoInOneOut { result, .. })
        | Operation::Mul(TwoInOneOut { result, .. })
        | Operation::Div(TwoInOneOut { result, .. })
        | Operation::Mod(TwoInOneOut { result, .. })
        | Operation::Exp(TwoInOneOut { result, .. })
        | Operation::SignExtend(TwoInOneOut { result, .. })
        | Operation::SDiv(TwoInOneOut { result, .. })
        | Operation::SMod(TwoInOneOut { result, .. })
        | Operation::Lt(TwoInOneOut { result, .. })
        | Operation::Gt(TwoInOneOut { result, .. })
        | Operation::SLt(TwoInOneOut { result, .. })
        | Operation::SGt(TwoInOneOut { result, .. })
        | Operation::Eq(TwoInOneOut { result, .. })
        | Operation::And(TwoInOneOut { result, .. })
        | Operation::Or(TwoInOneOut { result, .. })
        | Operation::Xor(TwoInOneOut { result, .. })
        | Operation::Shl(TwoInOneOut { result, .. })
        | Operation::Shr(TwoInOneOut { result, .. })
        | Operation::Sar(TwoInOneOut { result, .. })
        | Operation::Byte(TwoInOneOut { result, .. })
        | Operation::Keccak256(TwoInOneOut { result, .. }) => Some(result.get()),

        Operation::AddMod(LargeInOneOut { result, .. })
        | Operation::MulMod(LargeInOneOut { result, .. }) => Some(result.get()),

        Operation::IsZero(OneInOneOut { result, .. })
        | Operation::Not(OneInOneOut { result, .. })
        | Operation::Balance(OneInOneOut { result, .. })
        | Operation::ExtCodeSize(OneInOneOut { result, .. })
        | Operation::ExtCodeHash(OneInOneOut { result, .. })
        | Operation::SLoad(OneInOneOut { result, .. })
        | Operation::TLoad(OneInOneOut { result, .. })
        | Operation::CallDataLoad(OneInOneOut { result, .. })
        | Operation::BlobHash(OneInOneOut { result, .. }) => Some(result.get()),

        Operation::MemoryLoad(MemoryLoad { result, .. }) => Some(result.get()),

        Operation::Address(ZeroInOneOut { result })
        | Operation::Origin(ZeroInOneOut { result })
        | Operation::Caller(ZeroInOneOut { result })
        | Operation::CallValue(ZeroInOneOut { result })
        | Operation::CallDataSize(ZeroInOneOut { result })
        | Operation::GasPrice(ZeroInOneOut { result })
        | Operation::ReturnDataSize(ZeroInOneOut { result })
        | Operation::SelfBalance(ZeroInOneOut { result })
        | Operation::Number(ZeroInOneOut { result })
        | Operation::Difficulty(ZeroInOneOut { result })
        | Operation::GasLimit(ZeroInOneOut { result })
        | Operation::ChainId(ZeroInOneOut { result })
        | Operation::Coinbase(ZeroInOneOut { result })
        | Operation::Timestamp(ZeroInOneOut { result })
        | Operation::BaseFee(ZeroInOneOut { result })
        | Operation::BlobBaseFee(ZeroInOneOut { result })
        | Operation::Gas(ZeroInOneOut { result })
        | Operation::AcquireFreePointer(ZeroInOneOut { result }) => Some(result.get()),

        _ => None,
    })
}
/// Helper to create a sequence of LocalSetSmallConst operations
pub fn set_locals(values: &[(u32, u64)]) -> Vec<Operation> {
    values
        .iter()
        .map(|&(local_id, value)| {
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(local_id), value })
        })
        .collect()
}

/// Helper to create multiple zero-valued locals
pub fn set_zero_locals(start_id: u32, count: u32) -> Vec<Operation> {
    (start_id..start_id + count)
        .map(|id| {
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(id), value: 0 })
        })
        .collect()
}

/// Helper to create a memory store operation with less boilerplate
pub fn memory_store(address_local: u32, value_local: u32, byte_size: u8) -> Operation {
    Operation::MemoryStore(MemoryStore {
        address: LocalId::new(address_local),
        value: LocalId::new(value_local),
        byte_size,
    })
}

/// Helper to create a memory load operation with less boilerplate
pub fn memory_load(result_local: u32, address_local: u32, byte_size: u8) -> Operation {
    Operation::MemoryLoad(MemoryLoad {
        result: LocalId::new(result_local),
        address: LocalId::new(address_local),
        byte_size,
    })
}

/// Helper to add return operations to an operations vector
pub fn with_return(mut ops: Vec<Operation>, result_local: u32) -> Vec<Operation> {
    ops.extend(create_return_for_local(result_local, result_local + 1, result_local + 2));
    ops
}

/// Helper that combines the common pattern: create_simple_program -> ir_to_bytecode ->
/// execute_and_get_result
pub fn execute_operations(operations: Vec<Operation>) -> Result<U256, String> {
    let program = create_simple_program(operations);
    let bytecode = ir_to_bytecode(program);
    execute_and_get_result(bytecode)
}

/// Helper that adds return and then executes operations
pub fn execute_operations_with_return(
    operations: Vec<Operation>,
    result_local: u32,
) -> Result<U256, String> {
    let ops_with_return = with_return(operations, result_local);
    execute_operations(ops_with_return)
}
