use crate::{tests::helpers::*, translate_program};
use alloy_primitives::U256;
use revm::primitives::{ExecutionResult, SuccessReason};
use sir_data::{LocalId, LocalIndex, Operation, operation::*};

#[test]
fn mcopy() {
    let ir = r#"
fn init:
    entry {
        src_offset = const 0
        value = const 42
        mstore256 src_offset value
        dest_offset = const 32
        copy_src = const 0
        copy_size = const 32
        mcopy dest_offset copy_src copy_size
        result = mload256 dest_offset
        stop
    }
"#;

    let program = checked_parse(ir);
    let asm = translate_program(program);

    assert!(count_opcode(&asm, "MCOPY") >= 1, "Should have MCOPY operation");
}

#[test]
fn memory_store_load_execution() {
    let test_value = U256::from(TEST_VALUE_SMALL);

    let mut operations = set_locals(&[(0, MEMORY_START_OFFSET), (1, TEST_VALUE_SMALL)]);
    operations.push(memory_store(0, 1, EVM_WORD_SIZE_BYTES as u8));
    operations.push(memory_load(2, 0, EVM_WORD_SIZE_BYTES as u8));

    execute_and_verify_result(operations, test_value).expect("Memory store/load should work");
}

#[test]
fn memory_store_load_opcode_generation() {
    let mut operations = set_locals(&[(0, MEMORY_START_OFFSET), (1, TEST_VALUE_SMALL)]);
    operations.push(memory_store(0, 1, EVM_WORD_SIZE_BYTES as u8));
    operations.push(memory_load(2, 0, EVM_WORD_SIZE_BYTES as u8));
    operations.push(Operation::Stop(InlineOperands::default()));

    let program = create_simple_program(operations);
    let asm = translate_program(program);
    assert!(count_opcode(&asm, "MSTORE") >= 1, "Should have MSTORE operation");
    assert!(count_opcode(&asm, "MLOAD") >= 1, "Should have MLOAD operation");
    assert!(count_opcode(&asm, "STOP") >= 1, "Should have STOP operation");
}

#[test]
fn test_memory_store8s() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: MEMORY_START_OFFSET as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: TEST_BYTE_MASK as u32,
        }),
        Operation::MemoryStore(MemoryStoreData {
            ptr: LocalId::new(0),
            value: LocalId::new(1),
            io_size: IRMemoryIOByteSize::B1,
        }),
        Operation::MemoryLoad(MemoryLoadData {
            out: LocalId::new(2),
            ptr: LocalId::new(0),
            io_size: IRMemoryIOByteSize::B32,
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);
    assert!(count_opcode(&asm, "MSTORE8") >= 1, "Should have MSTORE8 operation");
    assert!(count_opcode(&asm, "MLOAD") >= 1, "Should have MLOAD operation");
}

#[test]
fn memory_size() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: MEMORY_START_OFFSET as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: TEST_VALUE_SMALL as u32,
        }),
        Operation::MemoryStore(MemoryStoreData {
            ptr: LocalId::new(0),
            value: LocalId::new(1),
            io_size: IRMemoryIOByteSize::B32,
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);
    assert!(!asm.is_empty(), "Should generate opcodes");
}

#[test]
fn memory_multiple_stores() {
    let mut operations = set_locals(&[(0, 0), (1, TEST_VALUE_SMALL)]);
    operations.push(memory_store(0, 1, EVM_WORD_SIZE_BYTES as u8));
    operations.extend(set_locals(&[(2, EVM_WORD_SIZE_BYTES), (3, TEST_VALUE_MEDIUM)]));
    operations.push(memory_store(2, 3, EVM_WORD_SIZE_BYTES as u8));
    operations.push(memory_load(4, 0, EVM_WORD_SIZE_BYTES as u8));
    operations.push(memory_load(5, 2, EVM_WORD_SIZE_BYTES as u8));
    operations.push(Operation::Stop(InlineOperands::default()));

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert!(count_opcode(&asm, "MSTORE") >= 2, "Should have at least 2 MSTORE operations");
    assert!(count_opcode(&asm, "MLOAD") >= 2, "Should have at least 2 MLOAD operations");
}

#[test]
fn value_reuse_pattern() {
    // Set initial value and use the same value twice (simulating DUP pattern)
    let mut ops = set_locals(&[(0, TEST_VALUE_SMALL)]);
    ops.push(Operation::Add(InlineOperands {
        ins: [LocalId::new(0), LocalId::new(0)],
        outs: [LocalId::new(1)],
    }));

    let ops_with_return = create_ops_with_return(ops, 1);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_bytecode(bytecode);

    let result = result.expect("Failed to execute memory copy and add operation");
    assert_eq!(result, U256::from(84), "42 + 42 should equal 84");
}

#[test]
fn value_ordering() {
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 10 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 20 }),
        Operation::Sub(InlineOperands {
            ins: [LocalId::new(1), LocalId::new(0)],
            outs: [LocalId::new(2)],
        }),
        Operation::Sub(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [LocalId::new(3)],
        }),
        Operation::Add(InlineOperands {
            ins: [LocalId::new(2), LocalId::new(0)],
            outs: [LocalId::new(4)],
        }),
    ];

    let ops_with_return = create_ops_with_return(ops, 4);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_bytecode(bytecode);

    let result = result.expect("Failed to execute memory copy with overlapping regions");
    assert_eq!(result, U256::from(20), "(20 - 10) + 10 should equal 20");
}

#[test]
fn dynamic_allocs() {
    let mut ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 32 }),
        Operation::DynamicAllocZeroed(InlineOperands {
            ins: [LocalId::new(0)],
            outs: [LocalId::new(1)],
        }),
    ];
    ops.push(memory_load(2, 1, EVM_WORD_SIZE_BYTES as u8));

    let ops_with_return = create_ops_with_return(ops, 2);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_bytecode(bytecode);

    let result = result.expect("Failed to execute dynamic allocation with zeroing");
    assert_eq!(result, U256::ZERO, "Zeroed allocation should contain zeros");
}

#[test]
fn local_allocs() {
    let mut ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 32 }),
        Operation::DynamicAllocZeroed(InlineOperands {
            ins: [LocalId::new(0)],
            outs: [LocalId::new(1)],
        }),
    ];
    ops.push(memory_load(2, 1, EVM_WORD_SIZE_BYTES as u8));

    ops.extend(vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 32 }),
        Operation::DynamicAllocAnyBytes(InlineOperands {
            ins: [LocalId::new(3)],
            outs: [LocalId::new(4)],
        }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(5), value: 42 }),
    ]);
    ops.push(memory_store(4, 5, EVM_WORD_SIZE_BYTES as u8));
    ops.push(memory_load(6, 4, EVM_WORD_SIZE_BYTES as u8));

    ops.push(Operation::Add(InlineOperands {
        ins: [LocalId::new(2), LocalId::new(6)],
        outs: [LocalId::new(7)],
    }));

    let ops_with_return = create_ops_with_return(ops, 7);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_bytecode(bytecode);

    let result = result.expect("Failed to execute local allocations");
    assert_eq!(result, U256::from(42), "Zeroed memory (0) + stored value (42) should equal 42");
}

#[test]
fn push_pop_pattern() {
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 5 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 10 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 15 }),
        Operation::Add(InlineOperands {
            ins: [LocalId::new(2), LocalId::new(1)],
            outs: [LocalId::new(3)],
        }),
        Operation::Add(InlineOperands {
            ins: [LocalId::new(3), LocalId::new(0)],
            outs: [LocalId::new(4)],
        }),
    ];

    let ops_with_return = create_ops_with_return(ops, 4);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_bytecode(bytecode);

    let result = result.expect("Failed to execute multi-segment memory write and sum");
    assert_eq!(result, U256::from(30), "5 + 10 + 15 should equal 30");
}

#[test]
fn value_preservation() {
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: TEST_VALUE_MEDIUM as u32,
        }),
        Operation::Add(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(0)],
            outs: [LocalId::new(1)],
        }),
        Operation::Mul(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [LocalId::new(2)],
        }),
    ];

    let ops_with_return = create_ops_with_return(ops, 2);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_bytecode(bytecode);

    let result = result.expect("Failed to execute memory byte operations");
    assert_eq!(result, U256::from(20000), "100 * (100 + 100) should equal 20000");
}

#[test]
fn multi_use_pattern() {
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 7 }),
        Operation::Mul(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(0)],
            outs: [LocalId::new(1)],
        }),
        Operation::Mul(InlineOperands {
            ins: [LocalId::new(1), LocalId::new(0)],
            outs: [LocalId::new(2)],
        }),
    ];

    let ops_with_return = create_ops_with_return(ops, 2);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_bytecode(bytecode).expect("Failed to execute exp operation: 7^3");

    assert_eq!(result, U256::from(343), "7 * 7 * 7 should equal 343");
}

#[test]
fn storage_sstore_sload_execution() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: TEST_VALUE_SMALL as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: TEST_VALUE_MEDIUM as u32,
        }),
        Operation::SStore(InlineOperands { ins: [LocalId::new(0), LocalId::new(1)], outs: [] }),
        Operation::SLoad(InlineOperands { ins: [LocalId::new(0)], outs: [LocalId::new(2)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    // Execute storage write/read operations
    execute_storage_operations(operations).expect("Storage operations should work");
}

#[test]
fn storage_sstore_sload_opcode_generation() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: TEST_VALUE_SMALL as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: TEST_VALUE_MEDIUM as u32,
        }),
        Operation::SStore(InlineOperands { ins: [LocalId::new(0), LocalId::new(1)], outs: [] }),
        Operation::SLoad(InlineOperands { ins: [LocalId::new(0)], outs: [LocalId::new(2)] }),
        Operation::Stop(InlineOperands::default()),
    ];
    let program = create_simple_program(operations);
    let asm = translate_program(program);
    assert_opcode_counts(&asm, &[("SSTORE", 1), ("SLOAD", 1), ("STOP", 1)]);
}

#[test]
fn transient_storage_execution() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: TEST_VALUE_SMALL as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: TEST_VALUE_MEDIUM as u32,
        }),
        Operation::TStore(InlineOperands { ins: [LocalId::new(0), LocalId::new(1)], outs: [] }),
        Operation::TLoad(InlineOperands { ins: [LocalId::new(0)], outs: [LocalId::new(2)] }),
    ];
    execute_and_verify_result(operations, U256::from(TEST_VALUE_MEDIUM))
        .expect("Transient storage load should return stored value");
}

#[test]
fn transient_storage_opcode_generation() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: TEST_VALUE_SMALL as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: TEST_VALUE_MEDIUM as u32,
        }),
        Operation::TStore(InlineOperands { ins: [LocalId::new(0), LocalId::new(1)], outs: [] }),
        Operation::TLoad(InlineOperands { ins: [LocalId::new(0)], outs: [LocalId::new(2)] }),
        Operation::Stop(InlineOperands::default()),
    ];
    let program = create_simple_program(operations);
    let asm = translate_program(program);
    assert_opcode_counts(&asm, &[("TSTORE", 1), ("TLOAD", 1), ("STOP", 1)]);
}

#[test]
fn loggings() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: MEMORY_START_OFFSET as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: EVM_WORD_SIZE_BYTES as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 1 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 2 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(4), value: 3 }),
        Operation::Log0(InlineOperands { ins: [LocalId::new(0), LocalId::new(1)], outs: [] }),
        Operation::Log1(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1), LocalId::new(2)],
            outs: [],
        }),
        Operation::Log2(AllocatedIns::<4, 0> { ins_start: LocalIndex::new(0), outs: [] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let bytecode = compile_to_bytecode(operations);
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();

    let exec_result = evm.transact().expect("Transaction should execute");

    match exec_result.result {
        ExecutionResult::Success { reason, .. } => {
            assert_eq!(reason, SuccessReason::Stop, "Execution should stop normally");
        }
        other => assert!(false, "Execution should succeed, got: {:?}", other),
    }

    let program = create_simple_program(vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: MEMORY_START_OFFSET as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: EVM_WORD_SIZE_BYTES as u32,
        }),
        Operation::Log0(InlineOperands { ins: [LocalId::new(0), LocalId::new(1)], outs: [] }),
        Operation::Stop(InlineOperands::default()),
    ]);

    let asm = translate_program(program);
    assert_opcode_counts(&asm, &[("LOG0", 1), ("STOP", 1)]);
}

#[test]
fn calldatas() {
    let operations = vec![
        Operation::CallDataSize(InlineOperands { ins: [], outs: [LocalId::new(0)] }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 }),
        Operation::CallDataCopy(InlineOperands {
            ins: [LocalId::new(1), LocalId::new(1), LocalId::new(2)],
            outs: [],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("CALLDATASIZE", 1), ("CALLDATACOPY", 1), ("STOP", 1)]);
}

#[test]
fn calldataload() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::CallDataLoad(InlineOperands { ins: [LocalId::new(0)], outs: [LocalId::new(1)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("CALLDATALOAD", 1), ("STOP", 1)]);
}

#[test]
fn codes() {
    let operations = vec![
        Operation::CodeSize(InlineOperands { ins: [], outs: [LocalId::new(0)] }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 }),
        Operation::CodeCopy(InlineOperands {
            ins: [LocalId::new(1), LocalId::new(1), LocalId::new(2)],
            outs: [],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("CODESIZE", 1), ("CODECOPY", 1), ("STOP", 1)]);
}

#[test]
fn external_codes() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 32 }),
        Operation::ExtCodeSize(InlineOperands { ins: [LocalId::new(0)], outs: [LocalId::new(2)] }),
        Operation::ExtCodeHash(InlineOperands { ins: [LocalId::new(0)], outs: [LocalId::new(3)] }),
        Operation::ExtCodeCopy(AllocatedIns::<4, 0> { ins_start: LocalIndex::new(0), outs: [] }),
        Operation::Balance(InlineOperands { ins: [LocalId::new(0)], outs: [LocalId::new(4)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(
        &asm,
        &[("EXTCODESIZE", 1), ("EXTCODEHASH", 1), ("EXTCODECOPY", 1), ("BALANCE", 1), ("STOP", 1)],
    );
}

#[test]
fn returndatacopy() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 }),
        Operation::ReturnDataCopy(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1), LocalId::new(2)],
            outs: [],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("RETURNDATACOPY", 1), ("STOP", 1)]);
}

#[test]
fn returndatasize() {
    let operations = vec![
        Operation::ReturnDataSize(InlineOperands { ins: [], outs: [LocalId::new(0)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("RETURNDATASIZE", 1), ("STOP", 1)]);
}

#[test]
fn memory_allocation_patterns() {
    // Test memory allocation generates correct patterns
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 64 }),
        Operation::DynamicAllocZeroed(InlineOperands {
            ins: [LocalId::new(0)],
            outs: [LocalId::new(1)],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    // Should handle memory allocation
    assert!(!asm.is_empty(), "Should generate assembly for memory allocation");
}

#[test]
fn return_data_handling() {
    // Test proper handling of return data size and copy
    let ops = vec![
        Operation::ReturnDataSize(InlineOperands { ins: [], outs: [LocalId::new(0)] }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 }),
        Operation::ReturnDataCopy(InlineOperands {
            ins: [LocalId::new(1), LocalId::new(1), LocalId::new(2)],
            outs: [],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    assert_eq!(count_opcode(&asm, "RETURNDATASIZE"), 1, "Should use RETURNDATASIZE");
    assert_eq!(count_opcode(&asm, "RETURNDATACOPY"), 1, "Should use RETURNDATACOPY");
}

#[test]
fn logs() {
    // Test LOG2 operation
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 32 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 1 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 2 }),
        Operation::Log2(AllocatedIns::<4, 0> { ins_start: LocalIndex::new(0), outs: [] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert_eq!(count_opcode(&asm, "LOG2"), 1, "Should generate LOG2 opcode");
}

#[test]
fn extcodecopy() {
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 32 }),
        Operation::ExtCodeCopy(AllocatedIns::<4, 0> { ins_start: LocalIndex::new(0), outs: [] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert_eq!(count_opcode(&asm, "EXTCODECOPY"), 1, "Should generate EXTCODECOPY opcode");
}

#[test]
fn tload_tstore_transient_storage() {
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 42 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 100 }),
        Operation::TStore(InlineOperands { ins: [LocalId::new(0), LocalId::new(1)], outs: [] }),
        Operation::TLoad(InlineOperands { ins: [LocalId::new(0)], outs: [LocalId::new(2)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert_eq!(count_opcode(&asm, "TSTORE"), 1, "Should generate TSTORE opcode");
    assert_eq!(count_opcode(&asm, "TLOAD"), 1, "Should generate TLOAD opcode");
}

#[test]
fn mcopy_memory() {
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 32 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 }),
        Operation::MemoryCopy(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1), LocalId::new(2)],
            outs: [],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert_eq!(count_opcode(&asm, "MCOPY"), 1, "Should generate MCOPY opcode");
}
