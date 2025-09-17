use crate::{tests::helpers::*, translate_program};
use alloy_primitives::U256;
use eth_ir_data::{LocalId, LocalIndex, Operation, operation::*};
use revm::primitives::{ExecutionResult, SuccessReason};

#[test]
fn mcopy() {
    let mut operations = set_locals(&[(0, 0), (1, 42)]);
    operations.push(memory_store(0, 1, 32));
    operations.extend(set_locals(&[(2, 32), (3, 0), (4, 32)]));
    operations.push(Operation::MCopy(ThreeInZeroOut {
        arg1: LocalId::new(2),
        arg2: LocalId::new(3),
        arg3: LocalId::new(4),
    }));
    operations.push(memory_load(5, 2, 32));
    operations.push(Operation::Stop);

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("MCopy operation should translate");

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
    operations.push(Operation::Stop);

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Memory operations should translate");
    assert!(count_opcode(&asm, "MSTORE") >= 1, "Should have MSTORE operation");
    assert!(count_opcode(&asm, "MLOAD") >= 1, "Should have MLOAD operation");
    assert!(count_opcode(&asm, "STOP") >= 1, "Should have STOP operation");
}

#[test]
fn test_memory_store8s() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: MEMORY_START_OFFSET,
        }),
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(1),
            value: TEST_BYTE_MASK,
        }),
        Operation::MemoryStore(MemoryStore {
            address: LocalId::new(0),
            value: LocalId::new(1),
            byte_size: 1,
        }),
        Operation::MemoryLoad(MemoryLoad {
            result: LocalId::new(2),
            address: LocalId::new(0),
            byte_size: EVM_WORD_SIZE_BYTES as u8,
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Memory store8 should translate");
    assert!(count_opcode(&asm, "MSTORE8") >= 1, "Should have MSTORE8 operation");
    assert!(count_opcode(&asm, "MLOAD") >= 1, "Should have MLOAD operation");
}

#[test]
fn memory_size() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: MEMORY_START_OFFSET,
        }),
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(1),
            value: TEST_VALUE_SMALL,
        }),
        Operation::MemoryStore(MemoryStore {
            address: LocalId::new(0),
            value: LocalId::new(1),
            byte_size: EVM_WORD_SIZE_BYTES as u8,
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Memory operations should translate");
    assert!(!asm.is_empty(), "Should generate opcodes");
}

#[test]
fn memory_at_boundary() {
    let mut operations = set_locals(&[(0, TEST_MEMORY_BOUNDARY), (1, TEST_VALUE_SMALL)]);
    operations.push(memory_store(0, 1, EVM_WORD_SIZE_BYTES as u8));
    operations.push(memory_load(2, 0, EVM_WORD_SIZE_BYTES as u8));
    operations.push(Operation::Stop);

    let program = create_simple_program(operations);
    let asm = translate_program(program);
    assert!(asm.is_ok(), "Memory operations at boundary address should translate");
}

#[test]
fn memory_multiple_stores() {
    let mut operations = set_locals(&[(0, 0), (1, TEST_VALUE_SMALL)]);
    operations.push(memory_store(0, 1, EVM_WORD_SIZE_BYTES as u8));
    operations.extend(set_locals(&[(2, EVM_WORD_SIZE_BYTES), (3, TEST_VALUE_MEDIUM)]));
    operations.push(memory_store(2, 3, EVM_WORD_SIZE_BYTES as u8));
    operations.push(memory_load(4, 0, EVM_WORD_SIZE_BYTES as u8));
    operations.push(memory_load(5, 2, EVM_WORD_SIZE_BYTES as u8));
    operations.push(Operation::Stop);

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Multiple memory stores should translate");

    assert!(count_opcode(&asm, "MSTORE") >= 2, "Should have at least 2 MSTORE operations");
    assert!(count_opcode(&asm, "MLOAD") >= 2, "Should have at least 2 MLOAD operations");
}

#[test]
fn value_reuse_pattern() {
    // Set initial value and use the same value twice (simulating DUP pattern)
    let mut ops = set_locals(&[(0, TEST_VALUE_SMALL)]);
    ops.push(Operation::Add(TwoInOneOut {
        result: LocalId::new(1),
        arg1: LocalId::new(0),
        arg2: LocalId::new(0),
    }));

    let ops_with_return = create_ops_with_return(ops, 1);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_and_get_result(bytecode);

    let result = result.expect("Failed to execute memory copy and add operation");
    assert_eq!(result, U256::from(84), "42 + 42 should equal 84");
}

#[test]
fn value_ordering() {
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 10 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 20 }),
        Operation::Sub(TwoInOneOut {
            result: LocalId::new(2),
            arg1: LocalId::new(1),
            arg2: LocalId::new(0),
        }),
        Operation::Sub(TwoInOneOut {
            result: LocalId::new(3),
            arg1: LocalId::new(0),
            arg2: LocalId::new(1),
        }),
        Operation::Add(TwoInOneOut {
            result: LocalId::new(4),
            arg1: LocalId::new(2),
            arg2: LocalId::new(0),
        }),
    ];

    let ops_with_return = create_ops_with_return(ops, 4);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_and_get_result(bytecode);

    let result = result.expect("Failed to execute memory copy with overlapping regions");
    assert_eq!(result, U256::from(20), "(20 - 10) + 10 should equal 20");
}

#[test]
fn dynamic_allocs() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 32 }),
        Operation::DynamicAllocZeroed(OneInOneOut {
            result: LocalId::new(1),
            arg1: LocalId::new(0),
        }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 64 }),
        Operation::DynamicAllocAnyBytes(OneInOneOut {
            result: LocalId::new(3),
            arg1: LocalId::new(2),
        }),
        Operation::AcquireFreePointer(ZeroInOneOut { result: LocalId::new(4) }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(5), value: 128 }),
        Operation::DynamicAllocUsingFreePointer(TwoInZeroOut {
            arg1: LocalId::new(4),
            arg2: LocalId::new(5),
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Dynamic allocation operations should translate");
    assert!(!asm.is_empty(), "Should generate assembly for dynamic allocations");
    assert!(count_opcode(&asm, "STOP") >= 1, "Should have STOP operation");
}

#[test]
fn local_allocs() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 32 }),
        Operation::LocalAllocZeroed(OneInOneOut { result: LocalId::new(1), arg1: LocalId::new(0) }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 64 }),
        Operation::LocalAllocAnyBytes(OneInOneOut {
            result: LocalId::new(3),
            arg1: LocalId::new(2),
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Local allocation operations should translate");
    assert!(!asm.is_empty(), "Should generate assembly for local allocations");
}

#[test]
fn push_pop_pattern() {
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 5 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 10 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 15 }),
        Operation::Add(TwoInOneOut {
            result: LocalId::new(3),
            arg1: LocalId::new(2),
            arg2: LocalId::new(1),
        }),
        Operation::Add(TwoInOneOut {
            result: LocalId::new(4),
            arg1: LocalId::new(3),
            arg2: LocalId::new(0),
        }),
    ];

    let ops_with_return = create_ops_with_return(ops, 4);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_and_get_result(bytecode);

    let result = result.expect("Failed to execute multi-segment memory write and sum");
    assert_eq!(result, U256::from(30), "5 + 10 + 15 should equal 30");
}

#[test]
fn value_preservation() {
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: TEST_VALUE_MEDIUM,
        }),
        Operation::Add(TwoInOneOut {
            result: LocalId::new(1),
            arg1: LocalId::new(0),
            arg2: LocalId::new(0),
        }),
        Operation::Mul(TwoInOneOut {
            result: LocalId::new(2),
            arg1: LocalId::new(0),
            arg2: LocalId::new(1),
        }),
    ];

    let ops_with_return = create_ops_with_return(ops, 2);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_and_get_result(bytecode);

    let result = result.expect("Failed to execute memory byte operations");
    assert_eq!(result, U256::from(20000), "100 * (100 + 100) should equal 20000");
}

#[test]
fn multi_use_pattern() {
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 7 }),
        Operation::Mul(TwoInOneOut {
            result: LocalId::new(1),
            arg1: LocalId::new(0),
            arg2: LocalId::new(0),
        }),
        Operation::Mul(TwoInOneOut {
            result: LocalId::new(2),
            arg1: LocalId::new(1),
            arg2: LocalId::new(0),
        }),
    ];

    let ops_with_return = create_ops_with_return(ops, 2);
    let bytecode = compile_to_bytecode(ops_with_return);
    let result = execute_and_get_result(bytecode).expect("Failed to execute exp operation: 7^3");

    assert_eq!(result, U256::from(343), "7 * 7 * 7 should equal 343");
}

#[test]
fn storage_sstore_sload_execution() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: TEST_VALUE_SMALL,
        }),
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(1),
            value: TEST_VALUE_MEDIUM,
        }),
        Operation::SStore(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
        Operation::SLoad(OneInOneOut { result: LocalId::new(2), arg1: LocalId::new(0) }),
        Operation::Stop,
    ];

    // Execute storage write/read operations
    execute_storage_operations(operations).expect("Storage operations should work");
}

#[test]
fn storage_sstore_sload_opcode_generation() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: TEST_VALUE_SMALL,
        }),
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(1),
            value: TEST_VALUE_MEDIUM,
        }),
        Operation::SStore(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
        Operation::SLoad(OneInOneOut { result: LocalId::new(2), arg1: LocalId::new(0) }),
        Operation::Stop,
    ];
    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Storage operations should translate");
    assert_opcode_counts(&asm, &[("SSTORE", 1), ("SLOAD", 1), ("STOP", 1)]);
}

#[test]
fn transient_storage_execution() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: TEST_VALUE_SMALL,
        }),
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(1),
            value: TEST_VALUE_MEDIUM,
        }),
        Operation::TStore(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
        Operation::TLoad(OneInOneOut { result: LocalId::new(2), arg1: LocalId::new(0) }),
    ];
    execute_and_verify_result(operations, U256::from(TEST_VALUE_MEDIUM))
        .expect("Transient storage load should return stored value");
}

#[test]
fn transient_storage_opcode_generation() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: TEST_VALUE_SMALL,
        }),
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(1),
            value: TEST_VALUE_MEDIUM,
        }),
        Operation::TStore(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
        Operation::TLoad(OneInOneOut { result: LocalId::new(2), arg1: LocalId::new(0) }),
        Operation::Stop,
    ];
    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Transient storage operations should translate");
    assert_opcode_counts(&asm, &[("TSTORE", 1), ("TLOAD", 1), ("STOP", 1)]);
}

#[test]
fn loggings() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: MEMORY_START_OFFSET,
        }),
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(1),
            value: EVM_WORD_SIZE_BYTES,
        }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 1 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 2 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(4), value: 3 }),
        Operation::Log0(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
        Operation::Log1(ThreeInZeroOut {
            arg1: LocalId::new(0),
            arg2: LocalId::new(1),
            arg3: LocalId::new(2),
        }),
        Operation::Log2(LargeInZeroOut { args_start: LocalIndex::new(0) }),
        Operation::Stop,
    ];

    let bytecode = compile_to_bytecode(operations);
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();

    let exec_result = evm.transact().expect("Transaction should execute");

    match exec_result.result {
        ExecutionResult::Success { reason, .. } => {
            assert_eq!(reason, SuccessReason::Stop, "Execution should stop normally");
        }
        _ => panic!("Execution should succeed"),
    }

    let program = create_simple_program(vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: MEMORY_START_OFFSET,
        }),
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(1),
            value: EVM_WORD_SIZE_BYTES,
        }),
        Operation::Log0(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
        Operation::Stop,
    ]);

    let asm = translate_program(program).expect("Logging operations should translate");
    assert_opcode_counts(&asm, &[("LOG0", 1), ("STOP", 1)]);
}

#[test]
fn calldatas() {
    let operations = vec![
        Operation::CallDataSize(ZeroInOneOut { result: LocalId::new(0) }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 32 }),
        Operation::CallDataCopy(ThreeInZeroOut {
            arg1: LocalId::new(1),
            arg2: LocalId::new(1),
            arg3: LocalId::new(2),
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Calldata operations should translate");

    assert_opcode_counts(&asm, &[("CALLDATASIZE", 1), ("CALLDATACOPY", 1), ("STOP", 1)]);
}

#[test]
fn calldataload() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
        Operation::CallDataLoad(OneInOneOut { result: LocalId::new(1), arg1: LocalId::new(0) }),
        Operation::Stop,
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("CallDataLoad operation should translate");

    assert_opcode_counts(&asm, &[("CALLDATALOAD", 1), ("STOP", 1)]);
}

#[test]
fn codes() {
    let operations = vec![
        Operation::CodeSize(ZeroInOneOut { result: LocalId::new(0) }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 32 }),
        Operation::CodeCopy(ThreeInZeroOut {
            arg1: LocalId::new(1),
            arg2: LocalId::new(1),
            arg3: LocalId::new(2),
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("Code operations should translate");

    assert_opcode_counts(&asm, &[("CODESIZE", 1), ("CODECOPY", 2), ("STOP", 1)]); // 2 CODECOPY: 1 from op, 1 from deployment
}

#[test]
fn external_codes() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 32 }),
        Operation::ExtCodeSize(OneInOneOut { result: LocalId::new(2), arg1: LocalId::new(0) }),
        Operation::ExtCodeHash(OneInOneOut { result: LocalId::new(3), arg1: LocalId::new(0) }),
        Operation::ExtCodeCopy(LargeInZeroOut { args_start: LocalIndex::new(0) }),
        Operation::Balance(OneInOneOut { result: LocalId::new(4), arg1: LocalId::new(0) }),
        Operation::Stop,
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("External code operations should translate");

    assert_opcode_counts(
        &asm,
        &[("EXTCODESIZE", 1), ("EXTCODEHASH", 1), ("EXTCODECOPY", 1), ("BALANCE", 1), ("STOP", 1)],
    );
}

#[test]
fn returndatacopy() {
    let operations = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 32 }),
        Operation::ReturnDataCopy(ThreeInZeroOut {
            arg1: LocalId::new(0),
            arg2: LocalId::new(1),
            arg3: LocalId::new(2),
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("ReturnDataCopy operation should translate");

    assert_opcode_counts(&asm, &[("RETURNDATACOPY", 1), ("STOP", 1)]);
}

#[test]
fn returndatasize() {
    let operations =
        vec![Operation::ReturnDataSize(ZeroInOneOut { result: LocalId::new(0) }), Operation::Stop];

    let program = create_simple_program(operations);
    let asm = translate_program(program).expect("ReturnDataSize operation should translate");

    assert_opcode_counts(&asm, &[("RETURNDATASIZE", 1), ("STOP", 1)]);
}

#[test]
fn memory_allocation_patterns() {
    // Test memory allocation generates correct patterns
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 64 }),
        Operation::DynamicAllocZeroed(OneInOneOut {
            result: LocalId::new(1),
            arg1: LocalId::new(0),
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program).expect("Translation should succeed");

    // Should handle memory allocation
    assert!(!asm.is_empty(), "Should generate assembly for memory allocation");
}

#[test]
fn return_data_handling() {
    // Test proper handling of return data size and copy
    let ops = vec![
        Operation::ReturnDataSize(ZeroInOneOut { result: LocalId::new(0) }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 32 }),
        Operation::ReturnDataCopy(ThreeInZeroOut {
            arg1: LocalId::new(1),
            arg2: LocalId::new(1),
            arg3: LocalId::new(2),
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program).expect("Translation should succeed");

    assert_eq!(count_opcode(&asm, "RETURNDATASIZE"), 1, "Should use RETURNDATASIZE");
    assert_eq!(count_opcode(&asm, "RETURNDATACOPY"), 1, "Should use RETURNDATACOPY");
}

#[test]
fn logs() {
    // Test LOG2 operation
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 32 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 1 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 2 }),
        Operation::Log2(LargeInZeroOut { args_start: LocalIndex::new(0) }),
        Operation::Stop,
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program).expect("Translation should succeed");
    assert_eq!(count_opcode(&asm, "LOG2"), 1, "Should generate LOG2 opcode");
}

#[test]
fn extcodecopy() {
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 32 }),
        Operation::ExtCodeCopy(LargeInZeroOut { args_start: LocalIndex::new(0) }),
        Operation::Stop,
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program).expect("Translation should succeed");
    assert_eq!(count_opcode(&asm, "EXTCODECOPY"), 1, "Should generate EXTCODECOPY opcode");
}

#[test]
fn tload_tstore_transient_storage() {
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 42 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 100 }),
        Operation::TStore(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
        Operation::TLoad(OneInOneOut { result: LocalId::new(2), arg1: LocalId::new(0) }),
        Operation::Stop,
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program).expect("Translation should succeed");
    assert_eq!(count_opcode(&asm, "TSTORE"), 1, "Should generate TSTORE opcode");
    assert_eq!(count_opcode(&asm, "TLOAD"), 1, "Should generate TLOAD opcode");
}

#[test]
fn mcopy_memory() {
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 32 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 32 }),
        Operation::MCopy(ThreeInZeroOut {
            arg1: LocalId::new(0),
            arg2: LocalId::new(1),
            arg3: LocalId::new(2),
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program).expect("Translation should succeed");
    assert_eq!(count_opcode(&asm, "MCOPY"), 1, "Should generate MCOPY opcode");
}
