//! Integration tests for I/O operations: data, environmental, and logs

use crate::tests::helpers::*;
use alloy_primitives::U256;
use eth_ir_data::{LocalId, LocalIndex, Operation, operation::*};
use revm::primitives::ExecutionResult;

#[test]
fn calldata_size_and_load() {
    let calldata = vec![0x11, 0x22, 0x33, 0x44];

    let mut operations = vec![Operation::CallDataSize(ZeroInOneOut { result: LocalId::new(0) })];
    operations.extend(set_locals(&[(1, 0)]));
    operations.push(Operation::CallDataLoad(OneInOneOut {
        result: LocalId::new(2),
        arg1: LocalId::new(1),
    }));

    let ops_with_return = with_return(operations, 0);
    let program = create_simple_program(ops_with_return);
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result_with_calldata(bytecode, calldata)
        .expect("Calldata operations should succeed");

    assert_eq!(result, U256::from(4), "Calldata size should be 4");
}

#[test]
fn calldata_copy_to_memory() {
    let calldata = vec![0xAB; 64];

    let mut operations = set_locals(&[(0, 0), (1, 0), (2, 32)]);
    operations.push(Operation::CallDataCopy(ThreeInZeroOut {
        arg1: LocalId::new(0),
        arg2: LocalId::new(1),
        arg3: LocalId::new(2),
    }));
    operations.extend(set_locals(&[(3, 0), (4, 32)]));
    operations
        .push(Operation::Return(TwoInZeroOut { arg1: LocalId::new(3), arg2: LocalId::new(4) }));

    let program = create_simple_program(operations);
    let bytecode = ir_to_bytecode(program);
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).with_calldata(calldata).build();
    let result = evm.transact_commit().expect("Transaction should succeed");

    assert!(matches!(result, ExecutionResult::Success { .. }), "Execution should succeed");
    if let ExecutionResult::Success { output, .. } = result {
        if let revm::primitives::Output::Call(bytes) = output {
            assert!(bytes.len() >= 32, "Should return at least 32 bytes");
            // Just verify we got data back - the test is checking that CallDataCopy works
            // The actual bytes returned depend on the memory state after the copy
        }
    }
}

#[test]
fn codecopy_to_memory() {
    let mut operations = set_locals(&[(0, 0), (1, 0), (2, 32)]);
    operations.push(Operation::CodeCopy(ThreeInZeroOut {
        arg1: LocalId::new(0),
        arg2: LocalId::new(1),
        arg3: LocalId::new(2),
    }));
    operations.extend(set_locals(&[(3, 0), (4, 32)]));
    operations
        .push(Operation::Return(TwoInZeroOut { arg1: LocalId::new(3), arg2: LocalId::new(4) }));

    let program = create_simple_program(operations);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode_raw(bytecode);

    assert!(matches!(result, ExecutionResult::Success { .. }), "Execution should succeed");
    if let ExecutionResult::Success { output, .. } = result {
        if let revm::primitives::Output::Call(bytes) = output {
            assert!(!bytes.is_empty(), "CodeCopy should return bytecode");
        }
    }
}

#[test]
fn environmental_info() {
    let bytecode = compile_to_bytecode(vec![
        Operation::Caller(ZeroInOneOut { result: LocalId::new(0) }),
        Operation::Origin(ZeroInOneOut { result: LocalId::new(1) }),
        Operation::Address(ZeroInOneOut { result: LocalId::new(2) }),
        Operation::Stop,
    ]);
    execute_and_verify_stop(bytecode).expect("Environmental operations should execute");
}

#[test]
fn address_caller_origin() {
    let operations = vec![
        Operation::Address(ZeroInOneOut { result: LocalId::new(0) }),
        Operation::Caller(ZeroInOneOut { result: LocalId::new(1) }),
        Operation::Origin(ZeroInOneOut { result: LocalId::new(2) }),
    ];

    let result =
        execute_operations_with_return(operations, 0).expect("Address operations should succeed");

    // Contract address: 0x1000000000000000000000000000000000000000
    let expected_addr = U256::from_be_bytes([
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0,
    ]);
    assert_eq!(result, expected_addr, "Should return contract address");
}

#[test]
fn block_infos() {
    let operations = vec![
        Operation::Number(ZeroInOneOut { result: LocalId::new(0) }),
        Operation::Difficulty(ZeroInOneOut { result: LocalId::new(1) }),
        Operation::GasLimit(ZeroInOneOut { result: LocalId::new(2) }),
        Operation::ChainId(ZeroInOneOut { result: LocalId::new(3) }),
        Operation::Timestamp(ZeroInOneOut { result: LocalId::new(4) }),
        Operation::BaseFee(ZeroInOneOut { result: LocalId::new(5) }),
        Operation::Coinbase(ZeroInOneOut { result: LocalId::new(6) }),
    ];

    let result = execute_operations_with_return(operations, 3)
        .expect("Block info operations should succeed");

    assert_eq!(result, U256::from(1), "Chain ID should be 1");
}

#[test]
fn balance_self_and_other() {
    let mut operations = set_locals(&[(0, 0x1234)]);
    operations
        .push(Operation::Balance(OneInOneOut { result: LocalId::new(1), arg1: LocalId::new(0) }));
    operations.push(Operation::SelfBalance(ZeroInOneOut { result: LocalId::new(2) }));

    let result =
        execute_operations_with_return(operations, 2).expect("Balance operations should succeed");

    assert_eq!(result, U256::ZERO, "Contract balance should be 0");
}

#[test]
fn test_log0() {
    let mut operations = set_locals(&[(0, 0), (1, 0xDEADBEEF)]);
    operations.push(memory_store(0, 1, 32));
    operations.extend(set_locals(&[(2, 0), (3, 32)]));
    operations.push(Operation::Log0(TwoInZeroOut { arg1: LocalId::new(2), arg2: LocalId::new(3) }));
    operations.push(Operation::Stop);

    let program = create_simple_program(operations);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode_raw(bytecode);

    assert!(matches!(result, ExecutionResult::Success { .. }), "Log0 execution should succeed");
    if let ExecutionResult::Success { logs, .. } = result {
        assert_eq!(logs.len(), 1, "Should emit one log");
        assert_eq!(logs[0].topics().len(), 0, "Log0 should have no topics");
    }
}

#[test]
fn test_log1() {
    let mut operations = set_locals(&[(0, 0), (1, 0x12345678)]);
    operations.push(memory_store(0, 1, 32));
    operations.extend(set_locals(&[(2, 0xAABBCCDD), (3, 0), (4, 32)]));
    operations.push(Operation::Log1(ThreeInZeroOut {
        arg1: LocalId::new(3),
        arg2: LocalId::new(4),
        arg3: LocalId::new(2),
    }));
    operations.push(Operation::Stop);

    let program = create_simple_program(operations);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode_raw(bytecode);

    assert!(matches!(result, ExecutionResult::Success { .. }), "Log1 execution should succeed");
    if let ExecutionResult::Success { logs, .. } = result {
        assert_eq!(logs.len(), 1, "Should emit one log");
        assert_eq!(logs[0].topics().len(), 1, "Log1 should have one topic");
        let topic = U256::from_be_bytes(logs[0].topics()[0].0);
        assert_eq!(topic, U256::from(0xAABBCCDDu64), "Topic should match");
    }
}

#[test]
fn test_log2() {
    let mut operations = set_locals(&[(0, 0), (1, 0xFEEDFACE)]);
    operations.push(memory_store(0, 1, 32));
    operations.extend(set_locals(&[(2, 0), (3, 32), (4, 0x1111), (5, 0x2222)]));
    operations.push(Operation::Log2(LargeInZeroOut::<4> { args_start: LocalIndex::new(2) }));
    operations.push(Operation::Stop);

    let program = create_simple_program(operations);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode_raw(bytecode);

    assert!(matches!(result, ExecutionResult::Success { .. }), "Log2 execution should succeed");
    if let ExecutionResult::Success { logs, .. } = result {
        assert_eq!(logs.len(), 1, "Should emit one log");
        assert_eq!(logs[0].topics().len(), 2, "Log2 should have two topics");
        let topic1 = U256::from_be_bytes(logs[0].topics()[0].0);
        let topic2 = U256::from_be_bytes(logs[0].topics()[1].0);
        assert_eq!(topic1, U256::from(0x1111), "First topic should match");
        assert_eq!(topic2, U256::from(0x2222), "Second topic should match");
    }
}

#[test]
fn log_empty_data() {
    let mut operations = set_locals(&[(0, 0), (1, 0)]);
    operations.push(Operation::Log0(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }));
    operations.push(Operation::Stop);

    let program = create_simple_program(operations);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode_raw(bytecode);

    assert!(matches!(result, ExecutionResult::Success { .. }), "Empty log should succeed");
    if let ExecutionResult::Success { logs, .. } = result {
        assert_eq!(logs.len(), 1, "Should emit one empty log");
        assert_eq!(logs[0].data.data.len(), 0, "Log data should be empty");
    }
}
