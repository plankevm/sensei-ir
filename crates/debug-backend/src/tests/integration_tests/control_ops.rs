//! Integration tests for control flow and call operations

use crate::tests::helpers::*;
use alloy_primitives::U256;
use revm::primitives::{ExecutionResult, address};
use sir_data::{BasicBlockId, Branch, Control, LocalId, LocalIndex, Operation, operation::*};

#[test]
fn control_flow_jump() {
    let blocks = vec![
        (set_locals(&[(0, 10)]), Control::ContinuesTo(BasicBlockId::new(2))),
        (set_locals(&[(0, 20)]), Control::ContinuesTo(BasicBlockId::new(3))),
        (
            {
                let mut ops = set_locals(&[(1, 5)]);
                ops.push(Operation::Add(InlineOperands {
                    ins: [LocalId::new(0), LocalId::new(1)],
                    outs: [LocalId::new(2)],
                }));
                ops
            },
            Control::ContinuesTo(BasicBlockId::new(3)),
        ),
        (create_return_for_local(2, 3, 4), Control::LastOpTerminates),
    ];

    let result = execute_blocks_and_extract(blocks, 0);
    assert_eq!(result, U256::from(15), "10 + 5 should equal 15 after jump");
}

#[test]
fn control_flow_branch_taken() {
    let blocks = vec![
        (
            set_locals(&[(0, 1)]),
            Control::Branches(Branch {
                condition: LocalId::new(0),
                non_zero_target: BasicBlockId::new(1),
                zero_target: BasicBlockId::new(2),
            }),
        ),
        (set_locals(&[(1, 42)]), Control::LastOpTerminates),
        (set_locals(&[(1, 99)]), Control::LastOpTerminates),
    ];

    let mut blocks_with_return = blocks;
    blocks_with_return[1].0.extend(create_return_for_local(1, 2, 3));
    blocks_with_return[2].0.extend(create_return_for_local(1, 2, 3));

    let result = execute_blocks_and_extract(blocks_with_return, 0);
    assert_eq!(result, U256::from(42), "True branch should return 42");
}

#[test]
fn termination_stop() {
    let ir = r#"
fn init:
    entry {
        stop
    }
"#;

    let program = checked_parse(ir);
    let bytecode = ir_to_bytecode(program);
    execute_and_verify_stop(bytecode).expect("Stop should execute successfully");
}

#[test]
fn termination_return() {
    let result = OperationTestBuilder::new()
        .add_operation(|_| {
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(0),
                value: TEST_RETURN_VALUE as u32,
            })
        })
        .add_operation(|_| {
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(1),
                value: local_memory_offset(0) as u32,
            })
        })
        .add_operation(|_| {
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 })
        })
        .add_operation(|_| {
            Operation::Return(InlineOperands { ins: [LocalId::new(1), LocalId::new(2)], outs: [] })
        })
        .build_and_execute(0)
        .expect("Return should succeed");

    assert_eq!(result, U256::from(TEST_RETURN_VALUE), "Return value test failed");
}

#[test]
fn termination_revert() {
    // Test revert with empty data
    let ir = r#"
fn init:
    entry {
        offset = const 0
        size = const 0
        revert offset size
    }
"#;

    let program = checked_parse(ir);
    let bytecode = ir_to_bytecode(program);
    execute_and_verify_revert(bytecode).expect("Should revert with empty data");
}

#[test]
fn termination_revert_with_data() {
    // Test revert with specific error data
    let ir = format!(
        r#"
fn init:
    entry {{
        offset = const 0
        error_code = const {}
        mstore256 offset error_code
        revert_offset = const 0
        revert_size = const 32
        revert revert_offset revert_size
    }}
"#,
        TEST_REVERT_ERROR_CODE
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode_raw(bytecode);

    let revm::primitives::ExecutionResult::Revert { output, .. } = result else {
        assert!(false, "Expected revert with data, got: {:?}", result);
        return;
    };

    // Verify the revert data contains our error code
    let expected_error = U256::from(TEST_REVERT_ERROR_CODE);
    let buffer = expected_error.to_be_bytes::<32>();
    assert_eq!(output, buffer.to_vec(), "Revert should contain error code");
}

#[test]
fn external_call() {
    let mut operations = vec![];
    operations.extend(set_locals(&[(0, TEST_CALL_GAS), (1, TEST_CALL_ADDRESS)]));
    operations.extend(set_zero_locals(2, 4)); // Creates locals 2, 3, 4, 5 with value 0
    operations
        .push(Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(6), value: 32 }));
    operations.push(Operation::Call(AllocatedIns {
        ins_start: LocalIndex::new(0),
        outs: [LocalId::new(7)],
    }));

    let ops_with_return = with_return(operations, 7);

    let program = create_simple_program(ops_with_return);
    let bytecode = ir_to_bytecode(program);

    // Create EVM with a mock contract at address 0x2000
    let mut db = revm::InMemoryDB::default();
    let target_addr = address!("0000000000000000000000000000000000002000");
    let contract_addr = address!("1000000000000000000000000000000000000000");
    let caller_addr = address!("9000000000000000000000000000000000000000");

    // Simple bytecode that returns 42
    let target_bytecode = vec![0x60, 0x2A, 0x60, 0x00, 0x52, 0x60, 0x20, 0x60, 0x00, 0xF3];

    db.insert_account_info(
        target_addr,
        revm::primitives::AccountInfo {
            balance: U256::ZERO,
            nonce: 0,
            code_hash: revm::primitives::keccak256(&target_bytecode),
            code: Some(revm::primitives::Bytecode::new_raw(target_bytecode.into())),
        },
    );

    db.insert_account_info(
        contract_addr,
        revm::primitives::AccountInfo {
            balance: U256::ZERO,
            nonce: 0,
            code_hash: revm::primitives::keccak256(&bytecode),
            code: Some(revm::primitives::Bytecode::new_raw(bytecode.clone().into())),
        },
    );

    db.insert_account_info(
        caller_addr,
        revm::primitives::AccountInfo {
            balance: U256::from(TEST_ETH_BALANCE),
            nonce: 0,
            code_hash: revm::primitives::KECCAK_EMPTY,
            code: None,
        },
    );

    let mut evm = revm::Evm::builder()
        .with_db(db)
        .modify_tx_env(|tx| {
            tx.caller = caller_addr;
            tx.transact_to = revm::primitives::TransactTo::Call(contract_addr);
            tx.data = vec![].into();
            tx.gas_limit = TEST_GAS_LIMIT;
            tx.gas_price = U256::ZERO;
            tx.value = U256::ZERO;
        })
        .build();

    let result = evm.transact_commit().expect("Transaction should succeed");

    assert!(matches!(result, ExecutionResult::Success { .. }), "Execution should succeed");
    if let ExecutionResult::Success { output, .. } = result {
        if let revm::primitives::Output::Call(bytes) = output {
            let value = U256::from_be_bytes::<32>(
                bytes[0..32].try_into().expect("Return data should be 32 bytes"),
            );
            assert_eq!(value, U256::from(1), "Call should succeed and return 1");
        }
    }
}

#[test]
fn contract_create() {
    let mut operations = set_locals(&[(0, 0), (1, 0x60F3)]);
    operations.push(memory_store(0, 1, 32));
    operations.extend(set_locals(&[(2, 0), (3, 0), (4, 5)]));
    operations.push(Operation::Create(AllocatedIns {
        ins_start: LocalIndex::new(2),
        outs: [LocalId::new(5)],
    }));

    let mut ops_with_return = operations;
    ops_with_return.extend(create_return_for_local(5, 6, 7));

    let program = create_simple_program(ops_with_return);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode_raw(bytecode);

    assert!(matches!(result, ExecutionResult::Success { .. }), "CREATE execution should complete");
}

#[test]
fn external_delegatecall() {
    let mut operations = set_locals(&[(0, 1000), (1, 0x3000), (2, 0), (3, 0), (4, 0), (5, 32)]);
    operations.push(Operation::DelegateCall(AllocatedIns {
        ins_start: LocalIndex::new(0),
        outs: [LocalId::new(6)],
    }));

    let mut ops_with_return = operations;
    ops_with_return.extend(create_return_for_local(6, 7, 8));

    let program = create_simple_program(ops_with_return);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode_raw(bytecode);

    assert!(matches!(result, ExecutionResult::Success { .. }));
}

#[test]
fn termination_selfdestruct() {
    let mut operations = set_locals(&[(0, 0x5000)]);
    operations.push(Operation::SelfDestruct(InlineOperands { ins: [LocalId::new(0)], outs: [] }));
    let bytecode = compile_to_bytecode(operations);

    // Note: SelfDestruct actually succeeds, not halts
    let result = execute_bytecode_raw(bytecode);
    assert!(
        matches!(result, ExecutionResult::Success { .. }),
        "SelfDestruct execution should succeed"
    );
    if let ExecutionResult::Success { output, .. } = result {
        if let revm::primitives::Output::Call(bytes) = output {
            assert_eq!(bytes.len(), 0, "SelfDestruct should not return data");
        }
    }
}
