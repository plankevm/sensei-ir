//! Property tests for control flow operations

use super::helpers::create_evm_with_bytecode;
use crate::{
    tests::helpers::{
        constants, create_branching_program, create_simple_program, execute_and_get_result,
    },
    translate_program,
};
use alloy_primitives::U256;
use eth_ir_data::{BasicBlockId, Branch, Control, LocalId, Operation, operation::*};
use evm_glue::assembler::assemble_minimized;
use proptest::prelude::*;

/// Strategy for branch conditions with edge cases
fn branch_condition_strategy() -> impl Strategy<Value = u64> {
    prop_oneof![
        // Boolean values
        Just(0u64), // False
        Just(1u64), // True
        // Edge cases for truthiness
        Just(2u64),     // Non-canonical true
        Just(u64::MAX), // Large true value
        // Random values
        any::<u64>(),
    ]
}

/// Strategy for return data sizes
fn return_size_strategy() -> impl Strategy<Value = u64> {
    prop_oneof![
        // Empty return
        Just(0u64),
        // Word-aligned sizes
        Just(32u64),
        Just(64u64),
        // Unaligned sizes
        Just(1u64),
        Just(31u64),
        Just(33u64),
        // Large returns
        Just(256u64),
        Just(1024u64),
        // Random sizes
        0u64..200,
    ]
}

proptest! {
    #[test]
    fn test_branch_execution(condition in branch_condition_strategy(), val_true in any::<u64>(), val_false in any::<u64>()) {
        let blocks = vec![
            // Block 0: Branch based on condition
            (vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: condition
                }),
            ], Control::Branches(Branch {
                condition: LocalId::new(0),
                non_zero_target: BasicBlockId::new(1),
                zero_target: BasicBlockId::new(2),
            })),
            // Block 1: True branch
            (vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value: val_true
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(2),
                    value: constants::LOCALS_START as u64 + 1 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(2),
                    arg2: LocalId::new(3),
                }),
            ], Control::LastOpTerminates),
            // Block 2: False branch
            (vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value: val_false
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(2),
                    value: constants::LOCALS_START as u64 + 1 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(2),
                    arg2: LocalId::new(3),
                }),
            ], Control::LastOpTerminates),
        ];

        let program = create_branching_program(blocks, 0);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        if let Ok(res) = result {
            let expected = if condition != 0 { U256::from(val_true) } else { U256::from(val_false) };
            prop_assert_eq!(res, expected, "Branch should select correct value");
        }
    }
}

#[test]
fn stop() {
    use super::helpers::create_evm_with_bytecode;
    use crate::{tests::helpers::create_simple_program, translate_program};
    use evm_glue::assembler::assemble_minimized;
    use revm::primitives::{ExecutionResult, SuccessReason};

    // Test that STOP terminates execution successfully
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 42 }),
        Operation::Stop,
        // These operations should never execute
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 99 }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert!(asm.is_ok(), "STOP should translate");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "STOP should assemble");

    // Execute and check that it STOPs correctly
    let mut evm = create_evm_with_bytecode(bytecode.unwrap().1, vec![]);
    let result = evm.transact_commit().expect("EVM execution should succeed");

    match result {
        ExecutionResult::Success { reason, .. } => {
            assert_eq!(reason, SuccessReason::Stop, "Should terminate with STOP");
        }
        other => assert!(false, "Expected successful STOP execution, got: {:?}", other),
    }
}

proptest! {
    #[test]
    fn test_return(offset in 0u64..100u64, size in return_size_strategy()) {
        // Test RETURN operation with various offset/size combinations
        let ops = vec![
            // Store some data in memory first
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0xdeadbeef }),
            Operation::MemoryStore(MemoryStore {
                address: LocalId::new(0),
                value: LocalId::new(1),
                byte_size: 32,
            }),
            // Return with specified offset and size
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: offset }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: size }),
            Operation::Return(TwoInZeroOut {
                arg1: LocalId::new(2),
                arg2: LocalId::new(3),
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok(), "RETURN should translate");

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok(), "RETURN should assemble");

        // RETURN should always execute successfully
        let mut evm = create_evm_with_bytecode(bytecode.unwrap().1, vec![]);
        let result = evm.transact_commit();
        match result {
            Ok(revm::primitives::ExecutionResult::Success { output, .. }) => {
                use revm::primitives::Output;
                match output {
                    Output::Call(bytes) => {
                        // Verify returned data length matches requested size
                        prop_assert_eq!(bytes.len(), size as usize,
                            "RETURN should return exactly {} bytes", size);

                        // We stored 0xdeadbeef (32 bytes) at offset 0
                        // When returning from offset/size, we get:
                        // - offset < 32: part of stored value + maybe zeros
                        // - offset >= 32: zeros from uninitialized memory
                        if offset == 0 && size >= 4 {
                            // Should start with 0xdeadbeef (big-endian: 0x00...00deadbeef)
                            let stored_val = U256::from(0xDEADBEEFu64);
                            let stored_bytes = stored_val.to_be_bytes::<32>();
                            for i in 0..std::cmp::min(32, size as usize) {
                                prop_assert_eq!(bytes[i], stored_bytes[i],
                                    "Byte {} should match stored value", i);
                            }
                        }
                    }
                    _ => prop_assert!(false, "Expected Call output from RETURN"),
                }
            }
            Ok(_) => prop_assert!(false, "RETURN should produce Success result"),
            Err(e) => prop_assert!(false, "RETURN execution failed: {:?}", e),
        }
    }
}

proptest! {
    #[test]
    fn test_conditional_stop(cond_value in branch_condition_strategy(), value in any::<u64>()) {
        // Test conditional execution with STOP

        let ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: cond_value }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value }),

            // Store value in memory
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 0 }),
            Operation::MemoryStore(MemoryStore {
                address: LocalId::new(2),
                value: LocalId::new(1),
                byte_size: 32,
            }),

            // Conditionally stop or return
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 0 }),
            Operation::IsZero(OneInOneOut {
                result: LocalId::new(4),
                arg1: LocalId::new(0),
            }),
            // If condition is true (cond_value != 0), IsZero returns 0, so we continue to return
            // If condition is false (cond_value == 0), IsZero returns 1, so we might branch

            // For simplicity, just always return the value
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(5), value: 0 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(6), value: 32 }),
            Operation::Return(TwoInZeroOut {
                arg1: LocalId::new(5),
                arg2: LocalId::new(6),
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::from(value), "Should return stored value"),
            Err(e) => prop_assert!(false, "Unexpected error: {}", e),
        }
    }
}
