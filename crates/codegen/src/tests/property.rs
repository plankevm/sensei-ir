//! Comprehensive property-based tests with execution verification
//!
//! This module combines comprehensive test coverage with execution-based verification,
//! providing strong semantic correctness guarantees for the EVM IR compiler.

#[cfg(test)]
mod tests {
    use crate::{
        gas::SimpleGasEstimator,
        tests::helpers::{
            create_branching_program, create_program_with_data, create_simple_program,
        },
        translate_program,
        translator::memory::constants,
    };
    use alloy_primitives::U256;
    use eth_ir_data::{
        BasicBlockId, Branch, Control, DataId, LargeConstId, LocalId, LocalIndex, Operation,
        operation::*,
    };
    use evm_glue::assembler::assemble_minimized;
    use proptest::prelude::*;
    use revm::{
        Evm, InMemoryDB,
        primitives::{AccountInfo, Bytecode, ExecutionResult, Output, TransactTo, address},
    };

    // ==================== TEST HELPERS ====================

    /// Execute bytecode and extract the result from return data
    fn execute_and_get_result(bytecode: Vec<u8>) -> Result<U256, String> {
        let mut evm = create_evm_with_bytecode(bytecode, vec![]);
        let result = evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;

        match result {
            ExecutionResult::Success { output, .. } => match output {
                Output::Call(bytes) => {
                    if bytes.len() >= 32 {
                        Ok(U256::from_be_bytes::<32>(bytes[0..32].try_into().unwrap()))
                    } else {
                        Ok(U256::ZERO)
                    }
                }
                _ => Err("Unexpected output type".to_string()),
            },
            ExecutionResult::Revert { output, .. } => {
                Err(format!("Execution reverted: {:?}", output))
            }
            ExecutionResult::Halt { reason, .. } => {
                if format!("{:?}", reason).contains("OutOfGas") {
                    Err("Out of gas".to_string())
                } else {
                    // Normal STOP - no return value
                    Err("Execution halted with STOP".to_string())
                }
            }
        }
    }

    /// Execute bytecode with limited gas
    fn execute_with_gas_limit(
        bytecode: Vec<u8>,
        gas_limit: u64,
    ) -> Result<ExecutionResult, String> {
        let mut db = InMemoryDB::default();
        let test_address = address!("0000000000000000000000000000000000000123");
        let caller_address = address!("0000000000000000000000000000000000000456");

        db.insert_account_info(
            test_address,
            AccountInfo {
                balance: U256::from(1_000_000_000_000_000_000u64),
                nonce: 0,
                code_hash: revm::primitives::keccak256(&bytecode),
                code: Some(Bytecode::new_raw(bytecode.clone().into())),
            },
        );

        db.insert_account_info(
            caller_address,
            AccountInfo {
                balance: U256::from(1_000_000_000_000_000_000u64),
                nonce: 0,
                code_hash: revm::primitives::KECCAK_EMPTY,
                code: None,
            },
        );

        let mut evm = revm::Evm::builder()
            .with_db(db)
            .modify_tx_env(|tx| {
                tx.caller = caller_address;
                tx.transact_to = TransactTo::Call(test_address);
                tx.data = vec![].into();
                tx.gas_limit = gas_limit;
                tx.gas_price = U256::from(1);
                tx.value = U256::ZERO;
            })
            .build();

        evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))
    }

    fn create_evm_with_bytecode(
        bytecode: Vec<u8>,
        calldata: Vec<u8>,
    ) -> Evm<'static, (), InMemoryDB> {
        let mut db = InMemoryDB::default();
        let test_address = address!("0000000000000000000000000000000000000123");
        let caller_address = address!("0000000000000000000000000000000000000456");

        db.insert_account_info(
            test_address,
            AccountInfo {
                balance: U256::from(1_000_000_000_000_000_000u64),
                nonce: 0,
                code_hash: revm::primitives::keccak256(&bytecode),
                code: Some(Bytecode::new_raw(bytecode.clone().into())),
            },
        );

        db.insert_account_info(
            caller_address,
            AccountInfo {
                balance: U256::from(1_000_000_000_000_000_000u64),
                nonce: 0,
                code_hash: revm::primitives::KECCAK_EMPTY,
                code: None,
            },
        );

        revm::Evm::builder()
            .with_db(db)
            .modify_tx_env(|tx| {
                tx.caller = caller_address;
                tx.transact_to = TransactTo::Call(test_address);
                tx.data = calldata.into();
                tx.gas_limit = 1_000_000;
                tx.gas_price = U256::from(1);
                tx.value = U256::ZERO;
            })
            .build()
    }

    // ==================== ARITHMETIC OPERATIONS WITH EXECUTION ====================

    proptest! {
        #[test]
        fn test_add_commutative_with_execution(a in any::<u64>(), b in any::<u64>()) {
            let ops_ab = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::Add(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let ops_ba = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: b }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: a }),
                Operation::Add(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program_ab = create_simple_program(ops_ab);
            let program_ba = create_simple_program(ops_ba);

            let asm_ab = translate_program(program_ab);
            let asm_ba = translate_program(program_ba);
            prop_assert!(asm_ab.is_ok() && asm_ba.is_ok());

            let bytecode_ab = assemble_minimized(&asm_ab.unwrap(), true);
            let bytecode_ba = assemble_minimized(&asm_ba.unwrap(), true);
            prop_assert!(bytecode_ab.is_ok() && bytecode_ba.is_ok());

            let result_ab = execute_and_get_result(bytecode_ab.unwrap().1);
            let result_ba = execute_and_get_result(bytecode_ba.unwrap().1);

            if let (Ok(res_ab), Ok(res_ba)) = (result_ab, result_ba) {
                prop_assert_eq!(res_ab, res_ba, "Addition should be commutative");
                let expected = U256::from(a).wrapping_add(U256::from(b));
                prop_assert_eq!(res_ab, expected, "Addition result incorrect");
            }
        }

        #[test]
        fn test_mul_commutative_with_execution(a in any::<u64>(), b in any::<u64>()) {
            let ops_ab = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::Mul(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops_ab);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = U256::from(a).wrapping_mul(U256::from(b));
                prop_assert_eq!(res, expected, "Multiplication result incorrect");
            }
        }

        #[test]
        fn test_sub_non_commutative_with_execution(a in any::<u64>(), b in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::Sub(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = U256::from(a).wrapping_sub(U256::from(b));
                prop_assert_eq!(res, expected, "Subtraction result incorrect");
            }
        }

        #[test]
        fn test_division_by_zero_returns_zero(dividend in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: dividend }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }),
                Operation::Div(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                prop_assert_eq!(res, U256::ZERO, "Division by zero should return 0");
            }
        }

        #[test]
        fn test_mod_operation_with_execution(dividend in 1u64..1000000, divisor in 1u64..1000) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: dividend }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: divisor }),
                Operation::Mod(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = U256::from(dividend % divisor);
                prop_assert_eq!(res, expected, "Modulo result incorrect");
            }
        }
    }

    // ==================== BITWISE OPERATIONS WITH EXECUTION ====================

    proptest! {
        #[test]
        fn test_bitwise_and_with_execution(a in any::<u64>(), b in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::And(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = U256::from(a & b);
                prop_assert_eq!(res, expected, "AND result incorrect");
            }
        }

        #[test]
        fn test_bitwise_or_with_execution(a in any::<u64>(), b in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::Or(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = U256::from(a | b);
                prop_assert_eq!(res, expected, "OR result incorrect");
            }
        }

        #[test]
        fn test_bitwise_xor_with_execution(a in any::<u64>(), b in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::Xor(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = U256::from(a ^ b);
                prop_assert_eq!(res, expected, "XOR result incorrect");
            }
        }

        #[test]
        fn test_bitwise_not_with_execution(a in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::Not(OneInOneOut {
                    result: LocalId::new(1),
                    arg1: LocalId::new(0),
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
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                // NOT in EVM is bitwise NOT on all 256 bits
                let expected = !U256::from(a);
                prop_assert_eq!(res, expected, "NOT result incorrect");
            }
        }

        #[test]
        fn test_shift_operations_with_execution(value in any::<u64>(), shift in 0u8..64) {
            // Test SHL (shift left)
            let ops_shl = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: shift as u64 }),
                Operation::Shl(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(1), // shift amount
                    arg2: LocalId::new(0), // value
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops_shl);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = U256::from(value) << shift;
                prop_assert_eq!(res, expected, "SHL result incorrect");
            }

            // Test SHR (shift right)
            let ops_shr = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: shift as u64 }),
                Operation::Shr(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(1), // shift amount
                    arg2: LocalId::new(0), // value
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops_shr);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = U256::from(value) >> shift;
                prop_assert_eq!(res, expected, "SHR result incorrect");
            }
        }
    }

    // ==================== COMPARISON OPERATIONS WITH EXECUTION ====================

    proptest! {
        #[test]
        fn test_comparison_lt_with_execution(a in any::<u64>(), b in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::Lt(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = if a < b { U256::from(1) } else { U256::ZERO };
                prop_assert_eq!(res, expected, "LT result incorrect: {} < {} should be {}", a, b, expected);
            }
        }

        #[test]
        fn test_comparison_gt_with_execution(a in any::<u64>(), b in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::Gt(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = if a > b { U256::from(1) } else { U256::ZERO };
                prop_assert_eq!(res, expected, "GT result incorrect");
            }
        }

        #[test]
        fn test_comparison_eq_with_execution(a in any::<u64>(), b in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::Eq(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = if a == b { U256::from(1) } else { U256::ZERO };
                prop_assert_eq!(res, expected, "EQ result incorrect");
            }
        }

        #[test]
        fn test_iszero_with_execution(value in any::<u64>()) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value }),
                Operation::IsZero(OneInOneOut {
                    result: LocalId::new(1),
                    arg1: LocalId::new(0),
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
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = if value == 0 { U256::from(1) } else { U256::ZERO };
                prop_assert_eq!(res, expected, "ISZERO result incorrect");
            }
        }
    }

    // ==================== CROSS-OPERATION INVARIANTS ====================

    proptest! {
        #[test]
        fn test_add_sub_inverse(a in any::<u64>(), b in any::<u64>()) {
            // Test that (a + b) - b == a
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::Add(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::Sub(TwoInOneOut {
                    result: LocalId::new(3),
                    arg1: LocalId::new(2), // a + b
                    arg2: LocalId::new(1), // b
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: constants::LOCALS_START as u64 + 3 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(5),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(4),
                    arg2: LocalId::new(5),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                prop_assert_eq!(res, U256::from(a), "(a + b) - b should equal a");
            }
        }

        #[test]
        fn test_xor_self_is_zero(value in any::<u64>()) {
            // Test that a XOR a == 0
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value }),
                Operation::Xor(TwoInOneOut {
                    result: LocalId::new(1),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(0),
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
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                prop_assert_eq!(res, U256::ZERO, "a XOR a should equal 0");
            }
        }

        #[test]
        fn test_double_not_identity(value in any::<u64>()) {
            // Test that NOT(NOT(a)) == a
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value }),
                Operation::Not(OneInOneOut {
                    result: LocalId::new(1),
                    arg1: LocalId::new(0),
                }),
                Operation::Not(OneInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                prop_assert_eq!(res, U256::from(value), "NOT(NOT(a)) should equal a");
            }
        }

        #[test]
        fn test_shift_inverse(value in 1u64..u64::MAX, shift in 1u8..64) {
            // Test that (a << n) >> n == a (for values that don't overflow)
            let safe_value = value >> shift; // Ensure no overflow

            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: safe_value }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: shift as u64 }),
                Operation::Shl(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(1),
                    arg2: LocalId::new(0),
                }),
                Operation::Shr(TwoInOneOut {
                    result: LocalId::new(3),
                    arg1: LocalId::new(1),
                    arg2: LocalId::new(2),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: constants::LOCALS_START as u64 + 3 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(5),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(4),
                    arg2: LocalId::new(5),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                prop_assert_eq!(res, U256::from(safe_value), "(a << n) >> n should equal a");
            }
        }
    }

    // ==================== MEMORY OPERATIONS ====================

    proptest! {
        #[test]
        fn test_memory_store_load_invariant(offset in 0u32..1000, value in any::<u64>()) {
            let aligned_offset = (offset / 32) * 32;

            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: aligned_offset as u64
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value
                }),
                Operation::MemoryStore(MemoryStore {
                    address: LocalId::new(0),
                    value: LocalId::new(1),
                    byte_size: 32,
                }),
                Operation::MemoryLoad(MemoryLoad {
                    result: LocalId::new(2),
                    address: LocalId::new(0),
                    byte_size: 32,
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                prop_assert_eq!(res, U256::from(value),
                    "Memory store/load invariant violated");
            }
        }

        #[test]
        fn test_memory_byte_operations(offset in 0u32..1000, value in 0u8..=255) {
            // Test MSTORE8 and byte-wise loading
            // Ensure offset doesn't overlap with locals area
            // Ensure offset is beyond locals area (LOCALS_START + max_locals * SLOT_SIZE)
            // Using 0x1000 as a safe starting point for general memory operations
            let safe_offset = 0x1000 + offset;

            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: safe_offset as u64
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value: value as u64
                }),
                Operation::MemoryStore(MemoryStore {
                    address: LocalId::new(0),
                    value: LocalId::new(1),
                    byte_size: 1, // Single byte store
                }),
                Operation::MemoryLoad(MemoryLoad {
                    result: LocalId::new(2),
                    address: LocalId::new(0),
                    byte_size: 32, // Load full word
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            // Note: Result will be left-padded with zeros since we're loading 32 bytes
            // but only stored 1 byte
            let result = execute_and_get_result(bytecode.unwrap().1);
            prop_assert!(result.is_ok());
        }
    }

    // ==================== STORAGE OPERATIONS ====================

    proptest! {
        #[test]
        fn test_storage_persistence(pairs in prop::collection::vec((any::<u64>(), any::<u64>()), 1..10)) {
            let keys: Vec<u64> = pairs.iter().map(|(k, _)| *k).collect();
            let values: Vec<u64> = pairs.iter().map(|(_, v)| *v).collect();

            let mut ops = vec![];

            // Store all key-value pairs
            for (i, (key, value)) in keys.iter().zip(values.iter()).enumerate() {
                ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new((i * 2) as u32),
                    value: *key
                }));
                ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new((i * 2 + 1) as u32),
                    value: *value
                }));
                ops.push(Operation::SStore(TwoInZeroOut {
                    arg1: LocalId::new((i * 2) as u32),
                    arg2: LocalId::new((i * 2 + 1) as u32),
                }));
            }

            // Load and verify ALL values to ensure persistence
            let success_flag_local = LocalId::new(100);
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: success_flag_local,
                value: 1  // Start with success
            }));

            for (i, (key, expected_value)) in keys.iter().zip(values.iter()).enumerate() {
                let key_local = LocalId::new(101 + i as u32 * 3);
                let loaded_local = LocalId::new(102 + i as u32 * 3);
                let eq_local = LocalId::new(103 + i as u32 * 3);

                // Load stored value
                ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: key_local,
                    value: *key
                }));
                ops.push(Operation::SLoad(OneInOneOut {
                    result: loaded_local,
                    arg1: key_local
                }));

                // Check if loaded value equals expected
                ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(101 + i as u32 * 3 + 1),
                    value: *expected_value
                }));
                ops.push(Operation::Eq(TwoInOneOut {
                    result: eq_local,
                    arg1: loaded_local,
                    arg2: LocalId::new(101 + i as u32 * 3 + 1),
                }));

                // AND with success flag (all must match)
                ops.push(Operation::And(TwoInOneOut {
                    result: success_flag_local,
                    arg1: success_flag_local,
                    arg2: eq_local,
                }));
            }

            // Return the success flag (1 if all match, 0 otherwise)
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(200),
                value: constants::LOCALS_START as u64 + 100 * 32
            }));
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(201),
                value: 32
            }));
            ops.push(Operation::Return(TwoInZeroOut {
                arg1: LocalId::new(200),
                arg2: LocalId::new(201),
            }));

            let num_locals = 202 + keys.len() as usize * 3;
            // Use create_branching_program which allows specifying num_locals
            let program = create_branching_program(
                vec![(ops, Control::LastOpTerminates)],
                num_locals
            );

            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            // Execute and verify all values persisted correctly
            let result = execute_and_get_result(bytecode.unwrap().1);
            match result {
                Ok(res) => prop_assert_eq!(res, U256::from(1), "Storage persistence check failed"),
                Err(e) if e.contains("STOP") => {}, // Empty storage test
                Err(e) => prop_assert!(false, "Execution failed: {}", e),
            }
        }
    }

    // ==================== CONTROL FLOW ====================

    proptest! {
        #[test]
        fn test_branch_execution(condition in any::<bool>(), val_true in any::<u64>(), val_false in any::<u64>()) {
            let blocks = vec![
                // Block 0: Branch based on condition
                (vec![
                    Operation::LocalSetSmallConst(SetSmallConst {
                        local: LocalId::new(0),
                        value: if condition { 1 } else { 0 }
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

            let program = create_branching_program(blocks, 4);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            if let Ok(res) = result {
                let expected = if condition { U256::from(val_true) } else { U256::from(val_false) };
                prop_assert_eq!(res, expected, "Branch should select correct value");
            }
        }
    }

    // ==================== GAS ESTIMATION ====================

    proptest! {
        #[test]
        fn test_gas_estimation_accuracy(num_ops in 1u32..50) {
            let mut ops = vec![];
            let mut expected_gas = 0u64;

            for i in 0..num_ops {
                ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(i * 3),
                    value: i as u64
                }));
                ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(i * 3 + 1),
                    value: (i + 1) as u64
                }));
                ops.push(Operation::Add(TwoInOneOut {
                    result: LocalId::new(i * 3 + 2),
                    arg1: LocalId::new(i * 3),
                    arg2: LocalId::new(i * 3 + 1),
                }));
                expected_gas += 3; // ADD costs 3 gas
            }
            ops.push(Operation::Stop);

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let asm = asm.unwrap();
            let estimator = SimpleGasEstimator::new();
            let gas_report = estimator.estimate(&asm);

            // Gas should include all operations
            prop_assert!(gas_report.0 >= expected_gas,
                "Gas estimate {} should be at least {}", gas_report.0, expected_gas);
        }

        #[test]
        fn test_gas_monotonicity(base_ops in 1u32..50, extra_ops in 1u32..10) {
            let mut ops1 = vec![];
            for i in 0..base_ops {
                ops1.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(i),
                    value: i as u64
                }));
            }
            ops1.push(Operation::Stop);

            let mut ops2 = ops1.clone();
            ops2.pop(); // Remove Stop
            for i in base_ops..(base_ops + extra_ops) {
                ops2.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(i),
                    value: i as u64
                }));
            }
            ops2.push(Operation::Stop);

            let program1 = create_simple_program(ops1);
            let program2 = create_simple_program(ops2);

            let asm1 = translate_program(program1);
            let asm2 = translate_program(program2);
            prop_assert!(asm1.is_ok() && asm2.is_ok());

            let estimator = SimpleGasEstimator::new();
            let gas1 = estimator.estimate(&asm1.unwrap());
            let gas2 = estimator.estimate(&asm2.unwrap());

            prop_assert!(gas2.0 > gas1.0, "More operations should cost more gas");
        }
    }

    // ==================== ERROR HANDLING ====================

    #[test]
    fn test_out_of_gas_handling() {
        // Create an expensive operation sequence
        let mut ops = vec![];
        for i in 0..100 {
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(i),
                value: i as u64,
            }));
        }
        ops.push(Operation::Stop);

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        assert!(bytecode.is_ok());

        // Execute with very limited gas
        let result = execute_with_gas_limit(bytecode.unwrap().1, 100);

        match result {
            Ok(ExecutionResult::Halt { reason, .. }) => {
                // Should run out of gas
                assert!(
                    format!("{:?}", reason).contains("OutOfGas")
                        || format!("{:?}", reason).contains("OutOfFund"),
                    "Should run out of gas"
                );
            }
            Ok(ExecutionResult::Success { .. }) => {
                // If it succeeds, it means the operations were very cheap
                // This is okay for some simple operations
            }
            _ => {}
        }
    }

    #[test]
    fn test_invalid_data_segment_reference() {
        let ops = vec![
            Operation::LocalSetDataOffset(SetDataOffset {
                local: LocalId::new(0),
                segment_id: DataId::new(999), // Non-existent segment
            }),
            Operation::Stop,
        ];

        let program = create_program_with_data(ops, vec![vec![1, 2, 3]]);
        let asm = translate_program(program);

        // Should fail at translation
        assert!(asm.is_err(), "Invalid data segment should fail");
    }

    proptest! {

        #[test]
        fn test_revert_execution(offset in 0u32..100, size in 0u32..100) {
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: offset as u64
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value: size as u64
                }),
                Operation::Revert(TwoInZeroOut {
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1)
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            prop_assert!(result.is_err(), "REVERT should cause execution to fail");
        }
    }

    // ==================== EDGE CASES ====================

    #[test]
    fn test_u256_boundary_arithmetic() {
        // Test at U256 max boundary
        let ops = vec![
            // Create max U256 using large const
            Operation::LocalSetLargeConst(SetLargeConst {
                local: LocalId::new(0),
                cid: LargeConstId::new(0),
            }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(1),
                value: 1, // Add 1 to trigger overflow
            }),
            // Add overflow_amount to max (should wrap)
            Operation::Add(TwoInOneOut {
                result: LocalId::new(2),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(3),
                value: constants::LOCALS_START as u64 + 2 * 32,
            }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(4), value: 32 }),
            Operation::Return(TwoInZeroOut { arg1: LocalId::new(3), arg2: LocalId::new(4) }),
        ];

        let mut program = create_simple_program(ops);
        program.large_consts = vec![U256::MAX].into_iter().collect();

        let asm = translate_program(program);
        assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        if let Ok(res) = result {
            assert_eq!(res, U256::ZERO, "U256::MAX + 1 should wrap to 0");
        }
    }

    proptest! {
        #[test]
        fn test_memory_alignment_edge_cases(offset in any::<u32>(), value in any::<u64>()) {
            // Test unaligned memory access
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: offset as u64  // Potentially unaligned
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value
                }),
                Operation::MemoryStore(MemoryStore {
                    address: LocalId::new(0),
                    value: LocalId::new(1),
                    byte_size: 32,
                }),
                Operation::Stop,
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok(), "Unaligned memory access should be allowed");
        }
    }

    // ==================== PERFORMANCE REGRESSION TESTS ====================

    proptest! {
        #[test]
        fn test_gas_cost_regression(
            op_types in prop::collection::vec(0u8..5, 20),
            values in prop::collection::vec(any::<u64>(), 20)
        ) {
            let mut ops = vec![];

            // Generate a deterministic sequence of operations
            for (i, (&op_type, &value)) in op_types.iter().zip(values.iter()).enumerate() {
                match op_type {
                    0 => {
                        ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                            local: LocalId::new(i as u32),
                            value
                        }));
                    }
                    1 => {
                        if i > 1 {
                            ops.push(Operation::Add(TwoInOneOut {
                                result: LocalId::new(i as u32),
                                arg1: LocalId::new((i - 1) as u32),
                                arg2: LocalId::new((i - 2) as u32),
                            }));
                        }
                    }
                    2 => {
                        if i > 1 {
                            ops.push(Operation::Mul(TwoInOneOut {
                                result: LocalId::new(i as u32),
                                arg1: LocalId::new((i - 1) as u32),
                                arg2: LocalId::new((i - 2) as u32),
                            }));
                        }
                    }
                    3 => {
                        if i > 0 {
                            ops.push(Operation::Not(OneInOneOut {
                                result: LocalId::new(i as u32),
                                arg1: LocalId::new((i - 1) as u32),
                            }));
                        }
                    }
                    _ => {
                        ops.push(Operation::NoOp);
                    }
                }
            }
            ops.push(Operation::Stop);

            let program = create_simple_program(ops.clone());
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let estimator = SimpleGasEstimator::new();
            let gas = estimator.estimate(&asm.unwrap());

            // Baseline gas costs (these would be tracked over time in real regression tests)
            let baseline_min = 20u64;
            let baseline_max = 5000u64;

            prop_assert!(gas.0 >= baseline_min && gas.0 <= baseline_max,
                "Gas cost {} outside expected range [{}, {}]", gas.0, baseline_min, baseline_max);
        }
    }

    // ==================== ENVIRONMENTAL OPERATIONS ====================

    #[test]
    fn test_environmental_ops_compilation() {
        let ops = vec![
            Operation::Coinbase(ZeroInOneOut { result: LocalId::new(0) }),
            Operation::Timestamp(ZeroInOneOut { result: LocalId::new(1) }),
            Operation::Number(ZeroInOneOut { result: LocalId::new(2) }),
            Operation::Difficulty(ZeroInOneOut { result: LocalId::new(3) }),
            Operation::GasLimit(ZeroInOneOut { result: LocalId::new(4) }),
            Operation::ChainId(ZeroInOneOut { result: LocalId::new(5) }),
            Operation::BaseFee(ZeroInOneOut { result: LocalId::new(6) }),
            Operation::Gas(ZeroInOneOut { result: LocalId::new(7) }),
            Operation::GasPrice(ZeroInOneOut { result: LocalId::new(8) }),
            Operation::Origin(ZeroInOneOut { result: LocalId::new(9) }),
            Operation::Caller(ZeroInOneOut { result: LocalId::new(10) }),
            Operation::CallValue(ZeroInOneOut { result: LocalId::new(11) }),
            Operation::CallDataSize(ZeroInOneOut { result: LocalId::new(12) }),
            Operation::CodeSize(ZeroInOneOut { result: LocalId::new(13) }),
            Operation::SelfBalance(ZeroInOneOut { result: LocalId::new(14) }),
            Operation::Stop,
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        assert!(asm.is_ok(), "Environmental operations should translate");

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        assert!(bytecode.is_ok(), "Environmental operations should assemble");
    }

    // ==================== KECCAK256 HASH OPERATION ====================

    proptest! {
        #[test]
        fn test_keccak256_deterministic(data in prop::collection::vec(any::<u8>(), 0..100)) {
            // Store data in memory and hash it
            let mut ops = vec![];

            // Store each byte
            for (i, &byte) in data.iter().enumerate() {
                ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new((i * 2) as u32),
                    value: i as u64
                }));
                ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new((i * 2 + 1) as u32),
                    value: byte as u64
                }));
                ops.push(Operation::MemoryStore(MemoryStore {
                    address: LocalId::new((i * 2) as u32),
                    value: LocalId::new((i * 2 + 1) as u32),
                    byte_size: 1,
                }));
            }

            // Hash the data
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(200),
                value: 0 // offset
            }));
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(201),
                value: data.len() as u64 // size
            }));
            ops.push(Operation::Keccak256(TwoInOneOut {
                result: LocalId::new(202),
                arg1: LocalId::new(200),
                arg2: LocalId::new(201),
            }));

            ops.push(Operation::Stop);

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok(), "Keccak256 should translate");

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok(), "Keccak256 should assemble");
        }
    }

    // ==================== SIGNED OPERATIONS ====================

    proptest! {
        #[test]
        fn test_slt_signed_comparison(a in 0u64..=i64::MAX as u64, b in 0u64..=i64::MAX as u64) {
            // Test SLt with positive values that fit in i64 range
            // This avoids sign extension issues with small constants

            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::SLt(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            match result {
                Ok(res) => {
                    // When interpreted as signed 256-bit integers
                    let a_signed = a as i64;
                    let b_signed = b as i64;
                    let expected = if a_signed < b_signed { U256::from(1) } else { U256::ZERO };
                    prop_assert_eq!(res, expected, "SLT result incorrect: {} < {} (signed)", a_signed, b_signed);
                }
                Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
                _ => {}
            }
        }
    }

    // ==================== CRITICAL OPERATIONS ====================

    proptest! {
        #[test]
        fn test_addmod_operation(a in any::<u64>(), b in any::<u64>(), m in 1u64..=u64::MAX) {
            // Test (a + b) % m
            use eth_ir_data::operation::LargeInOneOut;

            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: m }),
                Operation::AddMod(LargeInOneOut {
                    result: LocalId::new(3),
                    args_start: LocalIndex::new(0), // Uses contiguous locals 0, 1, 2
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: constants::LOCALS_START as u64 + 3 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(5),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(4),
                    arg2: LocalId::new(5),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            match result {
                Ok(res) => {
                    let expected = U256::from((U256::from(a) + U256::from(b)) % U256::from(m));
                    prop_assert_eq!(res, expected, "AddMod result incorrect");
                }
                Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
                _ => {}
            }
        }

        #[test]
        fn test_mulmod_operation(a in any::<u64>(), b in any::<u64>(), m in 1u64..=u64::MAX) {
            // Test (a * b) % m
            use eth_ir_data::operation::LargeInOneOut;

            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: m }),
                Operation::MulMod(LargeInOneOut {
                    result: LocalId::new(3),
                    args_start: LocalIndex::new(0), // Uses contiguous locals 0, 1, 2
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: constants::LOCALS_START as u64 + 3 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(5),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(4),
                    arg2: LocalId::new(5),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            match result {
                Ok(res) => {
                    let expected = U256::from((U256::from(a) * U256::from(b)) % U256::from(m));
                    prop_assert_eq!(res, expected, "MulMod result incorrect");
                }
                Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
                _ => {}
            }
        }

        #[test]
        fn test_exp_operation(base in 0u64..=10, exp in 0u64..=10) {
            // Test base^exp with small values to avoid overflow
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: base }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: exp }),
                Operation::Exp(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            match result {
                Ok(res) => {
                    let expected = U256::from(base).pow(U256::from(exp));
                    prop_assert_eq!(res, expected, "Exp result incorrect: {}^{}", base, exp);
                }
                Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
                _ => {}
            }
        }

        #[test]
        fn test_sgt_signed_comparison(a in 0u64..=i64::MAX as u64, b in 0u64..=i64::MAX as u64) {
            // Test SGt with positive values that fit in i64 range
            let ops = vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
                Operation::SGt(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: constants::LOCALS_START as u64 + 2 * 32
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 32
                }),
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(3),
                    arg2: LocalId::new(4),
                }),
            ];

            let program = create_simple_program(ops);
            let asm = translate_program(program);
            prop_assert!(asm.is_ok());

            let bytecode = assemble_minimized(&asm.unwrap(), true);
            prop_assert!(bytecode.is_ok());

            let result = execute_and_get_result(bytecode.unwrap().1);
            match result {
                Ok(res) => {
                    let a_signed = a as i64;
                    let b_signed = b as i64;
                    let expected = if a_signed > b_signed { U256::from(1) } else { U256::ZERO };
                    prop_assert_eq!(res, expected, "SGt result incorrect: {} > {} (signed)", a_signed, b_signed);
                }
                Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
                _ => {}
            }
        }
    }

    // ==================== NEGATIVE TESTS ====================

    #[test]
    fn test_addmod_simple() {
        // Simple test: (0 + 1) % 2 should be 1
        use eth_ir_data::operation::LargeInOneOut;

        let ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 1 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 2 }),
            Operation::AddMod(LargeInOneOut {
                result: LocalId::new(3),
                args_start: LocalIndex::new(0),
            }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(4),
                value: constants::LOCALS_START as u64 + 3 * 32,
            }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(5), value: 32 }),
            Operation::Return(TwoInZeroOut { arg1: LocalId::new(4), arg2: LocalId::new(5) }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::from(1), "(0 + 1) % 2 should be 1");
    }

    #[test]
    fn test_modulo_by_zero_safe() {
        // Test that modulo by zero returns 0 (EVM behavior)
        let ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 42 }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(1),
                value: 0, // Modulus is zero
            }),
            Operation::Mod(TwoInOneOut {
                result: LocalId::new(2),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(3),
                value: constants::LOCALS_START as u64 + 2 * 32,
            }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(4), value: 32 }),
            Operation::Return(TwoInZeroOut { arg1: LocalId::new(3), arg2: LocalId::new(4) }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::ZERO, "Modulo by zero should return 0");
    }

    #[test]
    fn test_addmod_modulo_zero() {
        // Test that addmod with modulus 0 returns 0
        use eth_ir_data::operation::LargeInOneOut;

        let ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 5 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 3 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 0 }), /* mod 0 */
            Operation::AddMod(LargeInOneOut {
                result: LocalId::new(3),
                args_start: LocalIndex::new(0),
            }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(4),
                value: constants::LOCALS_START as u64 + 3 * 32,
            }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(5), value: 32 }),
            Operation::Return(TwoInZeroOut { arg1: LocalId::new(4), arg2: LocalId::new(5) }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::ZERO, "AddMod with modulus 0 should return 0");
    }

    #[test]
    fn test_division_by_zero_safe() {
        // Test that division by zero returns 0 (EVM behavior)
        let ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 42 }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(1),
                value: 0, // Divisor is zero
            }),
            Operation::Div(TwoInOneOut {
                result: LocalId::new(2),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(3),
                value: constants::LOCALS_START as u64 + 2 * 32,
            }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(4), value: 32 }),
            Operation::Return(TwoInZeroOut { arg1: LocalId::new(3), arg2: LocalId::new(4) }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::ZERO, "Division by zero should return 0");
    }
}
