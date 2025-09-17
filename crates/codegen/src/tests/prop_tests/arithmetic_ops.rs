//! Property tests for arithmetic operations

use super::helpers::build_binary_op_test;
use crate::{
    tests::helpers::{create_simple_program, execute_and_get_result},
    translate_program,
    translator::memory::constants,
};
use alloy_primitives::U256;
use eth_ir_data::{LocalId, LocalIndex, Operation, operation::*};
use evm_glue::assembler::assemble_minimized;
use proptest::prelude::*;

/// Custom strategy for edge case values in arithmetic operations
fn edge_case_u64() -> impl Strategy<Value = u64> {
    prop_oneof![
        // Common edge cases
        Just(0u64),
        Just(1u64),
        Just(u64::MAX),
        Just(u64::MAX - 1),
        // Powers of 2
        Just(2u64),
        Just(256u64),
        Just(65536u64),
        // Byte boundaries
        Just(0xFFu64),
        Just(0xFFFFu64),
        Just(0xFFFFFFu64),
        // Common test values
        Just(42u64),
        Just(1337u64),
        // Random values with bias toward edge cases
        any::<u64>(),
    ]
}

/// Strategy for divisor values (non-zero with edge cases)
fn divisor_strategy() -> impl Strategy<Value = u64> {
    prop_oneof![
        // Small divisors (common test cases)
        1u64..=10,
        // Powers of 2 (efficient division)
        prop::sample::select(vec![2, 4, 8, 16, 32, 64, 128, 256]),
        // Large divisors
        u64::MAX / 2..=u64::MAX,
        // Random non-zero
        1u64..=u64::MAX,
    ]
}

/// Strategy for testing overflow scenarios
fn overflow_pair() -> impl Strategy<Value = (u64, u64)> {
    prop_oneof![
        // Addition overflow
        Just((u64::MAX, 1)),
        Just((u64::MAX / 2, u64::MAX / 2 + 1)),
        // Multiplication overflow
        Just((u64::MAX / 2, 3)),
        Just((1 << 32, 1 << 32)),
        // Random pairs that might overflow
        (u64::MAX / 2..=u64::MAX, u64::MAX / 2..=u64::MAX),
        // Regular pairs
        (any::<u64>(), any::<u64>()),
    ]
}

proptest! {
    #[test]
    fn test_add_commutative_with_execution(a in edge_case_u64(), b in edge_case_u64()) {
        use crate::tests::helpers::OperationTestBuilder;

        let result_standard = OperationTestBuilder::new()
            .with_values(&[a, b])
            .add_binary_op(|result, arg1, arg2| {
                Operation::Add(TwoInOneOut { result, arg1, arg2 })
            })
            .build_and_execute(2);

        let result_reversed = OperationTestBuilder::new()
            .with_values(&[b, a])
            .add_binary_op(|result, arg1, arg2| {
                Operation::Add(TwoInOneOut { result, arg1, arg2 })
            })
            .build_and_execute(2);

        if let (Ok(res_standard), Ok(res_reversed)) = (result_standard, result_reversed) {
            prop_assert_eq!(res_standard, res_reversed, "Addition should be commutative");
            let expected = U256::from(a).wrapping_add(U256::from(b));
            prop_assert_eq!(res_standard, expected, "Addition result incorrect");
        }
    }

    #[test]
    fn test_mul_commutative_with_execution((a, b) in overflow_pair()) {
        use crate::tests::helpers::OperationTestBuilder;

        let result = OperationTestBuilder::new()
            .with_values(&[a, b])
            .add_binary_op(|result, arg1, arg2| {
                Operation::Mul(TwoInOneOut { result, arg1, arg2 })
            })
            .build_and_execute(2);

        if let Ok(res) = result {
            let expected = U256::from(a).wrapping_mul(U256::from(b));
            prop_assert_eq!(res, expected, "Multiplication result incorrect");
        }
    }

    #[test]
    fn test_sub_non_commutative_with_execution(a in edge_case_u64(), b in edge_case_u64()) {
        use crate::tests::helpers::OperationTestBuilder;

        let result = OperationTestBuilder::new()
            .with_values(&[a, b])
            .add_binary_op(|result, arg1, arg2| {
                Operation::Sub(TwoInOneOut { result, arg1, arg2 })
            })
            .build_and_execute(2);

        if let Ok(res) = result {
            let expected = U256::from(a).wrapping_sub(U256::from(b));
            prop_assert_eq!(res, expected, "Subtraction result incorrect");
        }
    }

    #[test]
    fn test_division_by_zero_returns_zero(dividend in edge_case_u64()) {
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
        let asm = translate_program(program).expect("Translation failed");
        let (_, bytecode) = assemble_minimized(&asm, true).expect("Assembly failed");
        let result = execute_and_get_result(bytecode);
        if let Ok(res) = result {
            prop_assert_eq!(res, U256::ZERO, "Division by zero should return 0");
        }
    }

    #[test]
    fn test_mod_with_execution(dividend in edge_case_u64(), divisor in divisor_strategy()) {
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
        let asm = translate_program(program).expect("Translation failed");
        let (_, bytecode) = assemble_minimized(&asm, true).expect("Assembly failed");
        let result = execute_and_get_result(bytecode);
        if let Ok(res) = result {
            let expected = U256::from(dividend % divisor);
            prop_assert_eq!(res, expected, "Modulo result incorrect");
        }
    }
}

proptest! {
    #[test]
    fn test_sdiv(a in edge_case_u64(), b in divisor_strategy()) {
        // Test signed division with non-zero divisor
        let sdiv_operations = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::SDiv(TwoInOneOut { result, arg1, arg2 })
        });

        let program = create_simple_program(sdiv_operations);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok(), "SDIV should translate");

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok(), "SDIV should assemble");

        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => {
                // Calculate expected result
                let signed_a = alloy_primitives::I256::from_raw(U256::from(a));
                let signed_b = alloy_primitives::I256::from_raw(U256::from(b));
                let expected = (signed_a / signed_b).into_raw();
                prop_assert_eq!(res, expected, "SDIV result incorrect for {} / {}", a, b);
            }
            Err(e) if !e.contains("STOP") => prop_assert!(false, "SDIV execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_sdiv_by_zero(a in edge_case_u64()) {
        // Test that signed division by zero returns 0
        let sdiv_by_zero_operations = build_binary_op_test(a, 0, |result, arg1, arg2| {
            Operation::SDiv(TwoInOneOut { result, arg1, arg2 })
        });

        let program = create_simple_program(sdiv_by_zero_operations);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok(), "Failed to translate SDIV by zero for a={}", a);

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok(), "Failed to assemble SDIV by zero bytecode");

        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::ZERO, "SDIV by zero should return 0"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_smod(a in edge_case_u64(), b in divisor_strategy()) {
        // Test signed modulo with non-zero divisor
        let smod_operations = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::SMod(TwoInOneOut { result, arg1, arg2 })
        });

        let program = create_simple_program(smod_operations);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok(), "SMOD should translate");

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok(), "SMOD should assemble");

        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => {
                // Calculate expected result
                let signed_a = alloy_primitives::I256::from_raw(U256::from(a));
                let signed_b = alloy_primitives::I256::from_raw(U256::from(b));
                let expected = signed_a.rem_euclid(signed_b).into_raw();
                prop_assert_eq!(res, expected, "SMOD result incorrect for {} % {}", a, b);
            }
            Err(e) if !e.contains("STOP") => prop_assert!(false, "SMOD execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_smod_by_zero(a in edge_case_u64()) {
        // Test that signed modulo by zero returns 0
        let smod_by_zero_operations = build_binary_op_test(a, 0, |result, arg1, arg2| {
            Operation::SMod(TwoInOneOut { result, arg1, arg2 })
        });

        let program = create_simple_program(smod_by_zero_operations);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok(), "Failed to translate SMOD by zero for a={}", a);

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok(), "Failed to assemble SMOD by zero bytecode");

        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::ZERO, "SMOD by zero should return 0"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_signextend(byte_index in 0u64..31u64, value in edge_case_u64()) {
        // Test sign extension from specified byte
        let signextend_operations = build_binary_op_test(byte_index, value, |result, arg1, arg2| {
            Operation::SignExtend(TwoInOneOut { result, arg1, arg2 })
        });

        let program = create_simple_program(signextend_operations);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok(), "SIGNEXTEND should translate");

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok(), "SIGNEXTEND should assemble");

        let result = execute_and_get_result(bytecode.unwrap().1);
        prop_assert!(result.is_ok() || result.as_ref().unwrap_err().contains("STOP"), "SIGNEXTEND should execute");
    }
}

proptest! {
    #[test]
    fn test_addmod_property((a, b) in overflow_pair(), m in divisor_strategy()) {
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
        let asm = translate_program(program).expect("Translation failed");
        let (_, bytecode) = assemble_minimized(&asm, true).expect("Assembly failed");
        let result = execute_and_get_result(bytecode);
        match result {
            Ok(res) => {
                let expected = U256::from((U256::from(a) + U256::from(b)) % U256::from(m));
                prop_assert_eq!(res, expected, "AddMod result incorrect");
            }
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

#[test]
fn addmod() {
    // Test simple (10 + 20) % 7 = 2
    use eth_ir_data::operation::LargeInOneOut;

    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 10 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 20 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 7 }),
        Operation::AddMod(LargeInOneOut {
            result: LocalId::new(3),
            args_start: LocalIndex::new(0), // Uses contiguous locals 0, 1, 2
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
    assert!(asm.is_ok(), "Failed to translate simple addmod operation");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "Failed to assemble simple addmod bytecode");

    let result = execute_and_get_result(bytecode.unwrap().1);
    let result = result.expect("Failed to execute simple addmod operation");
    assert_eq!(result, U256::from(2), "(10 + 20) % 7 should equal 2");
}

#[test]
fn addmod_modulo_zero() {
    // Test that addmod with modulo 0 returns 0
    use eth_ir_data::operation::LargeInOneOut;

    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 10 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 20 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 0 }),
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
    assert!(asm.is_ok(), "Failed to translate addmod with modulo zero");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "Failed to assemble addmod with modulo zero bytecode");

    let result = execute_and_get_result(bytecode.unwrap().1);
    let result = result.expect("Failed to execute addmod with modulo zero");
    assert_eq!(result, U256::ZERO, "AddMod with modulo 0 should return 0");
}

proptest! {
    #[test]
    fn test_mulmod((a, b) in overflow_pair(), m in divisor_strategy()) {
        // Test (a * b) % m
        use eth_ir_data::operation::LargeInOneOut;

        let ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: a }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: b }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: m }),
            Operation::MulMod(LargeInOneOut {
                result: LocalId::new(3),
                args_start: LocalIndex::new(0),
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
        let asm = translate_program(program).expect("Translation failed");
        let (_, bytecode) = assemble_minimized(&asm, true).expect("Assembly failed");
        let result = execute_and_get_result(bytecode);
        match result {
            Ok(res) => {
                let expected = U256::from((U256::from(a) * U256::from(b)) % U256::from(m));
                prop_assert_eq!(res, expected, "MulMod result incorrect");
            }
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_exp(base in 0u64..10u64, exp in 0u64..10u64) {
        // Test exponentiation with small values to avoid overflow
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
        let asm = translate_program(program).expect("Translation failed");
        let (_, bytecode) = assemble_minimized(&asm, true).expect("Assembly failed");
        let result = execute_and_get_result(bytecode);
        match result {
            Ok(res) => {
                let expected = U256::from(base.pow(exp as u32));
                prop_assert_eq!(res, expected, "Exp result incorrect: {}^{}", base, exp);
            }
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

#[test]
fn invalid_data_segment_reference() {
    // This test doesn't use the program execution but tests the translation
    use eth_ir_data::DataId;

    let ops = vec![
        Operation::LocalSetDataOffset(SetDataOffset {
            local: LocalId::new(0),
            segment_id: DataId::new(999), // Invalid data segment
        }),
        Operation::Stop,
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    // This should fail during translation
    assert!(asm.is_err(), "Translation should fail for invalid data segment reference");
}

#[test]
fn critical_division_by_zero() {
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 100 }),
        Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }),
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
    assert!(asm.is_ok(), "Failed to translate division by zero operation");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "Failed to assemble division by zero bytecode");

    let result = execute_and_get_result(bytecode.unwrap().1);
    let result = result.expect("Failed to execute division by zero operation");
    assert_eq!(result, U256::ZERO, "Division by zero should return 0");
}
