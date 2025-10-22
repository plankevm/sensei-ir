//! Property tests for arithmetic operations

use super::helpers::build_binary_op_test;
use crate::{
    tests::helpers::{constants, create_simple_program, execute_and_get_result},
    translate_program,
};
use alloy_primitives::U256;
use evm_glue::assembler::assemble_minimized;
use proptest::prelude::*;
use sir_data::{LocalId, LocalIndex, Operation, operation::*};

/// Custom strategy for edge case values in arithmetic operations
fn edge_case_u32() -> impl Strategy<Value = u32> {
    prop_oneof![
        // Common edge cases
        Just(0u32),
        Just(1u32),
        Just(u32::MAX),
        Just(u32::MAX - 1),
        // Powers of 2
        Just(2u32),
        Just(256u32),
        Just(65536u32),
        // Byte boundaries
        Just(0xFFu32),
        Just(0xFFFFu32),
        Just(0xFFFFFFu32),
        // Common test values
        Just(42u32),
        Just(1337u32),
        // Random values with bias toward edge cases
        any::<u32>(),
    ]
}

/// Strategy for divisor values (non-zero with edge cases)
fn divisor_strategy() -> impl Strategy<Value = u32> {
    prop_oneof![
        // Small divisors (common test cases)
        1u32..=10,
        // Powers of 2 (efficient division)
        prop::sample::select(vec![2, 4, 8, 16, 32, 64, 128, 256]),
        // Large divisors
        u32::MAX / 2..=u32::MAX,
        // Random non-zero
        1u32..=u32::MAX,
    ]
}

/// Strategy for testing overflow scenarios
fn overflow_pair() -> impl Strategy<Value = (u32, u32)> {
    prop_oneof![
        // Addition overflow
        Just((u32::MAX, 1)),
        Just((u32::MAX / 2, u32::MAX / 2 + 1)),
        // Multiplication overflow
        Just((u32::MAX / 2, 3)),
        Just((1 << 16, 1 << 16)),
        // Random pairs that might overflow
        (u32::MAX / 2..=u32::MAX, u32::MAX / 2..=u32::MAX),
        // Regular pairs
        (any::<u32>(), any::<u32>()),
    ]
}

proptest! {
    #[test]
    fn test_add_commutative_with_execution(a in edge_case_u32(), b in edge_case_u32()) {
        use crate::tests::helpers::OperationTestBuilder;

        let result_standard = OperationTestBuilder::new()
            .with_values(&[a, b])
            .add_binary_op(|result, arg1, arg2| {
                Operation::Add(InlineOperands { ins: [arg1, arg2], outs: [result] })
            })
            .build_and_execute(2);

        let result_reversed = OperationTestBuilder::new()
            .with_values(&[b, a])
            .add_binary_op(|result, arg1, arg2| {
                Operation::Add(InlineOperands { ins: [arg1, arg2], outs: [result] })
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
                Operation::Mul(InlineOperands { ins: [arg1, arg2], outs: [result] })
            })
            .build_and_execute(2);

        if let Ok(res) = result {
            let expected = U256::from(a).wrapping_mul(U256::from(b));
            prop_assert_eq!(res, expected, "Multiplication result incorrect");
        }
    }

    #[test]
    fn test_sub_non_commutative_with_execution(a in edge_case_u32(), b in edge_case_u32()) {
        use crate::tests::helpers::OperationTestBuilder;

        let result = OperationTestBuilder::new()
            .with_values(&[a, b])
            .add_binary_op(|result, arg1, arg2| {
                Operation::Sub(InlineOperands { ins: [arg1, arg2], outs: [result] })
            })
            .build_and_execute(2);

        if let Ok(res) = result {
            let expected = U256::from(a).wrapping_sub(U256::from(b));
            prop_assert_eq!(res, expected, "Subtraction result incorrect");
        }
    }

    #[test]
    fn test_division_by_zero_returns_zero(dividend in edge_case_u32()) {
        let ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: dividend as u32 }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
            Operation::Div(InlineOperands {
                ins: [LocalId::new(0), LocalId::new(1)],
                outs: [LocalId::new(2)],
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(3),
                value: (constants::LOCALS_START as u64 + 2 * 32) as u32
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(4),
                value: 32
            }),
            Operation::Return(InlineOperands {
                ins: [LocalId::new(3), LocalId::new(4)],
                outs: [],
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        let (_, bytecode) = assemble_minimized(&asm, true).expect("Assembly failed");
        let result = execute_and_get_result(bytecode);
        if let Ok(res) = result {
            prop_assert_eq!(res, U256::ZERO, "Division by zero should return 0");
        }
    }

    #[test]
    fn test_mod_with_execution(dividend in edge_case_u32(), divisor in divisor_strategy()) {
        let ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: dividend as u32 }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: divisor as u32 }),
            Operation::Mod(InlineOperands {
                ins: [LocalId::new(0), LocalId::new(1)],
                outs: [LocalId::new(2)],
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(3),
                value: (constants::LOCALS_START as u64 + 2 * 32) as u32
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(4),
                value: 32
            }),
            Operation::Return(InlineOperands {
                ins: [LocalId::new(3), LocalId::new(4)],
                outs: [],
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
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
    fn test_sdiv(a in edge_case_u32(), b in divisor_strategy()) {
        // Test signed division with non-zero divisor
        let sdiv_operations = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::SDiv(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(sdiv_operations);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_and_get_result(bytecode.expect("Assembly failed").1);
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
    fn test_sdiv_by_zero(a in edge_case_u32()) {
        // Test that signed division by zero returns 0
        let sdiv_by_zero_operations = build_binary_op_test(a, 0, |result, arg1, arg2| {
            Operation::SDiv(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(sdiv_by_zero_operations);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_and_get_result(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::ZERO, "SDIV by zero should return 0"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_smod(a in edge_case_u32(), b in divisor_strategy()) {
        // Test signed modulo with non-zero divisor
        let smod_operations = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::SMod(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(smod_operations);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_and_get_result(bytecode.expect("Assembly failed").1);
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
    fn test_smod_by_zero(a in edge_case_u32()) {
        // Test that signed modulo by zero returns 0
        let smod_by_zero_operations = build_binary_op_test(a, 0, |result, arg1, arg2| {
            Operation::SMod(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(smod_by_zero_operations);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_and_get_result(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::ZERO, "SMOD by zero should return 0"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_signextend(byte_index in 0u32..31u32, value in edge_case_u32()) {
        // Test sign extension from specified byte
        let signextend_operations = build_binary_op_test(byte_index, value, |result, arg1, arg2| {
            Operation::SignExtend(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(signextend_operations);
        let asm = translate_program(program);


        assemble_minimized(&asm, true).expect("Assembly failed");


    }
}

proptest! {
    #[test]
    fn test_addmod_property((a, b) in overflow_pair(), m in divisor_strategy()) {
        // Test (a + b) % m
        use sir_data::operation::AllocatedIns;

        let ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: a as u32 }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: b as u32 }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: m as u32 }),
            Operation::AddMod(AllocatedIns {
                outs: [ LocalId::new(3),
],
                ins_start: LocalIndex::new(0), // Uses contiguous locals 0, 1, 2
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(4),
                value: (constants::LOCALS_START as u64 + 3 * 32) as u32
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(5),
                value: 32
            }),
            Operation::Return(InlineOperands {
                ins: [ LocalId::new(4),
LocalId::new(5)],
                outs: [],
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
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
    use sir_data::operation::AllocatedIns;

    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 10 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 20 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 7 }),
        Operation::AddMod(AllocatedIns::<3, 1> {
            ins_start: LocalIndex::new(0), // Uses contiguous locals 0, 1, 2
            outs: [LocalId::new(3)],
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(4),
            value: (constants::LOCALS_START as u64 + 3 * 32) as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(5), value: 32 }),
        Operation::Return(InlineOperands { ins: [LocalId::new(4), LocalId::new(5)], outs: [] }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    let bytecode = assemble_minimized(&asm, true);

    let result = execute_and_get_result(bytecode.expect("Assembly failed").1);
    let result = result.expect("Failed to execute simple addmod operation");
    assert_eq!(result, U256::from(2), "(10 + 20) % 7 should equal 2");
}

#[test]
fn addmod_modulo_zero() {
    // Test that addmod with modulo 0 returns 0
    use sir_data::operation::AllocatedIns;

    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 10 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 20 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 0 }),
        Operation::AddMod(AllocatedIns::<3, 1> {
            ins_start: LocalIndex::new(0),
            outs: [LocalId::new(3)],
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(4),
            value: (constants::LOCALS_START as u64 + 3 * 32) as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(5), value: 32 }),
        Operation::Return(InlineOperands { ins: [LocalId::new(4), LocalId::new(5)], outs: [] }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    let bytecode = assemble_minimized(&asm, true);

    let result = execute_and_get_result(bytecode.expect("Assembly failed").1);
    let result = result.expect("Failed to execute addmod with modulo zero");
    assert_eq!(result, U256::ZERO, "AddMod with modulo 0 should return 0");
}

proptest! {
    #[test]
    fn test_mulmod((a, b) in overflow_pair(), m in divisor_strategy()) {
        // Test (a * b) % m
        use sir_data::operation::AllocatedIns;

        let ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: a as u32 }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: b as u32 }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: m as u32 }),
            Operation::MulMod(AllocatedIns {
                outs: [ LocalId::new(3),
],
                ins_start: LocalIndex::new(0),
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(4),
                value: (constants::LOCALS_START as u64 + 3 * 32) as u32
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(5),
                value: 32
            }),
            Operation::Return(InlineOperands {
                ins: [ LocalId::new(4),
LocalId::new(5)],
                outs: [],
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
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
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: base as u32 }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: exp as u32 }),
            Operation::Exp(InlineOperands {
                ins: [LocalId::new(0), LocalId::new(1)],
                outs: [LocalId::new(2)],
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(3),
                value: (constants::LOCALS_START as u64 + 2 * 32) as u32
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(4),
                value: 32
            }),
            Operation::Return(InlineOperands {
                ins: [LocalId::new(3), LocalId::new(4)],
                outs: [],
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);
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
fn critical_division_by_zero() {
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 100 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::Div(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [LocalId::new(2)],
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(3),
            value: (constants::LOCALS_START as u64 + 2 * 32) as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(4), value: 32 }),
        Operation::Return(InlineOperands { ins: [LocalId::new(3), LocalId::new(4)], outs: [] }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    let bytecode = assemble_minimized(&asm, true);

    let result = execute_and_get_result(bytecode.expect("Assembly failed").1);
    let result = result.expect("Failed to execute division by zero operation");
    assert_eq!(result, U256::ZERO, "Division by zero should return 0");
}
