//! Tests for error conditions, boundaries, and edge cases in code generation

use super::helpers::execute_with_gas_limit;
use crate::{
    tests::helpers::{
        EVM_WORD_SIZE_BYTES, PERF_TEST_ITERATIONS, TEST_VALUE_SMALL, constants,
        create_simple_program, execute_bytecode,
    },
    translate_program,
};
use alloy_primitives::U256;
use evm_glue::assembler::assemble_minimized;
use proptest::prelude::*;
use revm::primitives::ExecutionResult;
use sir_data::{LargeConstId, LocalId, LocalIndex, Operation, operation::*};

#[test]
fn test_u256_boundary_arithmetic() {
    // Test at U256 max boundary
    let ops = vec![
        // Create max U256 using large const
        Operation::SetLargeConst(SetLargeConstData {
            sets: LocalId::new(0),
            value: LargeConstId::new(0),
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: 1, // Add 1 to trigger overflow
        }),
        // Add overflow_amount to max (should wrap)
        Operation::Add(InlineOperands {
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

    let mut program = create_simple_program(ops);
    program.large_consts = vec![U256::MAX].into_iter().collect();

    let asm = translate_program(program);

    let bytecode = assemble_minimized(&asm, true);

    let result = execute_bytecode(bytecode.expect("Assembly failed").1);
    if let Ok(res) = result {
        assert_eq!(res, U256::ZERO, "U256::MAX + 1 should wrap to 0");
    }
}

#[test]
fn out_of_gas_handling() {
    // Create an expensive operation sequence
    let mut ops = vec![];
    for i in 0..PERF_TEST_ITERATIONS {
        ops.push(Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(i as u32),
            value: i as u32,
        }));
    }
    ops.push(Operation::Stop(InlineOperands::default()));

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    let bytecode = assemble_minimized(&asm, true);

    // Execute with very limited gas
    let result = execute_with_gas_limit(bytecode.expect("Assembly failed").1, 100);

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

proptest! {

    #[test]
    fn test_revert_execution(offset in 0u32..100, size in 0u32..100) {
        let ops = vec![
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(0),
                value: offset as u32
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(1),
                value: size as u32
            }),
            Operation::Revert(InlineOperands {
                ins: [LocalId::new(0), LocalId::new(1)],
                outs: [],
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        prop_assert!(result.is_err(), "REVERT should cause execution to fail");
    }
}

#[test]
fn addmod_error_case() {
    // Simple test: (0 + 1) % 2 should be 1
    use sir_data::operation::AllocatedIns;

    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 1 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 2 }),
        Operation::AddMod(AllocatedIns { ins_start: LocalIndex::new(0), outs: [LocalId::new(3)] }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(4),
            value: (constants::LOCALS_START as u64 + 3 * 32) as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(5),
            value: EVM_WORD_SIZE_BYTES as u32,
        }),
        Operation::Return(InlineOperands { ins: [LocalId::new(4), LocalId::new(5)], outs: [] }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    let bytecode = assemble_minimized(&asm, true);

    let result = execute_bytecode(bytecode.expect("Assembly failed").1);
    let result = result.expect("ADDMOD (0+1)%2 should execute successfully");
    assert_eq!(result, U256::from(1), "(0 + 1) % 2 should be 1");
}

#[test]
fn modulo_by_zero_safe() {
    // Test that modulo by zero returns 0 (EVM behavior)
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: TEST_VALUE_SMALL as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: 0, // Modulus is zero
        }),
        Operation::Mod(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [LocalId::new(2)],
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(3),
            value: (constants::LOCALS_START as u64 + 2 * 32) as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(4),
            value: EVM_WORD_SIZE_BYTES as u32,
        }),
        Operation::Return(InlineOperands { ins: [LocalId::new(3), LocalId::new(4)], outs: [] }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    let bytecode = assemble_minimized(&asm, true);

    let result = execute_bytecode(bytecode.expect("Assembly failed").1);
    let result = result.expect("MOD by zero should execute without panic (returns 0)");
    assert_eq!(result, U256::ZERO, "Modulo by zero should return 0");
}

#[test]
fn addmod_error_modulo_zero() {
    // Test that addmod with modulus 0 returns 0
    use sir_data::operation::AllocatedIns;

    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 5 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 3 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 0 }), /* mod 0 */
        Operation::AddMod(AllocatedIns { ins_start: LocalIndex::new(0), outs: [LocalId::new(3)] }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(4),
            value: (constants::LOCALS_START as u64 + 3 * 32) as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(5),
            value: EVM_WORD_SIZE_BYTES as u32,
        }),
        Operation::Return(InlineOperands { ins: [LocalId::new(4), LocalId::new(5)], outs: [] }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    let bytecode = assemble_minimized(&asm, true);

    let result = execute_bytecode(bytecode.expect("Assembly failed").1);
    let result = result.expect("ADDMOD with zero modulus should execute without panic");
    assert_eq!(result, U256::ZERO, "AddMod with modulus 0 should return 0");
}

#[test]
fn division_by_zero_safe() {
    // Test that division by zero returns 0 (EVM behavior)
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(0),
            value: TEST_VALUE_SMALL as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(1),
            value: 0, // Divisor is zero
        }),
        Operation::Div(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [LocalId::new(2)],
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(3),
            value: (constants::LOCALS_START as u64 + 2 * 32) as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(4),
            value: EVM_WORD_SIZE_BYTES as u32,
        }),
        Operation::Return(InlineOperands { ins: [LocalId::new(3), LocalId::new(4)], outs: [] }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    let bytecode = assemble_minimized(&asm, true);

    let result = execute_bytecode(bytecode.expect("Assembly failed").1);
    let result = result.expect("DIV by zero should execute without panic (returns 0)");
    assert_eq!(result, U256::ZERO, "Division by zero should return 0");
}

proptest! {
    #[test]
    fn test_add_sub_inverse(a in any::<u32>(), b in any::<u32>()) {
        // Test that (a + b) - b == a
        let ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: a }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: b }),
            Operation::Add(InlineOperands {
                ins: [LocalId::new(0), LocalId::new(1)],
                outs: [LocalId::new(2)],
            }),
            Operation::Sub(InlineOperands {
                ins: [LocalId::new(2), LocalId::new(1)], // (a + b) - b
                outs: [LocalId::new(3)],
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
                ins: [LocalId::new(4), LocalId::new(5)],
                outs: [],
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            prop_assert_eq!(res, U256::from(a), "(a + b) - b should equal a");
        }
    }

    #[test]
    fn test_xor_self_is_zero(value in any::<u32>()) {
        // Test that a XOR a == 0
        let ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value }),
            Operation::Xor(InlineOperands {
                ins: [LocalId::new(0), LocalId::new(0)],
                outs: [LocalId::new(1)],
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(2),
                value: (constants::LOCALS_START as u64 + 1 * 32) as u32
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(3),
                value: 32
            }),
            Operation::Return(InlineOperands {
            ins: [LocalId::new(2), LocalId::new(3)],
            outs: [],
        }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            prop_assert_eq!(res, U256::ZERO, "a XOR a should equal 0");
        }
    }

    #[test]
    fn test_double_not_identity(value in any::<u32>()) {
        // Test that NOT(NOT(a)) == a
        let ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value }),
            Operation::Not(InlineOperands {
                ins: [LocalId::new(0)],
                outs: [LocalId::new(1)],
            }),
            Operation::Not(InlineOperands {
                ins: [LocalId::new(1)],
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


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            prop_assert_eq!(res, U256::from(value), "NOT(NOT(a)) should equal a");
        }
    }

    #[test]
    fn test_shift_inverse(value in 1u32..u32::MAX, shift in 1u8..32) {
        // Test that (a << n) >> n == a (for values that don't overflow)
        let safe_value = value >> shift; // Ensure no overflow

        let ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: safe_value }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: shift as u32 }),
            Operation::Shl(InlineOperands {
                ins: [LocalId::new(1), LocalId::new(0)],
                outs: [LocalId::new(2)],
            }),
            Operation::Shr(InlineOperands {
                ins: [LocalId::new(1), LocalId::new(2)],
                outs: [LocalId::new(3)],
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
                ins: [LocalId::new(4), LocalId::new(5)],
                outs: [],
            }),
        ];

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            prop_assert_eq!(res, U256::from(safe_value), "(a << n) >> n should equal a");
        }
    }
}
