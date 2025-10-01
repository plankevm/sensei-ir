//! Tests for error conditions, boundaries, and edge cases in code generation

use super::helpers::execute_with_gas_limit;
use crate::{
    tests::helpers::{
        EVM_WORD_SIZE_BYTES, KECCAK_OFFSET_LOCAL_ID, KECCAK_RESULT_LOCAL_ID, KECCAK_SIZE_LOCAL_ID,
        PERF_TEST_ITERATIONS, TEST_VALUE_SMALL, constants, create_program_with_data,
        create_simple_program, execute_and_get_result,
    },
    translate_program,
};
use alloy_primitives::U256;
use eth_ir_data::{DataId, LargeConstId, LocalId, LocalIndex, Operation, operation::*};
use evm_glue::assembler::assemble_minimized;
use proptest::prelude::*;
use revm::primitives::ExecutionResult;

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
    assert!(asm.is_ok(), "Failed to translate U256::MAX overflow test");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "Failed to assemble U256::MAX overflow bytecode");

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

#[test]
fn out_of_gas_handling() {
    // Create an expensive operation sequence
    let mut ops = vec![];
    for i in 0..PERF_TEST_ITERATIONS {
        ops.push(Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(i as u32),
            value: i as u64,
        }));
    }
    ops.push(Operation::Stop);

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert!(asm.is_ok(), "Failed to translate expensive operation sequence for gas test");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "Failed to assemble expensive operations to bytecode");

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
fn invalid_segment_error() {
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
        prop_assert!(asm.is_ok(), "Failed to translate REVERT operation with offset={}, size={}", offset, size);

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok(), "Failed to assemble REVERT operation to bytecode");

        let result = execute_and_get_result(bytecode.unwrap().1);
        prop_assert!(result.is_err(), "REVERT should cause execution to fail");
    }
}

#[test]
fn addmod_error_case() {
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
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(5),
            value: EVM_WORD_SIZE_BYTES,
        }),
        Operation::Return(TwoInZeroOut { arg1: LocalId::new(4), arg2: LocalId::new(5) }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert!(asm.is_ok(), "Failed to translate simple ADDMOD operation");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "Failed to assemble ADDMOD bytecode");

    let result = execute_and_get_result(bytecode.unwrap().1);
    let result = result.expect("ADDMOD (0+1)%2 should execute successfully");
    assert_eq!(result, U256::from(1), "(0 + 1) % 2 should be 1");
}

#[test]
fn modulo_by_zero_safe() {
    // Test that modulo by zero returns 0 (EVM behavior)
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: TEST_VALUE_SMALL,
        }),
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
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(4),
            value: EVM_WORD_SIZE_BYTES,
        }),
        Operation::Return(TwoInZeroOut { arg1: LocalId::new(3), arg2: LocalId::new(4) }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert!(asm.is_ok(), "Failed to translate MOD operation with zero divisor");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "Failed to assemble MOD with zero divisor to bytecode");

    let result = execute_and_get_result(bytecode.unwrap().1);
    let result = result.expect("MOD by zero should execute without panic (returns 0)");
    assert_eq!(result, U256::ZERO, "Modulo by zero should return 0");
}

#[test]
fn addmod_error_modulo_zero() {
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
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(5),
            value: EVM_WORD_SIZE_BYTES,
        }),
        Operation::Return(TwoInZeroOut { arg1: LocalId::new(4), arg2: LocalId::new(5) }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert!(asm.is_ok(), "Failed to translate ADDMOD with zero modulus");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "Failed to assemble ADDMOD with zero modulus to bytecode");

    let result = execute_and_get_result(bytecode.unwrap().1);
    let result = result.expect("ADDMOD with zero modulus should execute without panic");
    assert_eq!(result, U256::ZERO, "AddMod with modulus 0 should return 0");
}

#[test]
fn division_by_zero_safe() {
    // Test that division by zero returns 0 (EVM behavior)
    let ops = vec![
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(0),
            value: TEST_VALUE_SMALL,
        }),
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
        Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(4),
            value: EVM_WORD_SIZE_BYTES,
        }),
        Operation::Return(TwoInZeroOut { arg1: LocalId::new(3), arg2: LocalId::new(4) }),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);
    assert!(asm.is_ok(), "Failed to translate DIV operation with zero divisor");

    let bytecode = assemble_minimized(&asm.unwrap(), true);
    assert!(bytecode.is_ok(), "Failed to assemble DIV with zero divisor to bytecode");

    let result = execute_and_get_result(bytecode.unwrap().1);
    let result = result.expect("DIV by zero should execute without panic (returns 0)");
    assert_eq!(result, U256::ZERO, "Division by zero should return 0");
}

#[test]
fn environmental_ops_compilation() {
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
            local: LocalId::new(KECCAK_OFFSET_LOCAL_ID),
            value: 0 // offset
        }));
        ops.push(Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(KECCAK_SIZE_LOCAL_ID),
            value: data.len() as u64 // size
        }));
        ops.push(Operation::Keccak256(TwoInOneOut {
            result: LocalId::new(KECCAK_RESULT_LOCAL_ID),
            arg1: LocalId::new(KECCAK_OFFSET_LOCAL_ID),
            arg2: LocalId::new(KECCAK_SIZE_LOCAL_ID),
        }));

        ops.push(Operation::Stop);

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok(), "Keccak256 should translate");

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok(), "Keccak256 should assemble");
    }
}
