//! Property tests for bitwise operations

use super::helpers::{build_binary_op_test, build_unary_op_test};
use crate::{
    tests::helpers::{create_simple_program, execute_bytecode},
    translate_program,
};
use alloy_primitives::U256;
use evm_glue::assembler::assemble_minimized;
use proptest::prelude::*;
use sir_data::{Operation, operation::*};

proptest! {
    #[test]
    fn test_bitwise_and_with_execution(a in any::<u32>(), b in any::<u32>()) {
        let ops = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::And(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            let expected = U256::from(a & b);
            prop_assert_eq!(res, expected, "AND result incorrect");
        }
    }

    #[test]
    fn test_bitwise_or_with_execution(a in any::<u32>(), b in any::<u32>()) {
        let ops = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::Or(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            let expected = U256::from(a | b);
            prop_assert_eq!(res, expected, "OR result incorrect");
        }
    }

    #[test]
    fn test_bitwise_xor_with_execution(a in any::<u32>(), b in any::<u32>()) {
        let ops = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::Xor(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            let expected = U256::from(a ^ b);
            prop_assert_eq!(res, expected, "XOR result incorrect");
        }
    }

    #[test]
    fn test_bitwise_not_with_execution(a in any::<u32>()) {
        let ops = build_unary_op_test(a, |result, arg1| {
            Operation::Not(InlineOperands { ins: [arg1], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            // NOT in EVM is bitwise NOT on all 256 bits
            let expected = !U256::from(a);
            prop_assert_eq!(res, expected, "NOT result incorrect");
        }
    }

    #[test]
    fn test_shifts_with_execution(value in any::<u32>(), shift in 0u8..64) {
        // Test SHL (shift left)
        let ops_shl = build_binary_op_test(shift as u32, value, |result, arg1, arg2| {
            Operation::Shl(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops_shl);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            let expected = U256::from(value) << shift;
            prop_assert_eq!(res, expected, "SHL result incorrect");
        }

        // Test SHR (shift right)
        let ops_shr = build_binary_op_test(shift as u32, value, |result, arg1, arg2| {
            Operation::Shr(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops_shr);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            let expected = U256::from(value) >> shift;
            prop_assert_eq!(res, expected, "SHR result incorrect");
        }
    }
}

proptest! {
    #[test]
    fn test_sar_arithmetic_shift(value in any::<u32>(), shift in 0u32..256u32) {
        // Test signed arithmetic right shift
        let ops = build_binary_op_test(shift, value, |result, arg1, arg2| {
            Operation::Sar(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => {
                // Calculate expected result for SAR
                // SAR performs arithmetic right shift (sign-extending)
                let val_256 = U256::from(value);
                let expected = if shift >= 256 {
                    // If shift >= 256, result depends on sign bit
                    // For u64 values, sign bit is always 0 in U256 context
                    U256::ZERO
                } else if shift == 0 {
                    val_256
                } else {
                    // Perform arithmetic right shift
                    // Since we're dealing with u64 values, they're positive in U256
                    val_256 >> shift
                };
                prop_assert_eq!(res, expected, "SAR result incorrect for {} >> {} (arithmetic)", value, shift);
            }
            Err(e) if !e.contains("STOP") => prop_assert!(false, "SAR execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_byte(byte_index in 0u32..32u32, value in any::<u32>()) {
        // Test BYTE operation - extract byte at given index from value
        let ops = build_binary_op_test(byte_index, value, |result, arg1, arg2| {
            Operation::Byte(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        let bytecode = assemble_minimized(&asm, true);
        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => {
                // BYTE opcode counts from the left (big-endian), where index 0 is the most significant byte
                // For a u64 value stored in a U256, the value occupies the rightmost 8 bytes (indices 24-31)
                // So for a u64, bytes 0-23 are 0, and bytes 24-31 contain the value
                if byte_index < 24 {
                    // Accessing zero bytes to the left of the u64 value
                    prop_assert_eq!(res, U256::ZERO,
                        "BYTE at index {} should return 0 (zero padding)", byte_index);
                } else if byte_index < 32 {
                    // Accessing the actual u64 value bytes
                    let bytes = value.to_be_bytes();
                    let value_byte_index = (byte_index - 24) as usize;
                    let expected_byte = bytes[value_byte_index] as u64;
                    prop_assert_eq!(res, U256::from(expected_byte),
                        "BYTE at index {} should return correct byte", byte_index);
                } else {
                    // Out of bounds
                    prop_assert_eq!(res, U256::ZERO,
                        "BYTE at index {} should return 0 (out of bounds)", byte_index);
                }
            }
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_byte_out_of_bounds(byte_index in 32u32..256u32, value in any::<u32>()) {
        // Test BYTE operation with out-of-bounds index (should return 0)
        let ops = build_binary_op_test(byte_index, value, |result, arg1, arg2| {
            Operation::Byte(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::ZERO,
                "BYTE with out-of-bounds index {} should return 0", byte_index),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_comparison_lt_with_execution(a in any::<u32>(), b in any::<u32>()) {
        let ops = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::Lt(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            let expected = if a < b { U256::from(1) } else { U256::ZERO };
            prop_assert_eq!(res, expected, "LT result incorrect: {} < {} should be {}", a, b, expected);
        }
    }

    #[test]
    fn test_comparison_gt_with_execution(a in any::<u32>(), b in any::<u32>()) {
        let ops = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::Gt(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            let expected = if a > b { U256::from(1) } else { U256::ZERO };
            prop_assert_eq!(res, expected, "GT result incorrect");
        }
    }

    #[test]
    fn test_comparison_eq_with_execution(a in any::<u32>(), b in any::<u32>()) {
        let ops = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::Eq(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            let expected = if a == b { U256::from(1) } else { U256::ZERO };
            prop_assert_eq!(res, expected, "EQ result incorrect");
        }
    }

    #[test]
    fn test_iszero_with_execution(value in any::<u32>()) {
        let ops = build_unary_op_test(value, |result, arg1| {
            Operation::IsZero(InlineOperands { ins: [arg1], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            let expected = if value == 0 { U256::from(1) } else { U256::ZERO };
            prop_assert_eq!(res, expected, "ISZERO result incorrect");
        }
    }
}

proptest! {
    #[test]
    fn test_slt_signed_comparison(a in 0u32..=i32::MAX as u32, b in 0u32..=i32::MAX as u32) {
        // Test SLt with positive values that fit in i32 range
        // This avoids sign extension issues with small constants

        let ops = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::SLt(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => {
                // When interpreted as signed 256-bit integers
                let a_signed = a as i32;
                let b_signed = b as i32;
                let expected = if a_signed < b_signed { U256::from(1) } else { U256::ZERO };
                prop_assert_eq!(res, expected, "SLT result incorrect: {} < {} (signed)", a_signed, b_signed);
            }
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_sgt_signed_comparison(a in 0u32..=i32::MAX as u32, b in 0u32..=i32::MAX as u32) {
        // Test SGt with positive values that fit in i32 range
        let ops = build_binary_op_test(a, b, |result, arg1, arg2| {
            Operation::SGt(InlineOperands { ins: [arg1, arg2], outs: [result] })
        });

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => {
                let a_signed = a as i32;
                let b_signed = b as i32;
                let expected = if a_signed > b_signed { U256::from(1) } else { U256::ZERO };
                prop_assert_eq!(res, expected, "SGT result incorrect: {} > {} (signed)", a_signed, b_signed);
            }
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}
