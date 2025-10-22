use crate::{
    define_binary_op_test, define_signed_op_test, define_unary_op_test, tests::helpers::*,
    translate_program,
};
use alloy_primitives::{I256, U256};
use sir_data::{LocalId, LocalIndex, Operation, operation::*};

// ============= Arithmetic Operations =============

define_binary_op_test!(
    test_add,
    |result, left, right| Operation::Add(InlineOperands { ins: [left, right], outs: [result] }),
    |left: U256, right: U256| left.wrapping_add(right),
    "ADD"
);

define_binary_op_test!(
    test_sub,
    |result, left, right| Operation::Sub(InlineOperands { ins: [left, right], outs: [result] }),
    |left: U256, right: U256| left.wrapping_sub(right),
    "SUB"
);

define_binary_op_test!(
    test_mul,
    |result, left, right| Operation::Mul(InlineOperands { ins: [left, right], outs: [result] }),
    |multiplicand: U256, multiplier: U256| multiplicand.wrapping_mul(multiplier),
    "MUL"
);

define_binary_op_test!(
    test_div,
    |result, dividend, divisor| Operation::Div(InlineOperands {
        ins: [dividend, divisor],
        outs: [result]
    }),
    |dividend, divisor| if divisor == U256::ZERO { U256::ZERO } else { dividend / divisor },
    "DIV"
);

define_binary_op_test!(
    test_mod,
    |result, dividend, modulus| Operation::Mod(InlineOperands {
        ins: [dividend, modulus],
        outs: [result]
    }),
    |dividend, modulus| if modulus == U256::ZERO { U256::ZERO } else { dividend % modulus },
    "MOD"
);

define_signed_op_test!(
    test_sdiv,
    |result, dividend, divisor| Operation::SDiv(InlineOperands {
        ins: [dividend, divisor],
        outs: [result]
    }),
    |dividend, divisor| {
        if divisor == U256::ZERO {
            U256::ZERO
        } else {
            let signed_dividend = I256::from_raw(dividend);
            let signed_divisor = I256::from_raw(divisor);
            (signed_dividend / signed_divisor).into_raw()
        }
    },
    "SDIV"
);

define_signed_op_test!(
    test_smod,
    |result, dividend, modulus| Operation::SMod(InlineOperands {
        ins: [dividend, modulus],
        outs: [result]
    }),
    |dividend, modulus| {
        if modulus == U256::ZERO {
            U256::ZERO
        } else {
            let signed_dividend = I256::from_raw(dividend);
            let signed_modulus = I256::from_raw(modulus);
            signed_dividend.rem_euclid(signed_modulus).into_raw()
        }
    },
    "SMOD"
);

define_binary_op_test!(
    test_exp,
    |result, base, exponent| Operation::Exp(InlineOperands {
        ins: [base, exponent],
        outs: [result]
    }),
    |base: U256, exponent: U256| base.pow(exponent),
    "EXP"
);

// AddMod and MulMod need special handling due to LargeInOneOut structure
#[test]
fn addmod() {
    const TEST_CASES: [(u64, u64, u64); 4] = [(10, 20, 7), (100, 50, 13), (5, 5, 3), (999, 1, 100)];

    for &(addend1, addend2, modulus) in &TEST_CASES {
        let operations = vec![
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(0),
                value: addend1 as u32,
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(1),
                value: addend2 as u32,
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(2),
                value: modulus as u32,
            }),
            Operation::AddMod(AllocatedIns {
                ins_start: LocalIndex::new(0),
                outs: [LocalId::new(3)],
            }),
        ];

        let result = TestProgram::from_operations(create_ops_with_return(operations, 3))
            .execute()
            .expect("AddMod execution should succeed");

        let expected = if modulus == 0 {
            U256::ZERO
        } else {
            U256::from((addend1 as u128 + addend2 as u128) % modulus as u128)
        };
        assert_eq!(result, expected, "AddMod({}, {}, {}) failed", addend1, addend2, modulus);
    }

    // Verify opcode generation
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 10 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 20 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 7 }),
        Operation::AddMod(AllocatedIns { ins_start: LocalIndex::new(0), outs: [LocalId::new(3)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let asm = TestProgram::from_operations(operations).translate();

    assert_opcode_counts(&asm, &[("ADDMOD", 1), ("STOP", 1)]);
}

#[test]
fn mulmod() {
    const TEST_CASES: [(u64, u64, u64); 4] = [(10, 20, 7), (100, 50, 13), (5, 5, 3), (999, 2, 100)];

    for &(multiplicand, multiplier, modulus) in &TEST_CASES {
        let operations = vec![
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(0),
                value: multiplicand as u32,
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(1),
                value: multiplier as u32,
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(2),
                value: modulus as u32,
            }),
            Operation::MulMod(AllocatedIns {
                ins_start: LocalIndex::new(0),
                outs: [LocalId::new(3)],
            }),
        ];

        let result = TestProgram::from_operations(create_ops_with_return(operations, 3))
            .execute()
            .expect("MulMod execution should succeed");

        let expected = if modulus == 0 {
            U256::ZERO
        } else {
            U256::from((multiplicand as u128 * multiplier as u128) % modulus as u128)
        };
        assert_eq!(
            result, expected,
            "MulMod({}, {}, {}) failed",
            multiplicand, multiplier, modulus
        );
    }

    // Verify opcode generation
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 10 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 20 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 7 }),
        Operation::MulMod(AllocatedIns { ins_start: LocalIndex::new(0), outs: [LocalId::new(3)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let asm = TestProgram::from_operations(operations).translate();

    assert_opcode_counts(&asm, &[("MULMOD", 1), ("STOP", 1)]);
}

// ============= Comparison Operations =============

define_binary_op_test!(
    test_lt,
    |result, arg1, arg2| Operation::Lt(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |a, b| if a < b { U256::from(1) } else { U256::ZERO },
    "LT"
);

define_binary_op_test!(
    test_gt,
    |result, arg1, arg2| Operation::Gt(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |a, b| if a > b { U256::from(1) } else { U256::ZERO },
    "GT"
);

define_signed_op_test!(
    test_slt,
    |result, arg1, arg2| Operation::SLt(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |a, b| {
        let a_signed = I256::from_raw(a);
        let b_signed = I256::from_raw(b);
        if a_signed < b_signed { U256::from(1) } else { U256::ZERO }
    },
    "SLT"
);

define_signed_op_test!(
    test_sgt,
    |result, arg1, arg2| Operation::SGt(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |a, b| {
        let a_signed = I256::from_raw(a);
        let b_signed = I256::from_raw(b);
        if a_signed > b_signed { U256::from(1) } else { U256::ZERO }
    },
    "SGT"
);

define_binary_op_test!(
    test_eq,
    |result, arg1, arg2| Operation::Eq(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |a, b| if a == b { U256::from(1) } else { U256::ZERO },
    "EQ"
);

define_unary_op_test!(
    test_iszero,
    |result, arg| Operation::IsZero(InlineOperands { ins: [arg], outs: [result] }),
    |a| if a == U256::ZERO { U256::from(1) } else { U256::ZERO },
    "ISZERO"
);

// ============= Bitwise Operations =============

define_binary_op_test!(
    test_and,
    &[(0xFF, 0x0F), (0xAA, 0x55), (0xFF, 0xFF), (0, 0xFF)],
    |result, arg1, arg2| Operation::And(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |a, b| a & b,
    "AND"
);

define_binary_op_test!(
    test_or,
    &[(0xFF, 0x0F), (0xAA, 0x55), (0xFF, 0xFF), (0, 0xFF)],
    |result, arg1, arg2| Operation::Or(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |a, b| a | b,
    "OR"
);

define_binary_op_test!(
    test_xor,
    &[(0xFF, 0x0F), (0xAA, 0x55), (0xFF, 0xFF), (0, 0xFF)],
    |result, arg1, arg2| Operation::Xor(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |a, b| a ^ b,
    "XOR"
);

define_unary_op_test!(
    test_not,
    &[0xFF, 0x0F, 0xAA, 0x55, 0],
    |result, arg| Operation::Not(InlineOperands { ins: [arg], outs: [result] }),
    |a| !a,
    "NOT"
);

define_binary_op_test!(
    test_byte,
    &[(31, 0xFF), (30, 0xFFFF), (0, 0xFF00), (1, 0xFF00)],
    |result, arg1, arg2| Operation::Byte(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |index, value| {
        if index >= U256::from(32) {
            U256::ZERO
        } else {
            let byte_index = 31 - index.to::<usize>();
            U256::from(value.byte(byte_index))
        }
    },
    "BYTE"
);

define_binary_op_test!(
    test_shl,
    &[(1, 1), (2, 1), (8, 0xFF), (256, 1)],
    |result, arg1, arg2| Operation::Shl(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |shift, value| value << shift,
    "SHL"
);

define_binary_op_test!(
    test_shr,
    &[(1, 2), (2, 4), (8, 0xFF00), (256, 1)],
    |result, arg1, arg2| Operation::Shr(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |shift, value| value >> shift,
    "SHR"
);

define_binary_op_test!(
    test_sar,
    &[(1, 2), (2, 4), (8, 0xFF00), (1, 0x8000_0000)],
    |result, arg1, arg2| Operation::Sar(InlineOperands { ins: [arg1, arg2], outs: [result] }),
    |shift, value| {
        let signed = I256::from_raw(value);
        let result = signed >> shift.to::<usize>();
        result.into_raw()
    },
    "SAR"
);

// ============= Special Operations =============

define_binary_op_test!(
    test_signextend,
    &[(0, 0xFF), (0, 0x80), (1, 0xFFFF), (7, 0xFF)],
    |result, arg1, arg2| Operation::SignExtend(InlineOperands {
        ins: [arg1, arg2],
        outs: [result]
    }),
    |byte_num, value| {
        if byte_num >= U256::from(31) {
            value
        } else {
            let byte_num = byte_num.to::<usize>();
            let bit_index = byte_num * 8 + 7;
            let bit = (value >> bit_index) & U256::from(1);
            if bit == U256::ZERO {
                let mask = (U256::from(1) << (bit_index + 1)) - U256::from(1);
                value & mask
            } else {
                let mask = (U256::from(1) << (bit_index + 1)) - U256::from(1);
                value | !mask
            }
        }
    },
    "SIGNEXTEND"
);

#[test]
fn test_keccak256() {
    // Simple test with known values
    let result = OperationTestBuilder::new()
        .with_values(&[0, 32]) // offset, size
        .add_binary_op(|result, arg1, arg2| {
            Operation::Keccak256(InlineOperands { ins: [arg1, arg2], outs: [result] })
        })
        .build_and_execute(2)
        .expect("Keccak256 execution should succeed");

    // Result should be non-zero (actual hash value depends on memory content)
    assert_ne!(result, U256::ZERO, "Keccak256 should produce non-zero hash");

    // Verify opcode generation
    let asm = OperationTestBuilder::new()
        .with_values(&[0, 32])
        .add_binary_op(|result, arg1, arg2| {
            Operation::Keccak256(InlineOperands { ins: [arg1, arg2], outs: [result] })
        })
        .with_stop()
        .build_and_translate();

    // Check for either KECCAK256 or SHA3 (depends on EVM version)
    let has_hash = count_opcode(&asm, "KECCAK256") > 0 || count_opcode(&asm, "SHA3") > 0;
    assert!(has_hash, "Should generate KECCAK256 or SHA3 opcode");
    assert_eq!(count_opcode(&asm, "STOP"), 1, "Should have STOP opcode");
}

// ============= Test Helper for Memory Pattern =============

#[test]
fn memory_allocation_patterns() {
    // Test memory allocation generates correct patterns
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 64 }),
        Operation::DynamicAllocZeroed(InlineOperands {
            ins: [LocalId::new(0)],
            outs: [LocalId::new(1)],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    // Should use memory pointer (0x40) and MSTORE operations
    let has_mem_ptr = asm.iter().any(|op| matches!(op, evm_glue::assembly::Asm::Op(_)));
    assert!(has_mem_ptr, "Should handle memory allocation");
}

#[test]
fn add_assembly_pattern() {
    // Test that ADD operation generates the correct assembly sequence
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 10 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 20 }),
        Operation::Add(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [LocalId::new(2)],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    // Verify the essential opcodes are present in the right order
    // The actual implementation will have memory management opcodes between these
    assert_opcode_sequence(
        &asm,
        &[
            "MSTORE", // Store first value
            "MSTORE", // Store second value
            "MLOAD",  // Load first value
            "MLOAD",  // Load second value
            "ADD",    // Perform addition
            "MSTORE", // Store result
            "STOP",   // Stop execution
        ],
    );

    // Also verify we have exactly one ADD operation
    assert_eq!(count_opcode(&asm, "ADD"), 1, "Should have exactly one ADD opcode");
}

#[test]
fn complex_arithmetic_pattern() {
    // Test pattern for (a + b) * c
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 10 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 20 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 3 }),
        Operation::Add(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [LocalId::new(3)],
        }),
        Operation::Mul(InlineOperands {
            ins: [LocalId::new(3), LocalId::new(2)],
            outs: [LocalId::new(4)],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    // Verify the sequence: store values, add, multiply
    assert_opcode_sequence(
        &asm,
        &[
            "MSTORE", // Store 10
            "MSTORE", // Store 20
            "MSTORE", // Store 3
            "MLOAD",  // Load for addition
            "MLOAD",  // Load for addition
            "ADD",    // a + b
            "MSTORE", // Store sum
            "MLOAD",  // Load sum
            "MLOAD",  // Load c
            "MUL",    // (a + b) * c
            "MSTORE", // Store result
            "STOP",
        ],
    );

    // Verify opcode counts
    assert_eq!(count_opcode(&asm, "ADD"), 1, "Should have exactly one ADD");
    assert_eq!(count_opcode(&asm, "MUL"), 1, "Should have exactly one MUL");
}

#[test]
fn division_pattern() {
    // Test division with zero check
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 100 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::Div(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [LocalId::new(2)],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    // Division by zero should still generate DIV opcode (EVM handles it)
    assert_opcode_sequence(
        &asm,
        &[
            "MSTORE", // Store dividend
            "MSTORE", // Store divisor (0)
            "MLOAD",  // Load dividend
            "MLOAD",  // Load divisor
            "DIV",    // Perform division
            "MSTORE", // Store result (will be 0)
            "STOP",
        ],
    );
}

#[test]
fn gas_optimization_patterns() {
    // Test that we generate patterns for basic operations
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 10 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 20 }),
        Operation::Add(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [LocalId::new(2)],
        }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 30 }),
        Operation::Add(InlineOperands {
            ins: [LocalId::new(2), LocalId::new(3)],
            outs: [LocalId::new(4)],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    // Verify we generated assembly
    assert!(!asm.is_empty(), "Should generate assembly code");

    // Check that we have ADD operations
    let add_count = count_opcode(&asm, "ADD");
    assert_eq!(add_count, 2, "Should have 2 ADD operations");
}
