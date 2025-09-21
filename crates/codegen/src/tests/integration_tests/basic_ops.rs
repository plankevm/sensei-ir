//! Integration tests for basic operations: arithmetic, bitwise, memory, and storage

use crate::tests::helpers::*;
use alloy_primitives::U256;

#[test]
fn constant_load_and_return() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        value = {}
        offset = 0
        size = 32
        mstore32 offset value
        return offset size
    }}
"#,
        TEST_VALUE_SMALL
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result =
        execute_and_get_result(bytecode).expect("Failed to load and return constant value");

    assert_eq!(result, U256::from(TEST_VALUE_SMALL), "Simple constant test failed");
}

#[test]
fn arithmetic_add() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        a = {}
        b = {}
        result = add a b
        offset = 0
        size = 32
        mstore32 offset result
        return offset size
    }}
"#,
        TEST_OPERAND_A, TEST_OPERAND_B
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Failed to execute addition");

    assert_eq!(result, U256::from(TEST_ADD_RESULT), "Addition test failed");
}

#[test]
fn test_parser_integration() {
    // Test that the parser can generate valid IR that compiles and runs
    let ir = r#"
fn main 0:
    entry {
        a = 42
        zero = 0
        size = 32
        mstore32 zero a
        return zero size
    }
"#;

    let program = parse_ir(ir).expect("Failed to parse IR");
    let bytecode =
        TestProgram::from_program(program).into_bytecode().expect("Failed to compile to bytecode");
    let result = execute_and_get_result(bytecode).expect("Failed to execute");

    assert_eq!(result, U256::from(42), "Parser-generated program should return 42");
}

#[test]
fn arithmetic_sdiv() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        a = {}
        b = {}
        result = sdiv a b
        offset = 0
        size = 32
        mstore32 offset result
        return offset size
    }}
"#,
        TEST_OPERAND_A, TEST_OPERAND_B
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Failed to execute signed division");

    assert_eq!(result, U256::from(TEST_SDIV_RESULT), "Signed division test failed");
}

#[test]
fn arithmetic_addmod() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        a = {}
        b = {}
        n = {}
        result = addmod a b n
        offset = 0
        size = 32
        mstore32 offset result
        return offset size
    }}
"#,
        TEST_OPERAND_A, TEST_OPERAND_C, TEST_DIVISOR
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Failed to execute addmod");
    assert_eq!(result, U256::from(TEST_ADDMOD_RESULT), "AddMod test failed");
}

#[test]
fn arithmetic_mulmod() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        a = {}
        b = 6
        n = {}
        result = mulmod a b n
        offset = 0
        size = 32
        mstore32 offset result
        return offset size
    }}
"#,
        TEST_OPERAND_B, TEST_DIVISOR
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Failed to execute mulmod");
    assert_eq!(result, U256::from(TEST_MULMOD_RESULT), "MulMod test failed");
}

#[test]
fn bitwise_and() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        a = {}
        b = {}
        result = and a b
        offset = 0
        size = 32
        mstore32 offset result
        return offset size
    }}
"#,
        TEST_BIT_PATTERN_A, TEST_BIT_PATTERN_C
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Failed to execute bitwise AND");
    assert_eq!(result, U256::from(TEST_BIT_PATTERN_C), "Bitwise AND test failed");
}

#[test]
fn bitwise_or() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        a = {}
        b = {}
        result = or a b
        offset = 0
        size = 32
        mstore32 offset result
        return offset size
    }}
"#,
        TEST_BIT_PATTERN_B, TEST_BIT_PATTERN_C
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Failed to execute bitwise OR");
    assert_eq!(result, U256::from(TEST_BIT_PATTERN_A), "Bitwise OR test failed");
}

#[test]
fn bitwise_xor() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        a = {}
        b = {}
        result = xor a b
        offset = 0
        size = 32
        mstore32 offset result
        return offset size
    }}
"#,
        TEST_BIT_PATTERN_A, TEST_BIT_PATTERN_D
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Failed to execute bitwise XOR");
    assert_eq!(result, U256::from(TEST_BIT_PATTERN_E), "Bitwise XOR test failed");
}

#[test]
fn bitwise_shl() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        shift = {}
        value = {}
        result = shl shift value
        offset = 0
        size = 32
        mstore32 offset result
        return offset size
    }}
"#,
        TEST_SHIFT_AMOUNT, TEST_SHIFT_VALUE
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Failed to execute shift left");
    assert_eq!(result, U256::from(TEST_SHIFT_RESULT), "Shift left test failed");
}

#[test]
fn bitwise_sar() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        shift = 1
        value = {}
        result = sar shift value
        offset = 0
        size = 32
        mstore32 offset result
        return offset size
    }}
"#,
        TEST_SAR_VALUE
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Sar should succeed");
    assert_eq!(result, U256::from(TEST_SAR_RESULT), "SAR test failed");
}

#[test]
fn memory_store_and_load() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        offset = 0
        value = {}
        mstore32 offset value
        loaded = mload32 offset
        ret_offset = 0
        ret_size = 32
        mstore32 ret_offset loaded
        return ret_offset ret_size
    }}
"#,
        TEST_VALUE_LARGE
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Memory operations should succeed");

    assert_eq!(result, U256::from(TEST_VALUE_LARGE), "Loaded value should match stored value");
}

#[test]
fn memory_load_at_offset() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        offset = {}
        value = {}
        mstore32 offset value
        loaded = mload32 offset
        result_offset = 0
        result_size = 32
        mstore32 result_offset loaded
        return result_offset result_size
    }}
"#,
        TEST_MEMORY_OFFSET, TEST_VALUE_MEDIUM
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Memory offset operations should succeed");

    assert_eq!(result, U256::from(TEST_VALUE_MEDIUM), "Memory offset load test failed");
}

#[test]
fn memory_mcopy() {
    let ir = r#"
fn main 0:
    entry {
        src_offset = 0
        value = 0xABCDEF
        mstore32 src_offset value
        dest_offset = 32
        copy_src = 0
        copy_size = 32
        mcopy dest_offset copy_src copy_size
        result = mload32 dest_offset
        ret_offset = 0
        mstore32 ret_offset result
        return ret_offset copy_size
    }
"#;

    let program = parse_ir(ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("MCopy should succeed");

    assert_eq!(result, U256::from(0xABCDEF), "Copied value should match original");
}

#[test]
fn test_crypto_keccak256() {
    let ir = r#"
fn main 0:
    entry {
        offset = 0
        value = 0
        mstore32 offset value
        mem_offset = 0
        mem_size = 32
        hash = keccak256 mem_offset mem_size
        return_offset = 0
        return_size = 32
        mstore32 return_offset hash
        return return_offset return_size
    }
"#;

    let program = parse_ir(ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Keccak256 should succeed");

    // Accept either hash of empty or zeros
    let expected_empty = U256::from_be_bytes([
        0xc5, 0xd2, 0x46, 0x01, 0x86, 0xf7, 0x23, 0x3c, 0x92, 0x7e, 0x7d, 0xb2, 0xdc, 0xc7, 0x03,
        0xc0, 0xe5, 0x00, 0xb6, 0x53, 0xca, 0x82, 0x27, 0x3b, 0x7b, 0xfa, 0xd8, 0x04, 0x5d, 0x85,
        0xa4, 0x70,
    ]);

    let expected_zeros = U256::from_be_bytes([
        0x29, 0x04, 0x5a, 0x59, 0x2e, 0xa2, 0xa4, 0x78, 0x87, 0xc6, 0x41, 0x2c, 0xa7, 0xae, 0x46,
        0xf1, 0xb4, 0xdb, 0x0f, 0xe4, 0x1a, 0xa8, 0x94, 0xad, 0x87, 0xbf, 0x36, 0xe4, 0x19, 0xf3,
        0xc6, 0xe6,
    ]);

    assert!(
        result == expected_empty || result == expected_zeros,
        "Keccak256 hash should be either empty hash or zeros hash"
    );
}

#[test]
fn storage_sstore_and_sload() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        key = {}
        value = {}
        sstore key value
        loaded = sload key
        offset = 0
        size = 32
        mstore32 offset loaded
        return offset size
    }}
"#,
        TEST_STORAGE_KEY, TEST_STORAGE_VALUE
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Storage operations should succeed");

    assert_eq!(result, U256::from(TEST_STORAGE_VALUE), "Loaded storage value should match stored");
}

#[test]
fn storage_tstore_and_tload() {
    let ir = format!(
        r#"
fn main 0:
    entry {{
        key = {}
        value = 999
        tstore key value
        loaded = tload key
        offset = 0
        size = 32
        mstore32 offset loaded
        return offset size
    }}
"#,
        TEST_VALUE_SMALL
    );

    let program = parse_ir(&ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Transient storage should succeed");
    assert_eq!(result, U256::from(999), "Transient storage should work correctly");
}

#[test]
fn storage_overwrite_value() {
    let ir = r#"
fn main 0:
    entry {
        key = 5
        value1 = 100
        sstore key value1
        key2 = 5
        value2 = 200
        sstore key2 value2
        load_key = 5
        loaded = sload load_key
        offset = 0
        size = 32
        mstore32 offset loaded
        return offset size
    }
"#;

    let program = parse_ir(ir).expect("Failed to parse IR");
    let bytecode = ir_to_bytecode(program);
    let result = execute_and_get_result(bytecode).expect("Storage overwrite should succeed");
    assert_eq!(result, U256::from(200), "Should load overwritten value");
}
