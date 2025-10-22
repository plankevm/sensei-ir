//! Integration tests for basic operations: arithmetic, bitwise, memory, and storage

use crate::tests::helpers::*;
use alloy_primitives::U256;

#[test]
fn constant_load_and_return() {
    let ir = format!(
        r#"
fn init:
    entry {{
        value = const {}
        offset = const 0
        size = const 32
        mstore256 offset value
        return offset size
    }}
"#,
        TEST_VALUE_SMALL
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Failed to load and return constant value");

    assert_eq!(result, U256::from(TEST_VALUE_SMALL), "Simple constant test failed");
}

#[test]
fn arithmetic_add() {
    let ir = format!(
        r#"
fn init:
    entry {{
        a = const {}
        b = const {}
        result = add a b
        offset = const 0
        size = const 32
        mstore256 offset result
        return offset size
    }}
"#,
        TEST_OPERAND_A, TEST_OPERAND_B
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Failed to execute addition");

    assert_eq!(result, U256::from(TEST_ADD_RESULT), "Addition test failed");
}

#[test]
fn test_parser_integration() {
    // Test that the parser can generate valid IR that compiles and runs
    let ir = r#"
fn init:
    entry {
        a = const 42
        zero = const 0
        size = const 32
        mstore256 zero a
        return zero size
    }
"#;

    let program = checked_parse(ir);
    let bytecode =
        TestProgram::from_program(program).into_bytecode().expect("Failed to compile to bytecode");
    let result = execute_bytecode(bytecode).expect("Failed to execute");

    assert_eq!(result, U256::from(42), "Parser-generated program should return 42");
}

#[test]
fn arithmetic_sdiv() {
    let ir = format!(
        r#"
fn init:
    entry {{
        a = const {}
        b = const {}
        result = sdiv a b
        offset = const 0
        size = const 32
        mstore256 offset result
        return offset size
    }}
"#,
        TEST_OPERAND_A, TEST_OPERAND_B
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Failed to execute signed division");

    assert_eq!(result, U256::from(TEST_SDIV_RESULT), "Signed division test failed");
}

#[test]
fn arithmetic_addmod() {
    let ir = format!(
        r#"
fn init:
    entry {{
        a = const {}
        b = const {}
        n = const {}
        result = addmod a b n
        offset = const 0
        size = const 32
        mstore256 offset result
        return offset size
    }}
"#,
        TEST_OPERAND_A, TEST_OPERAND_C, TEST_DIVISOR
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Failed to execute addmod");
    assert_eq!(result, U256::from(TEST_ADDMOD_RESULT), "AddMod test failed");
}

#[test]
fn arithmetic_mulmod() {
    let ir = format!(
        r#"
fn init:
    entry {{
        a = const {}
        b = const 6
        n = const {}
        result = mulmod a b n
        offset = const 0
        size = const 32
        mstore256 offset result
        return offset size
    }}
"#,
        TEST_OPERAND_B, TEST_DIVISOR
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Failed to execute mulmod");
    assert_eq!(result, U256::from(TEST_MULMOD_RESULT), "MulMod test failed");
}

#[test]
fn bitwise_and() {
    let ir = format!(
        r#"
fn init:
    entry {{
        a = const {}
        b = const {}
        result = and a b
        offset = const 0
        size = const 32
        mstore256 offset result
        return offset size
    }}
"#,
        TEST_BIT_PATTERN_A, TEST_BIT_PATTERN_C
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Failed to execute bitwise AND");
    assert_eq!(result, U256::from(TEST_BIT_PATTERN_C), "Bitwise AND test failed");
}

#[test]
fn bitwise_or() {
    let ir = format!(
        r#"
fn init:
    entry {{
        a = const {}
        b = const {}
        result = or a b
        offset = const 0
        size = const 32
        mstore256 offset result
        return offset size
    }}
"#,
        TEST_BIT_PATTERN_B, TEST_BIT_PATTERN_C
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Failed to execute bitwise OR");
    assert_eq!(result, U256::from(TEST_BIT_PATTERN_A), "Bitwise OR test failed");
}

#[test]
fn bitwise_xor() {
    let ir = format!(
        r#"
fn init:
    entry {{
        a = const {}
        b = const {}
        result = xor a b
        offset = const 0
        size = const 32
        mstore256 offset result
        return offset size
    }}
"#,
        TEST_BIT_PATTERN_A, TEST_BIT_PATTERN_D
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Failed to execute bitwise XOR");
    assert_eq!(result, U256::from(TEST_BIT_PATTERN_E), "Bitwise XOR test failed");
}

#[test]
fn bitwise_shl() {
    let ir = format!(
        r#"
fn init:
    entry {{
        shift = const {}
        value = const {}
        result = shl shift value
        offset = const 0
        size = const 32
        mstore256 offset result
        return offset size
    }}
"#,
        TEST_SHIFT_AMOUNT, TEST_SHIFT_VALUE
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Failed to execute shift left");
    assert_eq!(result, U256::from(TEST_SHIFT_RESULT), "Shift left test failed");
}

#[test]
fn bitwise_sar() {
    let ir = format!(
        r#"
fn init:
    entry {{
        shift = const 1
        value = const {}
        result = sar shift value
        offset = const 0
        size = const 32
        mstore256 offset result
        return offset size
    }}
"#,
        TEST_SAR_VALUE
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Sar should succeed");
    assert_eq!(result, U256::from(TEST_SAR_RESULT), "SAR test failed");
}

#[test]
fn memory_store_and_load() {
    let ir = format!(
        r#"
fn init:
    entry {{
        offset = const 0
        value = const {}
        mstore256 offset value
        loaded = mload256 offset
        ret_offset = const 0
        ret_size = const 32
        mstore256 ret_offset loaded
        return ret_offset ret_size
    }}
"#,
        TEST_VALUE_LARGE
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Memory operations should succeed");

    assert_eq!(result, U256::from(TEST_VALUE_LARGE), "Loaded value should match stored value");
}

#[test]
fn memory_load_at_offset() {
    let ir = format!(
        r#"
fn init:
    entry {{
        offset = const {}
        value = const {}
        mstore256 offset value
        loaded = mload256 offset
        result_offset = const 0
        result_size = const 32
        mstore256 result_offset loaded
        return result_offset result_size
    }}
"#,
        TEST_MEMORY_OFFSET, TEST_VALUE_MEDIUM
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Memory offset operations should succeed");

    assert_eq!(result, U256::from(TEST_VALUE_MEDIUM), "Memory offset load test failed");
}

#[test]
fn memory_mcopy() {
    let ir = r#"
fn init:
    entry {
        src_offset = const 0
        value = const 0xABCDEF
        mstore256 src_offset value
        dest_offset = const 32
        copy_src = const 0
        copy_size = const 32
        mcopy dest_offset copy_src copy_size
        result = mload256 dest_offset
        ret_offset = const 0
        mstore256 ret_offset result
        return ret_offset copy_size
    }
"#;

    let program = checked_parse(ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("MCopy should succeed");

    assert_eq!(result, U256::from(0xABCDEF), "Copied value should match original");
}

#[test]
fn test_crypto_keccak256() {
    let ir = r#"
fn init:
    entry {
        offset = const 0
        value = const 0
        mstore256 offset value
        mem_offset = const 0
        mem_size = const 32
        hash = keccak256 mem_offset mem_size
        return_offset = const 0
        return_size = const 32
        mstore256 return_offset hash
        return return_offset return_size
    }
"#;

    let program = checked_parse(ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Keccak256 should succeed");

    // Compute expected hash: keccak256 of 32 zero bytes
    let zero_data = [0u8; 32];
    let expected = revm::primitives::keccak256(&zero_data);
    let expected_u256 = U256::from_be_slice(expected.as_slice());

    assert_eq!(result, expected_u256, "Keccak256 hash of 32 zero bytes should match");
}

#[test]
fn storage_sstore_and_sload() {
    let ir = format!(
        r#"
fn init:
    entry {{
        key = const {}
        value = const {}
        sstore key value
        loaded = sload key
        offset = const 0
        size = const 32
        mstore256 offset loaded
        return offset size
    }}
"#,
        TEST_STORAGE_KEY, TEST_STORAGE_VALUE
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Storage operations should succeed");

    assert_eq!(result, U256::from(TEST_STORAGE_VALUE), "Loaded storage value should match stored");
}

#[test]
fn storage_tstore_and_tload() {
    let ir = format!(
        r#"
fn init:
    entry {{
        key = const {}
        value = const 999
        tstore key value
        loaded = tload key
        offset = const 0
        size = const 32
        mstore256 offset loaded
        return offset size
    }}
"#,
        TEST_VALUE_SMALL
    );

    let program = checked_parse(&ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Transient storage should succeed");
    assert_eq!(result, U256::from(999), "Transient storage should work correctly");
}

#[test]
fn storage_overwrite_value() {
    let ir = r#"
fn init:
    entry {
        key = const 5
        value1 = const 100
        sstore key value1
        key2 = const 5
        value2 = const 200
        sstore key2 value2
        load_key = const 5
        loaded = sload load_key
        offset = const 0
        size = const 32
        mstore256 offset loaded
        return offset size
    }
"#;

    let program = checked_parse(ir);
    let bytecode = ir_to_bytecode(program);
    let result = execute_bytecode(bytecode).expect("Storage overwrite should succeed");
    assert_eq!(result, U256::from(200), "Should load overwritten value");
}
