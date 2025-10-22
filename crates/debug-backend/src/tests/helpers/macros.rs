//! Improved test framework using hybrid macro/function approach

use super::*;
use alloy_primitives::U256;
use sir_data::{LocalId, Operation};

/// Core implementation for binary operation tests
pub fn test_binary_op_impl<F>(
    test_cases: &[(u32, u32)],
    create_op: F,
    expected_fn: impl Fn(U256, U256) -> U256,
    opcode_name: &str,
) where
    F: Fn(LocalId, LocalId, LocalId) -> Operation + Clone,
{
    // Test with provided test cases
    for &(a, b) in test_cases {
        let result = OperationTestBuilder::new()
            .with_values(&[a, b])
            .add_binary_op(create_op.clone())
            .build_and_execute(2)
            .unwrap_or_else(|e| {
                panic!("Failed to execute {} with ({}, {}): {}", opcode_name, a, b, e)
            });

        let expected = expected_fn(U256::from(a), U256::from(b));
        assert_eq!(
            result, expected,
            "{} operation failed: {} op {} != {}",
            opcode_name, a, b, expected
        );
    }

    // Verify opcode generation
    let asm = OperationTestBuilder::new()
        .with_values(&[TEST_OPERAND_A, TEST_OPERAND_C])
        .add_binary_op(create_op)
        .with_stop()
        .build_and_translate();

    assert_opcode_counts(&asm, &[(opcode_name, 1), ("STOP", 1)]);
}

/// Core implementation for unary operation tests
pub fn test_unary_op_impl<F>(
    test_cases: &[u32],
    create_op: F,
    expected_fn: impl Fn(U256) -> U256,
    opcode_name: &str,
) where
    F: Fn(LocalId, LocalId) -> Operation + Clone,
{
    // Test with provided test cases
    for &value in test_cases {
        let result = OperationTestBuilder::new()
            .with_values(&[value])
            .add_unary_op(create_op.clone())
            .build_and_execute(1)
            .unwrap_or_else(|e| panic!("Failed to execute {} with {}: {}", opcode_name, value, e));

        let expected = expected_fn(U256::from(value));
        assert_eq!(
            result, expected,
            "{} operation failed: op({}) != {}",
            opcode_name, value, expected
        );
    }

    // Verify opcode generation
    let asm = OperationTestBuilder::new()
        .with_values(&[TEST_VALUE_SMALL])
        .add_unary_op(create_op)
        .with_stop()
        .build_and_translate();

    assert_opcode_counts(&asm, &[(opcode_name, 1), ("STOP", 1)]);
}

/// Macro to define a binary operation test with minimal boilerplate
#[macro_export]
macro_rules! define_binary_op_test {
    ($test_name:ident, $op_constructor:expr, $expected_fn:expr, $opcode:literal) => {
        #[test]
        fn $test_name() {
            $crate::tests::helpers::macros::test_binary_op_impl(
                &$crate::tests::helpers::BINARY_OP_TEST_CASES,
                $op_constructor,
                $expected_fn,
                $opcode,
            );
        }
    };
    // Variant with custom test cases
    (
        $test_name:ident,
        $test_cases:expr,
        $op_constructor:expr,
        $expected_fn:expr,
        $opcode:literal
    ) => {
        #[test]
        fn $test_name() {
            $crate::tests::helpers::macros::test_binary_op_impl(
                $test_cases,
                $op_constructor,
                $expected_fn,
                $opcode,
            );
        }
    };
}

/// Macro to define a unary operation test with minimal boilerplate
#[macro_export]
macro_rules! define_unary_op_test {
    ($test_name:ident, $op_constructor:expr, $expected_fn:expr, $opcode:literal) => {
        #[test]
        fn $test_name() {
            $crate::tests::helpers::macros::test_unary_op_impl(
                &$crate::tests::helpers::UNARY_OP_TEST_CASES,
                $op_constructor,
                $expected_fn,
                $opcode,
            );
        }
    };
    // Variant with custom test cases
    (
        $test_name:ident,
        $test_cases:expr,
        $op_constructor:expr,
        $expected_fn:expr,
        $opcode:literal
    ) => {
        #[test]
        fn $test_name() {
            $crate::tests::helpers::macros::test_unary_op_impl(
                $test_cases,
                $op_constructor,
                $expected_fn,
                $opcode,
            );
        }
    };
}

/// Macro to define a signed binary operation test
#[macro_export]
macro_rules! define_signed_op_test {
    ($test_name:ident, $op_constructor:expr, $expected_fn:expr, $opcode:literal) => {
        $crate::define_binary_op_test!(
            $test_name,
            &$crate::tests::helpers::SIGNED_OP_TEST_CASES,
            $op_constructor,
            $expected_fn,
            $opcode
        );
    };
}
