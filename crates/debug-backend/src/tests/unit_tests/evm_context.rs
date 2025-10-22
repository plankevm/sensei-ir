//! Tests for EVM context and environmental operations

use crate::tests::helpers::{OperationTestBuilder, assert_opcode_counts, count_opcode};
use sir_data::{LocalId, Operation, operation::InlineOperands};

#[test]
fn environmental_info() {
    let asm = OperationTestBuilder::new()
        .add_zero_input_op(|result| {
            Operation::Timestamp(InlineOperands { ins: [], outs: [result] })
        })
        .add_zero_input_op(|result| Operation::Number(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| {
            Operation::Difficulty(InlineOperands { ins: [], outs: [result] })
        })
        .add_zero_input_op(|result| Operation::GasLimit(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| Operation::ChainId(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| {
            Operation::SelfBalance(InlineOperands { ins: [], outs: [result] })
        })
        .add_zero_input_op(|result| Operation::BaseFee(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| {
            Operation::BlobBaseFee(InlineOperands { ins: [], outs: [result] })
        })
        .with_stop()
        .build_and_translate();

    assert_opcode_counts(
        &asm,
        &[
            ("TIMESTAMP", 1),
            ("NUMBER", 1),
            ("PREVRANDAO", 1), // DIFFICULTY was replaced with PREVRANDAO after the merge
            ("GASLIMIT", 1),
            ("CHAINID", 1),
            ("SELFBALANCE", 1),
            ("BASEFEE", 1),
            ("BLOBBASEFEE", 1),
            ("STOP", 1),
        ],
    );
}

#[test]
fn blockchain_infos() {
    let asm = OperationTestBuilder::new()
        .with_values(&[0])
        .add_operation(|locals| {
            Operation::BlockHash(InlineOperands { ins: [locals[0]], outs: [LocalId::new(1)] })
        })
        .add_zero_input_op(|result| Operation::Coinbase(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| {
            Operation::Timestamp(InlineOperands { ins: [], outs: [result] })
        })
        .add_zero_input_op(|result| Operation::Number(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| {
            Operation::Difficulty(InlineOperands { ins: [], outs: [result] })
        })
        .add_zero_input_op(|result| Operation::GasLimit(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| Operation::ChainId(InlineOperands { ins: [], outs: [result] }))
        .with_stop()
        .build_and_translate();

    assert_opcode_counts(
        &asm,
        &[
            ("BLOCKHASH", 1),
            ("COINBASE", 1),
            ("TIMESTAMP", 1),
            ("NUMBER", 1),
            ("PREVRANDAO", 1), // DIFFICULTY was replaced with PREVRANDAO after the merge
            ("GASLIMIT", 1),
            ("CHAINID", 1),
            ("STOP", 1),
        ],
    );
}

#[test]
fn block_hashes() {
    let asm = OperationTestBuilder::new()
        .with_values(&[0, 1])
        .add_operation(|locals| {
            Operation::BlockHash(InlineOperands { ins: [locals[0]], outs: [LocalId::new(2)] })
        })
        .add_operation(|locals| {
            Operation::BlobHash(InlineOperands { ins: [locals[1]], outs: [LocalId::new(3)] })
        })
        .with_stop()
        .build_and_translate();

    assert_opcode_counts(&asm, &[("BLOCKHASH", 1), ("BLOBHASH", 1), ("STOP", 1)]);
}

#[test]
fn contexts() {
    let asm = OperationTestBuilder::new()
        .add_zero_input_op(|result| Operation::Caller(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| {
            Operation::CallValue(InlineOperands { ins: [], outs: [result] })
        })
        .add_zero_input_op(|result| Operation::Origin(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| Operation::Address(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| Operation::GasPrice(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| Operation::BaseFee(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| {
            Operation::Timestamp(InlineOperands { ins: [], outs: [result] })
        })
        .with_stop()
        .build_and_translate();

    let expected_opcodes = [
        ("CALLER", 1),
        ("CALLVALUE", 1),
        ("ORIGIN", 1),
        ("ADDRESS", 1),
        ("GASPRICE", 1),
        ("BASEFEE", 1),
        ("TIMESTAMP", 1),
        ("STOP", 1),
    ];

    assert_opcode_counts(&asm, &expected_opcodes);
}

#[test]
fn gass() {
    let asm = OperationTestBuilder::new()
        .add_zero_input_op(|result| Operation::Gas(InlineOperands { ins: [], outs: [result] }))
        .add_zero_input_op(|result| Operation::GasPrice(InlineOperands { ins: [], outs: [result] }))
        .with_stop()
        .build_and_translate();

    assert_opcode_counts(&asm, &[("GAS", 1), ("GASPRICE", 1), ("STOP", 1)]);
}

#[test]
fn blobhash() {
    let asm = OperationTestBuilder::new()
        .with_values(&[0])
        .add_operation(|locals| {
            Operation::BlobHash(InlineOperands { ins: [locals[0]], outs: [LocalId::new(1)] })
        })
        .with_stop()
        .build_and_translate();
    assert_eq!(count_opcode(&asm, "BLOBHASH"), 1, "Should generate BLOBHASH opcode");
}
