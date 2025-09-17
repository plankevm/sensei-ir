//! Tests for EVM context and environmental operations

use crate::tests::helpers::{OperationTestBuilder, assert_opcode_counts, count_opcode};
use eth_ir_data::{LocalId, Operation, operation::*};

#[test]
fn environmental_info() {
    let asm = OperationTestBuilder::new()
        .add_zero_input_op(|result| Operation::Timestamp(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::Number(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::Difficulty(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::GasLimit(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::ChainId(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::SelfBalance(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::BaseFee(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::BlobBaseFee(ZeroInOneOut { result }))
        .with_stop()
        .build_and_translate()
        .expect("Environmental info operations should translate");

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
            Operation::BlockHash(OneInOneOut { result: LocalId::new(1), arg1: locals[0] })
        })
        .add_zero_input_op(|result| Operation::Coinbase(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::Timestamp(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::Number(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::Difficulty(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::GasLimit(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::ChainId(ZeroInOneOut { result }))
        .with_stop()
        .build_and_translate()
        .expect("Blockchain info operations should translate");

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
            Operation::BlockHash(OneInOneOut { result: LocalId::new(2), arg1: locals[0] })
        })
        .add_operation(|locals| {
            Operation::BlobHash(OneInOneOut { result: LocalId::new(3), arg1: locals[1] })
        })
        .with_stop()
        .build_and_translate()
        .expect("Block hash operations should translate");

    assert_opcode_counts(&asm, &[("BLOCKHASH", 1), ("BLOBHASH", 1), ("STOP", 1)]);
}

#[test]
fn contexts() {
    let asm = OperationTestBuilder::new()
        .add_zero_input_op(|result| Operation::Caller(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::CallValue(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::Origin(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::Address(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::GasPrice(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::BaseFee(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::Timestamp(ZeroInOneOut { result }))
        .with_stop()
        .build_and_translate()
        .expect("Context operations should translate");

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
        .add_zero_input_op(|result| Operation::Gas(ZeroInOneOut { result }))
        .add_zero_input_op(|result| Operation::GasPrice(ZeroInOneOut { result }))
        .with_stop()
        .build_and_translate()
        .expect("Gas operations should translate");

    assert_opcode_counts(&asm, &[("GAS", 1), ("GASPRICE", 1), ("STOP", 1)]);
}

#[test]
fn blobhash() {
    let asm = OperationTestBuilder::new()
        .with_values(&[0])
        .add_operation(|locals| {
            Operation::BlobHash(OneInOneOut { result: LocalId::new(1), arg1: locals[0] })
        })
        .with_stop()
        .build_and_translate()
        .expect("Translation should succeed");
    assert_eq!(count_opcode(&asm, "BLOBHASH"), 1, "Should generate BLOBHASH opcode");
}
