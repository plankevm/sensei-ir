//! Helper utilities for property tests

use crate::tests::helpers::*;
use alloy_primitives::U256;
use revm::{
    Evm, InMemoryDB,
    primitives::{AccountInfo, Bytecode, ExecutionResult, TransactTo, address},
};
use sir_data::{EthIRProgram, LocalId, Operation, operation::*};

// Note: execute_and_get_result is re-exported from helpers/builders.rs

/// Execute bytecode with limited gas
pub fn execute_with_gas_limit(
    bytecode: Vec<u8>,
    gas_limit: u64,
) -> Result<ExecutionResult, String> {
    let mut db = InMemoryDB::default();
    let test_address = address!("0000000000000000000000000000000000000123");
    let caller_address = address!("0000000000000000000000000000000000000456");

    db.insert_account_info(
        test_address,
        AccountInfo {
            balance: U256::from(1_000_000_000_000_000_000u64),
            nonce: 0,
            code_hash: revm::primitives::keccak256(&bytecode),
            code: Some(Bytecode::new_raw(bytecode.clone().into())),
        },
    );

    db.insert_account_info(
        caller_address,
        AccountInfo {
            balance: U256::from(1_000_000_000_000_000_000u64),
            nonce: 0,
            code_hash: revm::primitives::KECCAK_EMPTY,
            code: None,
        },
    );

    let mut evm = revm::Evm::builder()
        .with_db(db)
        .modify_tx_env(|tx| {
            tx.caller = caller_address;
            tx.transact_to = TransactTo::Call(test_address);
            tx.data = vec![].into();
            tx.gas_limit = gas_limit;
            tx.gas_price = U256::from(1);
            tx.value = U256::ZERO;
        })
        .build();

    evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))
}

pub fn create_return_ops(result_local: u32) -> Vec<Operation> {
    create_return_for_local(result_local, result_local + 1, result_local + 2)
}

pub fn calculate_safe_memory_offset(program: &EthIRProgram) -> u32 {
    let num_locals = program.locals.len() as u32;
    // Safe offset = LOCALS_START + (num_locals * SLOT_SIZE) + SLOT_SIZE (for free memory pointer)
    constants::LOCALS_START + (num_locals * constants::SLOT_SIZE) + constants::SLOT_SIZE
}

pub fn build_binary_op_test<F>(a: u32, b: u32, create_op: F) -> Vec<Operation>
where
    F: FnOnce(LocalId, LocalId, LocalId) -> Operation,
{
    build_binary_op_with_return(a, b, create_op)
}

pub fn build_unary_op_test<F>(value: u32, create_op: F) -> Vec<Operation>
where
    F: FnOnce(LocalId, LocalId) -> Operation,
{
    let mut ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value }),
        create_op(LocalId::new(1), LocalId::new(0)),
    ];
    ops.extend(create_return_ops(1));
    ops
}

pub fn create_evm_with_bytecode(
    bytecode: Vec<u8>,
    calldata: Vec<u8>,
) -> Evm<'static, (), InMemoryDB> {
    EvmBuilder::new()
        .with_bytecode(bytecode)
        .with_calldata(calldata)
        .with_gas_limit(1_000_000)
        .build()
}
