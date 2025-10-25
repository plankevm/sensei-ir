use crate::translate_program;
use alloy_primitives::{Address, Bytes, U256, address, hex};
use proptest::prelude::*;
use trevm::{
    NoopBlock, NoopCfg, TrevmBuilder,
    revm::{
        context::{result::ExecutionResult, tx::TxEnvBuilder},
        database::InMemoryDB,
        state::{AccountInfo, Bytecode},
    },
    trevm_aliases,
};

use sir_parser::{EmitConfig, parse_or_panic};

fn ir_to_bytecode(source: &str, config: EmitConfig) -> Vec<u8> {
    let ir = parse_or_panic(source, config);
    let asm = translate_program(ir);
    let (_, bytecode) =
        evm_glue::assemble_minimized(&asm, true).expect("valid asm from translate_program");
    bytecode
}

trevm_aliases!(InMemoryDB);

const TEST_BYTECODE_RUNNER: Address = address!("0x000000000000000000000000b1b1b1b1b1b1b1b1");

fn arb_u256() -> impl Strategy<Value = U256> {
    prop_oneof![
        // Explicit edge cases
        Just(U256::ZERO),
        Just(U256::MAX),
        any::<u8>().prop_map(|x| U256::ONE << x),
        any::<U256>()
    ]
}

fn test_simple_add_inner(a: U256, b: U256) {
    let bytecode = ir_to_bytecode(
        "
        fn init:
            init_entry {
                c0 = const 0
                c32 = const 32
                a = calldataload c0
                b = calldataload c32
                result = add a b
                no_gud = gt a result
                => no_gud ? @error : @gud
            }
            error {
                revert c0 c0
            }
            gud {
                buf = sallocany 32
                mstore256 buf result
                return buf c32
            }
        ",
        EmitConfig::init_only(),
    );

    let mut db = InMemoryDB::default();
    let account_info = AccountInfo::default().with_code(Bytecode::new_legacy(bytecode.into()));
    db.insert_account_info(TEST_BYTECODE_RUNNER, account_info);

    let mut contract_input = [0u8; 64];
    contract_input[0..32].copy_from_slice(&a.to_be_bytes::<32>());
    contract_input[32..64].copy_from_slice(&b.to_be_bytes::<32>());

    let tx = TxEnvBuilder::new().to(TEST_BYTECODE_RUNNER).data(contract_input.into()).build_fill();

    let evm =
        TrevmBuilder::new().with_db(db).build_trevm().fill_cfg(&NoopCfg).fill_block(&NoopBlock);

    let (res, _evm) = evm.run_tx(&tx).expect("no error").accept();
    let (succ, ret_data) = match res {
        ExecutionResult::Success { output, .. } => (true, output.into_data()),
        ExecutionResult::Revert { output, .. } => (false, output),
        ExecutionResult::Halt { .. } => (false, Bytes::new()),
    };

    match a.checked_add(b) {
        None => {
            assert!(!succ, "call succeeded unexpectedly with data: 0x{}", hex::encode(ret_data));
            assert_eq!(ret_data.len(), 0);
        }
        Some(out) => {
            assert!(succ, "call failed unexpectedly with data: 0x{}", hex::encode(ret_data));
            assert_eq!(ret_data.len(), 32);
            assert_eq!(out, a + b);
        }
    };
}

proptest! {
    #[test]
    fn test_simple_add(a in arb_u256(), b in arb_u256()) {
        test_simple_add_inner(a, b);
    }

}
