// Additional comprehensive tests for all operations

#[cfg(test)]
mod comprehensive_operation_tests {
    use crate::translate_program;
    use alloy_primitives::U256;
    use eth_ir_data::{index::*, operation::*, *};
    use evm_glue::{assembly::Asm, opcodes::Opcode};
    use std::str::FromStr;

    #[test]
    fn test_block_and_chain_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(15),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 1 }),
                Operation::BlockHash(OneInOneOut {
                    result: LocalId::new(0),
                    arg1: LocalId::new(1)
                }),
                Operation::Coinbase(ZeroInOneOut { result: LocalId::new(2) }),
                Operation::Timestamp(ZeroInOneOut { result: LocalId::new(3) }),
                Operation::Number(ZeroInOneOut { result: LocalId::new(4) }),
                Operation::Difficulty(ZeroInOneOut { result: LocalId::new(5) }),
                Operation::GasLimit(ZeroInOneOut { result: LocalId::new(7) }),
                Operation::ChainId(ZeroInOneOut { result: LocalId::new(8) }),
                Operation::BaseFee(ZeroInOneOut { result: LocalId::new(9) }),
                Operation::BlobBaseFee(ZeroInOneOut { result: LocalId::new(10) }),
                Operation::Gas(ZeroInOneOut { result: LocalId::new(11) }),
                Operation::GasPrice(ZeroInOneOut { result: LocalId::new(12) }),
                Operation::Origin(ZeroInOneOut { result: LocalId::new(13) }),
                Operation::SelfBalance(ZeroInOneOut { result: LocalId::new(14) }),
                Operation::Stop,
            ],
            locals: (0..15).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::BLOCKHASH))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::COINBASE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::TIMESTAMP))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::NUMBER))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::GASLIMIT))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CHAINID))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::BASEFEE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::BLOBBASEFEE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::GAS))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::GASPRICE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::ORIGIN))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SELFBALANCE))));
    }

    #[test]
    fn test_call_data_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(7),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 4 }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 32 }),
                Operation::CallDataSize(ZeroInOneOut { result: LocalId::new(3) }),
                Operation::CallDataLoad(OneInOneOut {
                    result: LocalId::new(4),
                    arg1: LocalId::new(0)
                }),
                Operation::CallDataCopy(ThreeInZeroOut {
                    arg1: LocalId::new(0), // dest offset
                    arg2: LocalId::new(1), // source offset
                    arg3: LocalId::new(2), // size
                }),
                Operation::Stop,
            ],
            locals: (0..5).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CALLDATASIZE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CALLDATALOAD))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CALLDATACOPY))));
    }

    #[test]
    fn test_advanced_arithmetic_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(11),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 10 }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 20 }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 7 }),
                Operation::AddMod(LargeInOneOut::<3> {
                    args_start: LocalIndex::new(1), // Uses locals 1-3
                    result: LocalId::new(0),
                }),
                Operation::MulMod(LargeInOneOut::<3> {
                    args_start: LocalIndex::new(1), // Uses locals 1-3
                    result: LocalId::new(4),
                }),
                Operation::Lt(TwoInOneOut {
                    result: LocalId::new(5),
                    arg1: LocalId::new(1),
                    arg2: LocalId::new(2),
                }),
                Operation::SLt(TwoInOneOut {
                    result: LocalId::new(6),
                    arg1: LocalId::new(1),
                    arg2: LocalId::new(2),
                }),
                Operation::SGt(TwoInOneOut {
                    result: LocalId::new(7),
                    arg1: LocalId::new(1),
                    arg2: LocalId::new(2),
                }),
                Operation::SignExtend(TwoInOneOut {
                    result: LocalId::new(10),
                    arg1: LocalId::new(1),
                    arg2: LocalId::new(2),
                }),
                Operation::Sar(TwoInOneOut {
                    result: LocalId::new(11),
                    arg1: LocalId::new(2),
                    arg2: LocalId::new(1),
                }),
                Operation::Stop,
            ],
            locals: (0..12).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::ADDMOD))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MULMOD))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::LT))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SLT))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SGT))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SIGNEXTEND))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SAR))));
    }

    #[test]
    fn test_code_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(11),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value: 0x100
                }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 0 }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 32 }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(5),
                    cid: LargeConstId::new(0)
                }),
                Operation::CodeSize(ZeroInOneOut { result: LocalId::new(0) }),
                Operation::CodeCopy(ThreeInZeroOut {
                    arg1: LocalId::new(1), // dest
                    arg2: LocalId::new(2), // offset
                    arg3: LocalId::new(3), // size
                }),
                Operation::ExtCodeSize(OneInOneOut {
                    result: LocalId::new(4),
                    arg1: LocalId::new(5)
                }),
                Operation::ExtCodeCopy(LargeInZeroOut::<4> {
                    args_start: LocalIndex::new(1), // Uses locals 1-4 (address, dest, offset, size)
                }),
                Operation::ExtCodeHash(OneInOneOut {
                    result: LocalId::new(6),
                    arg1: LocalId::new(5)
                }),
                Operation::Byte(TwoInOneOut {
                    result: LocalId::new(7),
                    arg1: LocalId::new(1),
                    arg2: LocalId::new(2),
                }),
                Operation::Stop,
            ],
            locals: (0..8).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![U256::from(0x1234567890abcdef_u64)],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CODESIZE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CODECOPY))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::EXTCODESIZE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::EXTCODECOPY))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::EXTCODEHASH))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::BYTE))));
    }

    #[test]
    fn test_logging_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(12),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: 0x80
                }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 32 }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(2),
                    cid: LargeConstId::new(0)
                }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(3),
                    cid: LargeConstId::new(1)
                }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(4),
                    cid: LargeConstId::new(2)
                }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(5),
                    cid: LargeConstId::new(3)
                }),
                Operation::Log0(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
                Operation::Log1(ThreeInZeroOut {
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                    arg3: LocalId::new(2),
                }),
                Operation::Log2(LargeInZeroOut::<4> {
                    args_start: LocalIndex::new(0), // Uses locals 0-3
                }),
                Operation::Log3(LargeInZeroOut::<5> {
                    args_start: LocalIndex::new(0), // Uses locals 0-4
                }),
                Operation::Log4(LargeInZeroOut::<6> {
                    args_start: LocalIndex::new(0), // Uses locals 0-5
                }),
                Operation::Stop,
            ],
            locals: (0..6).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![
                U256::from(0xaaa_u64),
                U256::from(0xbbb_u64),
                U256::from(0xccc_u64),
                U256::from(0xddd_u64),
            ],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::LOG0))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::LOG1))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::LOG2))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::LOG3))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::LOG4))));
    }

    #[test]
    fn test_remaining_memory_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(12),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value: 0x100
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(3),
                    value: 0x200
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(4),
                    value: 0x100
                }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(5), value: 32 }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(7),
                    cid: LargeConstId::new(0)
                }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(8),
                    cid: LargeConstId::new(1)
                }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(11), value: 64 }),
                Operation::MemoryLoad(MemoryLoad {
                    result: LocalId::new(0),
                    address: LocalId::new(1),
                    byte_size: 32
                }),
                Operation::MCopy(ThreeInZeroOut {
                    arg1: LocalId::new(3), // dest
                    arg2: LocalId::new(4), // source
                    arg3: LocalId::new(5), // size
                }),
                Operation::TLoad(OneInOneOut { result: LocalId::new(6), arg1: LocalId::new(7) }),
                Operation::TStore(TwoInZeroOut { arg1: LocalId::new(7), arg2: LocalId::new(8) }),
                Operation::LocalAllocZeroed(OneInOneOut {
                    result: LocalId::new(10),
                    arg1: LocalId::new(11)
                }),
                Operation::Stop,
            ],
            locals: (0..12).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![U256::from(0x111_u64), U256::from(0x222_u64)],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MLOAD))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MCOPY))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::TLOAD))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::TSTORE))));
    }

    #[test]
    fn test_create_and_selfdestruct_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(8),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(2),
                    value: 0x100
                }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 32 }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(5),
                    cid: LargeConstId::new(0)
                }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(6),
                    cid: LargeConstId::new(1)
                }),
                Operation::Create(LargeInOneOut::<3> {
                    args_start: LocalIndex::new(1), // Uses locals 1-3 (value, offset, size)
                    result: LocalId::new(0),
                }),
                Operation::Create2(LargeInOneOut::<4> {
                    args_start: LocalIndex::new(1), // Uses locals 1-4 (value, offset, size, salt)
                    result: LocalId::new(4),
                }),
                Operation::SelfDestruct(OneInZeroOut { arg1: LocalId::new(6) }),
            ],
            locals: (0..7).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![U256::from(0x5a17_u64), U256::from(0xbef1_u64)],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CREATE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CREATE2))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SELFDESTRUCT))));
    }

    #[test]
    fn test_revert_operation() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(3),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: 0x80
                }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 32 }),
                Operation::Revert(TwoInZeroOut {
                    arg1: LocalId::new(0), // offset
                    arg2: LocalId::new(1), // size
                }),
            ],
            locals: index_vec![LocalId::new(0), LocalId::new(1)],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::REVERT))));
    }

    #[test]
    fn test_basic_arithmetic_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(9),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 20 }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 5 }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(7), value: 3 }),
                Operation::Sub(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::Mul(TwoInOneOut {
                    result: LocalId::new(3),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::Div(TwoInOneOut {
                    result: LocalId::new(4),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::Mod(TwoInOneOut {
                    result: LocalId::new(5),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::Exp(TwoInOneOut {
                    result: LocalId::new(6),
                    arg1: LocalId::new(1),
                    arg2: LocalId::new(7),
                }),
                Operation::Stop,
            ],
            locals: (0..8).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SUB))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MUL))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::DIV))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MOD))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::EXP))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::STOP))));
    }

    #[test]
    fn test_basic_bitwise_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(8),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: 0b1100
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value: 0b1010
                }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(6), value: 2 }),
                Operation::And(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::Or(TwoInOneOut {
                    result: LocalId::new(3),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::Xor(TwoInOneOut {
                    result: LocalId::new(4),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::Shl(TwoInOneOut {
                    result: LocalId::new(5),
                    arg1: LocalId::new(6),
                    arg2: LocalId::new(0),
                }),
                Operation::Stop,
            ],
            locals: (0..7).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::AND))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::OR))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::XOR))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SHL))));
    }

    #[test]
    fn test_basic_comparison_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(7),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 10 }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 20 }),
                Operation::Gt(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::Eq(TwoInOneOut {
                    result: LocalId::new(3),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                Operation::IsZero(OneInOneOut { result: LocalId::new(4), arg1: LocalId::new(2) }),
                Operation::Not(OneInOneOut { result: LocalId::new(5), arg1: LocalId::new(2) }),
                Operation::Stop,
            ],
            locals: (0..6).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::GT))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::EQ))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::ISZERO))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::NOT))));
    }

    #[test]
    fn test_basic_environmental_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(6),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::Address(ZeroInOneOut { result: LocalId::new(0) }),
                Operation::Balance(OneInOneOut { result: LocalId::new(1), arg1: LocalId::new(0) }),
                Operation::Caller(ZeroInOneOut { result: LocalId::new(2) }),
                Operation::CallValue(ZeroInOneOut { result: LocalId::new(3) }),
                Operation::Origin(ZeroInOneOut { result: LocalId::new(4) }),
                Operation::Stop,
            ],
            locals: (0..5).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::ADDRESS))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::BALANCE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CALLER))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CALLVALUE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::ORIGIN))));
    }

    #[test]
    fn test_basic_storage_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(5),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 42 }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 123 }),
                Operation::SStore(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
                Operation::SLoad(OneInOneOut { result: LocalId::new(2), arg1: LocalId::new(0) }),
                Operation::Stop,
            ],
            locals: (0..3).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SSTORE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SLOAD))));
    }

    #[test]
    fn test_large_constants_operation() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(3),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(0),
                    cid: LargeConstId::new(0),
                }),
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(1),
                    cid: LargeConstId::new(1),
                }),
                Operation::Stop,
            ],
            locals: index_vec![LocalId::new(0), LocalId::new(1)],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![
                U256::from_str("0xdAC17F958D2ee523a2206206994597C13D831ec7").unwrap(),
                U256::MAX,
            ],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        use evm_glue::{assembly::Asm, opcodes::Opcode};

        // Verify large constants are properly loaded
        let mut found_push32_count = 0;
        let mut found_mstore_count = 0;

        for instruction in &asm {
            match instruction {
                Asm::Op(Opcode::PUSH32(_)) => found_push32_count += 1,
                Asm::Op(Opcode::MSTORE) => found_mstore_count += 1,
                _ => {}
            }
        }

        // Should push exactly 2 large constants and store them, plus the free memory pointer
        // initialization
        assert_eq!(found_push32_count, 2, "Should have exactly 2 PUSH32 for the 2 large constants");
        assert_eq!(
            found_mstore_count, 3,
            "Should have exactly 3 MSTOREs (free ptr + 2 large constants)"
        );
    }

    #[test]
    fn test_acquire_free_pointer_operation() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(2),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::AcquireFreePointer(ZeroInOneOut { result: LocalId::new(0) }),
                Operation::Stop,
            ],
            locals: index_vec![LocalId::new(0)],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        // Check that we load free memory pointer from 0x40
        let has_free_ptr_ops = asm.iter().any(|op| matches!(op, Asm::Op(Opcode::PUSH1([0x40]))));
        assert!(has_free_ptr_ops, "Should use free memory pointer at 0x40");

        let has_mload = asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MLOAD)));
        assert!(has_mload, "Should load free memory pointer");
    }

    #[test]
    fn test_memory_allocation_operations() {
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(4),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 64 }),
                Operation::DynamicAllocAnyBytes(OneInOneOut {
                    arg1: LocalId::new(0),
                    result: LocalId::new(1),
                }),
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 42 }),
                Operation::MemoryStore(MemoryStore {
                    address: LocalId::new(1),
                    value: LocalId::new(2),
                    byte_size: 32,
                }),
                Operation::Stop,
            ],
            locals: (0..3).map(|i| LocalId::new(i as u32)).collect(),
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program).expect("Translation should succeed");

        let has_mload = asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MLOAD)));
        let has_mstore = asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MSTORE)));
        assert!(has_mload && has_mstore, "Should load and store free memory pointer");
    }
}
