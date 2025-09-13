//! Tests for operation translation

#[cfg(test)]
mod tests {
    use crate::{test_helpers::*, translate_program};
    use alloy_primitives::U256;
    use eth_ir_data::{Branch, Control, LocalId, Switch, index::*, operation::*};
    use evm_glue::assembly::Asm;

    // Test constants to avoid magic numbers
    const TEST_VALUE_SMALL: u64 = 42;
    const BOUNDARY_ADDRESS: u64 = 0x1000000; // 16MB boundary

    // ==================== Arithmetic Operations ====================

    #[test]
    fn test_arithmetic_operations() {
        // Test basic and advanced arithmetic operations
        let mut operations = arithmetic_operations();
        // Remove the Stop at the end so we can add more operations
        operations.pop();

        // Add advanced arithmetic operations
        operations.extend([
            Operation::Mod(TwoInOneOut {
                result: LocalId::new(6),
                arg1: LocalId::new(1),
                arg2: LocalId::new(0),
            }),
            Operation::Exp(TwoInOneOut {
                result: LocalId::new(7),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::SDiv(TwoInOneOut {
                result: LocalId::new(8),
                arg1: LocalId::new(1),
                arg2: LocalId::new(0),
            }),
            Operation::SMod(TwoInOneOut {
                result: LocalId::new(9),
                arg1: LocalId::new(1),
                arg2: LocalId::new(0),
            }),
            Operation::AddMod(LargeInOneOut {
                result: LocalId::new(10),
                args_start: LocalIndex::new(0),
            }),
            Operation::MulMod(LargeInOneOut {
                result: LocalId::new(11),
                args_start: LocalIndex::new(0),
            }),
            Operation::SignExtend(TwoInOneOut {
                result: LocalId::new(12),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::Stop,
        ]);

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("Translation should succeed");

        // Verify arithmetic opcodes
        assert_opcode_counts(
            &asm,
            &[
                ("ADD", 1),
                ("SUB", 1),
                ("MUL", 1),
                ("DIV", 1),
                ("MOD", 1),
                ("EXP", 1),
                ("SDIV", 1),
                ("SMOD", 1),
                ("ADDMOD", 1),
                ("MULMOD", 1),
                ("SIGNEXTEND", 1),
                ("STOP", 1),
            ],
        );
    }

    // ==================== Bitwise Operations ====================

    #[test]
    fn test_bitwise_operations() {
        let operations = [
            // Set up initial values
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0xFF }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0x0F }),
            // Bitwise operations
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
            Operation::Not(OneInOneOut { result: LocalId::new(5), arg1: LocalId::new(0) }),
            Operation::Byte(TwoInOneOut {
                result: LocalId::new(6),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::Shl(TwoInOneOut {
                result: LocalId::new(7),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::Shr(TwoInOneOut {
                result: LocalId::new(8),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::Sar(TwoInOneOut {
                result: LocalId::new(9),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::Stop,
        ];

        let program = create_simple_program(operations.to_vec());
        let asm = translate_program(program).expect("Translation should succeed");

        // Verify bitwise opcodes
        assert_opcode_counts(
            &asm,
            &[
                ("AND", 1),
                ("OR", 1),
                ("XOR", 1),
                ("NOT", 1),
                ("BYTE", 1),
                ("SHL", 1),
                ("SHR", 1),
                ("SAR", 1),
                ("STOP", 1),
            ],
        );
    }

    // ==================== Comparison Operations ====================

    #[test]
    fn test_comparison_operations() {
        let operations = [
            // Set up values for comparison
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 10 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 20 }),
            Operation::Lt(TwoInOneOut {
                result: LocalId::new(2),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::Gt(TwoInOneOut {
                result: LocalId::new(3),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::SLt(TwoInOneOut {
                result: LocalId::new(4),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::SGt(TwoInOneOut {
                result: LocalId::new(5),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::Eq(TwoInOneOut {
                result: LocalId::new(6),
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            Operation::IsZero(OneInOneOut { result: LocalId::new(7), arg1: LocalId::new(0) }),
            Operation::Stop,
        ];

        let program = create_simple_program(operations.to_vec());
        let asm = translate_program(program).expect("Translation should succeed");

        // Verify comparison opcodes
        assert_opcode_counts(
            &asm,
            &[("LT", 1), ("GT", 1), ("SLT", 1), ("SGT", 1), ("EQ", 1), ("ISZERO", 1), ("STOP", 1)],
        );
    }

    // ==================== Environmental and Blockchain Operations ====================

    #[test]
    fn test_environmental_and_blockchain() {
        // Test environmental, blockchain, and call data operations
        let operations = vec![
            // Environmental operations
            Operation::Address(ZeroInOneOut { result: LocalId::new(0) }),
            Operation::Origin(ZeroInOneOut { result: LocalId::new(1) }),
            Operation::Caller(ZeroInOneOut { result: LocalId::new(2) }),
            Operation::CallValue(ZeroInOneOut { result: LocalId::new(3) }),
            Operation::GasPrice(ZeroInOneOut { result: LocalId::new(4) }),
            Operation::Gas(ZeroInOneOut { result: LocalId::new(5) }),
            Operation::SelfBalance(ZeroInOneOut { result: LocalId::new(6) }),
            // Blockchain operations
            Operation::Coinbase(ZeroInOneOut { result: LocalId::new(7) }),
            Operation::Timestamp(ZeroInOneOut { result: LocalId::new(8) }),
            Operation::Number(ZeroInOneOut { result: LocalId::new(9) }),
            Operation::Difficulty(ZeroInOneOut { result: LocalId::new(10) }),
            Operation::GasLimit(ZeroInOneOut { result: LocalId::new(11) }),
            Operation::ChainId(ZeroInOneOut { result: LocalId::new(12) }),
            Operation::BaseFee(ZeroInOneOut { result: LocalId::new(13) }),
            Operation::BlobBaseFee(ZeroInOneOut { result: LocalId::new(14) }),
            // Block hash and blob hash
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(15), value: 1 }),
            Operation::BlockHash(OneInOneOut { result: LocalId::new(16), arg1: LocalId::new(15) }),
            Operation::BlobHash(OneInOneOut { result: LocalId::new(17), arg1: LocalId::new(15) }),
            // Call data operations
            Operation::CallDataSize(ZeroInOneOut { result: LocalId::new(18) }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(19), value: 0 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(20), value: 32 }),
            Operation::CallDataCopy(ThreeInZeroOut {
                arg1: LocalId::new(19), // destOffset
                arg2: LocalId::new(19), // offset
                arg3: LocalId::new(20), // size
            }),
            // Code operations
            Operation::CodeSize(ZeroInOneOut { result: LocalId::new(21) }),
            Operation::CodeCopy(ThreeInZeroOut {
                arg1: LocalId::new(19), // destOffset
                arg2: LocalId::new(19), // offset
                arg3: LocalId::new(20), // size
            }),
            // External code operations
            Operation::ExtCodeSize(OneInOneOut {
                result: LocalId::new(22),
                arg1: LocalId::new(19),
            }),
            Operation::ExtCodeHash(OneInOneOut {
                result: LocalId::new(23),
                arg1: LocalId::new(19),
            }),
            Operation::ExtCodeCopy(LargeInZeroOut {
                args_start: LocalIndex::new(19), /* Uses locals 19-22 (address, destOffset,
                                                  * offset, size) */
            }),
            // Balance operations
            Operation::Balance(OneInOneOut { result: LocalId::new(24), arg1: LocalId::new(19) }),
            Operation::Stop,
        ];

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("Translation should succeed");

        // Verify expected opcodes with exact counts
        assert_opcode_counts(
            &asm,
            &[
                ("ADDRESS", 1),
                ("ORIGIN", 1),
                ("CALLER", 1),
                ("CALLVALUE", 1),
                ("GASPRICE", 1),
                ("GAS", 1),
                ("SELFBALANCE", 1),
                ("COINBASE", 1),
                ("TIMESTAMP", 1),
                ("NUMBER", 1),
                ("PREVRANDAO", 1),
                ("GASLIMIT", 1),
                ("CHAINID", 1),
                ("BASEFEE", 1),
                ("BLOBBASEFEE", 1),
                ("BLOCKHASH", 1),
                ("BLOBHASH", 1),
                ("CALLDATASIZE", 1),
                ("CALLDATACOPY", 1),
                ("CODESIZE", 1),
                ("CODECOPY", 2), // One from operation, one from deployment
                ("EXTCODESIZE", 1),
                ("EXTCODEHASH", 1),
                ("EXTCODECOPY", 1),
                ("BALANCE", 1),
            ],
        );
    }

    // ==================== Storage and Memory Operations ====================

    #[test]
    fn test_storage_and_memory() {
        // Test storage, memory, and transient storage operations
        // Start with extended storage operations (includes transient storage)
        let mut operations = extended_storage_operations();
        // Remove the Stop at the end so we can add more operations
        operations.pop();

        // Add additional memory operations
        operations.extend(vec![
            // Memory operations
            Operation::MemoryStore(MemoryStore {
                address: LocalId::new(0),
                value: LocalId::new(1),
                byte_size: 32,
            }),
            Operation::MemoryLoad(MemoryLoad {
                result: LocalId::new(4),
                address: LocalId::new(0),
                byte_size: 32,
            }),
            // Memory copy
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(5), value: 0 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(6), value: 32 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(7), value: 64 }),
            Operation::MCopy(ThreeInZeroOut {
                arg1: LocalId::new(7), // dest
                arg2: LocalId::new(5), // src
                arg3: LocalId::new(6), // size
            }),
            // Memory allocation
            Operation::AcquireFreePointer(ZeroInOneOut { result: LocalId::new(8) }),
            Operation::DynamicAllocAnyBytes(OneInOneOut {
                arg1: LocalId::new(6),
                result: LocalId::new(9),
            }),
            Operation::DynamicAllocZeroed(OneInOneOut {
                arg1: LocalId::new(6),
                result: LocalId::new(10),
            }),
            // Keccak256
            Operation::Keccak256(TwoInOneOut {
                result: LocalId::new(11),
                arg1: LocalId::new(5), // offset
                arg2: LocalId::new(6), // size
            }),
            Operation::Stop,
        ]);

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("Translation should succeed");

        // Verify storage and memory opcodes
        assert_opcode_counts(
            &asm,
            &[("SSTORE", 1), ("SLOAD", 1), ("TSTORE", 1), ("TLOAD", 1), ("MCOPY", 1), ("SHA3", 1)],
        );

        // Memory operations: 1 init + 7 locals + MemoryStore/Load + ops using locals
        assert_opcode_counts(&asm, &[("MSTORE", 17), ("MLOAD", 20)]);
    }

    // ==================== External Calls and Contract Creation ====================

    #[test]
    fn test_external_calls_and_creation() {
        // Test external calls and contract creation
        let operations = vec![
            // Setup arguments for calls
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }), /* gas */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 0 }), /* address */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 0 }), /* value */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 0 }), /* argsOffset */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(4), value: 0 }), /* argsSize */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(5), value: 0 }), /* retOffset */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(6), value: 32 }), /* retSize */
            // Various call types
            Operation::Call(LargeInOneOut {
                result: LocalId::new(7),
                args_start: LocalIndex::new(0), // Uses locals 0-6 (7 args)
            }),
            Operation::CallCode(LargeInOneOut {
                result: LocalId::new(8),
                args_start: LocalIndex::new(0),
            }),
            Operation::DelegateCall(LargeInOneOut {
                result: LocalId::new(9),
                args_start: LocalIndex::new(0), // Uses locals 0-5 (6 args, no value)
            }),
            Operation::StaticCall(LargeInOneOut {
                result: LocalId::new(10),
                args_start: LocalIndex::new(0),
            }),
            // Contract creation
            Operation::Create(LargeInOneOut {
                result: LocalId::new(11),
                args_start: LocalIndex::new(2), // Uses locals 2-4 (value, offset, size)
            }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(12), value: 0 }), /* salt */
            Operation::Create2(LargeInOneOut {
                result: LocalId::new(13),
                args_start: LocalIndex::new(2), // Uses locals 2-5 (value, offset, size, salt)
            }),
            // Return data operations
            Operation::ReturnDataSize(ZeroInOneOut { result: LocalId::new(14) }),
            Operation::ReturnDataSize(ZeroInOneOut { result: LocalId::new(15) }),
            Operation::ReturnDataCopy(ThreeInZeroOut {
                arg1: LocalId::new(5), // destOffset
                arg2: LocalId::new(3), // offset
                arg3: LocalId::new(6), // size
            }),
            // Self destruct
            Operation::SelfDestruct(OneInZeroOut { arg1: LocalId::new(1) }),
        ];

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("Translation should succeed");

        // Verify all call and creation opcodes
        assert_opcode_counts(
            &asm,
            &[
                ("CALL", 1),
                ("CALLCODE", 1),
                ("DELEGATECALL", 1),
                ("STATICCALL", 1),
                ("CREATE", 1),
                ("CREATE2", 1),
                ("RETURNDATASIZE", 2), // Both ReturndataSize and ReturnDataSize
                ("RETURNDATACOPY", 1),
                ("SELFDESTRUCT", 1),
            ],
        );
    }

    // ==================== Logging Operations ====================

    #[test]
    fn test_logging() {
        // Test LOG operations
        let operations = vec![
            // Setup log data
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }), /* offset */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 32 }), /* size */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 1 }), /* topic1 */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 2 }), /* topic2 */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(4), value: 3 }), /* topic3 */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(5), value: 4 }), /* topic4 */
            // All LOG variants
            Operation::Log0(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
            Operation::Log1(ThreeInZeroOut {
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
                arg3: LocalId::new(2),
            }),
            Operation::Log2(LargeInZeroOut { args_start: LocalIndex::new(0) }), /* offset, size, topic1, topic2 */
            Operation::Log3(LargeInZeroOut { args_start: LocalIndex::new(0) }), /* offset, size,
                                                                                 * topic1, topic2,
                                                                                 * topic3 */
            Operation::Log4(LargeInZeroOut { args_start: LocalIndex::new(0) }), /* offset, size, topic1, topic2, topic3, topic4 */
            Operation::Stop,
        ];

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("Translation should succeed");

        // Verify all LOG opcodes
        assert_opcode_counts(
            &asm,
            &[("LOG0", 1), ("LOG1", 1), ("LOG2", 1), ("LOG3", 1), ("LOG4", 1)],
        );
    }

    // ==================== Memory Boundary Tests ====================

    #[test]
    fn test_memory_boundary_operations() {
        // Test memory operations at boundary addresses
        let operations = vec![
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(0),
                value: BOUNDARY_ADDRESS,
            }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(1),
                value: TEST_VALUE_SMALL,
            }),
            Operation::MemoryStore(MemoryStore {
                address: LocalId::new(0),
                value: LocalId::new(1),
                byte_size: 32,
            }),
            Operation::MemoryLoad(MemoryLoad {
                result: LocalId::new(2),
                address: LocalId::new(0),
                byte_size: 32,
            }),
            Operation::Stop,
        ];

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("Memory boundary operations should translate");

        // Should handle large memory addresses
        // 1 init + 3 locals stored + 1 for MemoryStore = 5 MSTORE
        // 2 for loading locals + 1 for MemoryLoad + 1 for loading address = 4 MLOAD
        assert_opcode_counts(&asm, &[("MSTORE", 5), ("MLOAD", 4), ("STOP", 1)]);
    }

    // ==================== Control Flow and Termination ====================

    #[test]
    fn test_control_flow_and_termination() {
        // Test control flow, returns, and reverts
        use eth_ir_data::BasicBlockId;

        // Create a program with branching
        let blocks = vec![
            // Block 0: Initial block with branch
            (
                vec![Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: 1,
                })],
                Control::Branches(Branch {
                    condition: LocalId::new(0),
                    zero_target: BasicBlockId::new(1),
                    non_zero_target: BasicBlockId::new(2),
                }),
            ),
            // Block 1: Return path
            (
                vec![
                    Operation::LocalSetSmallConst(SetSmallConst {
                        local: LocalId::new(1),
                        value: 0,
                    }),
                    Operation::LocalSetSmallConst(SetSmallConst {
                        local: LocalId::new(2),
                        value: 32,
                    }),
                    Operation::Return(TwoInZeroOut {
                        arg1: LocalId::new(1),
                        arg2: LocalId::new(2),
                    }),
                ],
                Control::LastOpTerminates,
            ),
            // Block 2: Revert path
            (
                vec![
                    Operation::LocalSetSmallConst(SetSmallConst {
                        local: LocalId::new(3),
                        value: 0,
                    }),
                    Operation::LocalSetSmallConst(SetSmallConst {
                        local: LocalId::new(4),
                        value: 0,
                    }),
                    Operation::Revert(TwoInZeroOut {
                        arg1: LocalId::new(3),
                        arg2: LocalId::new(4),
                    }),
                ],
                Control::LastOpTerminates,
            ),
        ];

        let program = create_branching_program(blocks, 5);
        let asm = translate_program(program).expect("Translation should succeed");

        // Verify control flow opcodes - at least these counts based on the program structure
        assert_opcode_counts(
            &asm,
            &[
                ("JUMPI", 1),    // One conditional branch
                ("JUMP", 1),     // Jump to zero target after JUMPI
                ("JUMPDEST", 4), // Three blocks + init block
                ("RETURN", 1),   // One return in block 1
                ("REVERT", 1),   // One revert in block 2
            ],
        );
    }

    // ==================== Data Segments and Large Constants ====================

    #[test]
    fn test_data_segments_and_large_constants() {
        // Test data segments and large constants
        use alloy_primitives::U256;
        use eth_ir_data::{LargeConstId, index_vec};

        let operations = vec![
            // Reference data segments
            Operation::LocalSetDataOffset(SetDataOffset {
                local: LocalId::new(0),
                segment_id: DataId::new(0),
            }),
            Operation::LocalSetDataOffset(SetDataOffset {
                local: LocalId::new(1),
                segment_id: DataId::new(1),
            }),
            // Large constant
            Operation::LocalSetLargeConst(SetLargeConst {
                local: LocalId::new(2),
                cid: LargeConstId::new(0),
            }),
            Operation::LocalSetLargeConst(SetLargeConst {
                local: LocalId::new(3),
                cid: LargeConstId::new(1),
            }),
            Operation::Stop,
        ];

        let mut program = create_program_with_data(
            operations,
            vec![vec![0xde, 0xad, 0xbe, 0xef], vec![0xca, 0xfe, 0xba, 0xbe]],
        );

        // Add large constants
        program.large_consts =
            index_vec![U256::from(0xdeadbeef_u64), U256::from_be_bytes([0xff; 32]),];

        let asm = translate_program(program).expect("Translation should succeed");

        // Verify data and large constants
        assert_opcode_counts(
            &asm,
            &[
                ("PUSH32", 1), // For the 32-byte constant
                ("PUSH4", 1),  // For the 0xdeadbeef constant
            ],
        );

        // Count data segments
        let data_count = asm.iter().filter(|op| matches!(op, Asm::Data(_))).count();
        assert_eq!(data_count, 2, "Should have 2 data segments");
    }

    #[test]
    fn test_mcopy_operation() {
        // Test MCOPY memory-to-memory copy
        let operations = vec![
            // Set up source data at address 0
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(1),
                value: TEST_VALUE_SMALL,
            }),
            Operation::MemoryStore(MemoryStore {
                address: LocalId::new(0),
                value: LocalId::new(1),
                byte_size: 32,
            }),
            // Copy from address 0 to address 32
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 32 }), /* dest */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 0 }), /* src */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(4), value: 32 }), /* size */
            Operation::MCopy(ThreeInZeroOut {
                arg1: LocalId::new(2), // dest
                arg2: LocalId::new(3), // src
                arg3: LocalId::new(4), // size
            }),
            Operation::Stop,
        ];

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("MCOPY operation should translate");

        assert_opcode_counts(&asm, &[("MCOPY", 1), ("STOP", 1)]);
    }

    #[test]
    fn test_keccak256_operation() {
        // Test Keccak256 hash operation
        let operations = vec![
            // Store data to hash at memory address 0
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 0 }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(1),
                value: TEST_VALUE_SMALL,
            }),
            Operation::MemoryStore(MemoryStore {
                address: LocalId::new(0),
                value: LocalId::new(1),
                byte_size: 32,
            }),
            // Hash 32 bytes starting at address 0
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: 0 }), /* offset */
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(3), value: 32 }), /* size */
            Operation::Keccak256(TwoInOneOut {
                result: LocalId::new(4),
                arg1: LocalId::new(2), // offset
                arg2: LocalId::new(3), // size
            }),
            Operation::Stop,
        ];

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("Keccak256 operation should translate");

        assert_opcode_counts(&asm, &[("SHA3", 1), ("STOP", 1)]);
    }

    #[test]
    fn test_stop_and_invalid_operations() {
        // Test explicit Stop and Invalid operations
        let operations = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 1 }),
            Operation::Stop,
        ];

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("Stop operation should translate");
        assert_eq!(count_opcode(&asm, "STOP"), 1);

        // Test Invalid operation
        let operations_invalid = vec![Operation::Invalid];

        let program_invalid = create_simple_program(operations_invalid);
        let asm_invalid =
            translate_program(program_invalid).expect("Invalid operation should translate");
        assert_eq!(count_opcode(&asm_invalid, "INVALID"), 1);
    }

    #[test]
    fn test_internal_call_operation() {
        // Test InternalCall operation
        let operations = vec![
            // Set up arguments for internal call
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 10 }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 20 }),
            // Internal call to function 0
            Operation::InternalCall(InternalCall {
                function: FunctionId::new(0),
                args_start: LocalIndex::new(0),
                outputs_start: LocalIndex::new(2),
            }),
            Operation::Stop,
        ];

        let program = create_simple_program(operations);
        let asm = translate_program(program).expect("InternalCall operation should translate");

        // Debug output if needed for troubleshooting
        if std::env::var("DEBUG_ASM").is_ok() {
            print_assembly(&asm);
        }

        // Internal calls generate JUMP and other control flow
        assert!(count_opcode(&asm, "JUMP") >= 1);
    }

    // ==================== Switch Statement ====================

    #[test]
    fn test_switch_statement() {
        // Test switch statement with multiple cases
        use eth_ir_data::{BasicBlockId, Case, CasesId, index_vec};

        // Create a switch statement program
        let mut program = create_branching_program(
            vec![
                // Block 0: Switch statement
                (
                    vec![Operation::LocalSetSmallConst(SetSmallConst {
                        local: LocalId::new(0),
                        value: 2,
                    })],
                    Control::Switch(Switch {
                        condition: LocalId::new(0),
                        cases: CasesId::new(0),
                        fallback: Some(BasicBlockId::new(4)),
                    }),
                ),
                // Block 1: Case 0
                (vec![Operation::Stop], Control::LastOpTerminates),
                // Block 2: Case 1
                (vec![Operation::Stop], Control::LastOpTerminates),
                // Block 3: Case 2
                (vec![Operation::Stop], Control::LastOpTerminates),
                // Block 4: Fallback
                (vec![Operation::Invalid], Control::LastOpTerminates),
            ],
            1,
        );

        // Add cases
        use eth_ir_data::Cases;
        program.cases = index_vec![Cases {
            cases: vec![
                Case { value: U256::from(0), target: BasicBlockId::new(1) },
                Case { value: U256::from(1), target: BasicBlockId::new(2) },
                Case { value: U256::from(2), target: BasicBlockId::new(3) },
            ]
        },];

        let asm = translate_program(program).expect("Translation should succeed");

        // Switch with 3 cases: DUP1 for each case comparison (3 total)
        // EQ for each case comparison, JUMPI for each case
        // INVALID in fallback
        assert_opcode_counts(&asm, &[("DUP1", 3), ("EQ", 3), ("JUMPI", 3), ("INVALID", 1)]);
    }

    // ==================== Error Cases (Negative Tests) ====================

    #[test]
    fn test_undefined_local_error() {
        use crate::{CodegenError, Translator};

        // Create a program that references a local that doesn't exist in the locals vector
        let operations = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 10 }),
            // Try to use local 999 which doesn't exist
            Operation::Add(TwoInOneOut {
                arg1: LocalId::new(0),
                arg2: LocalId::new(999), // This local doesn't exist
                result: LocalId::new(1),
            }),
        ];

        let mut program = create_simple_program(operations);
        // Only allocate locals 0 and 1, not 999
        program.locals = index_vec![LocalId::new(0), LocalId::new(1)];

        let mut translator = Translator::new(program);
        let result = translator.translate();

        assert!(
            matches!(result, Err(CodegenError::LocalNotFound(id)) if id.get() == 999),
            "Expected LocalNotFound error for undefined local 999, got {:?}",
            result
        );
    }

    #[test]
    fn test_data_segment_not_found_error() {
        use crate::{CodegenError, Translator};
        use eth_ir_data::DataId;

        // Create a program that references a data segment that doesn't exist
        let operations = vec![Operation::LocalSetDataOffset(SetDataOffset {
            local: LocalId::new(0),
            segment_id: DataId::new(999), // Doesn't exist
        })];

        let program = create_simple_program(operations);
        // Program has no data segments

        let mut translator = Translator::new(program);
        let result = translator.translate();

        assert!(
            matches!(result, Err(CodegenError::DataSegmentNotFound(_))),
            "Expected DataSegmentNotFound error for invalid data segment"
        );
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn test_large_const_not_found_panic() {
        use crate::Translator;
        use eth_ir_data::LargeConstId;

        // Create a program that references a large constant that doesn't exist
        // This will panic with index out of bounds
        let operations = vec![Operation::LocalSetLargeConst(SetLargeConst {
            local: LocalId::new(0),
            cid: LargeConstId::new(999), // Doesn't exist
        })];

        let program = create_simple_program(operations);
        // Program has no large constants

        let mut translator = Translator::new(program);
        // This will panic when trying to access the non-existent large constant
        let _ = translator.translate();
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn test_invalid_local_range_in_call() {
        use crate::Translator;

        // CALL operation expects 7 contiguous locals starting from args_start
        // If the locals array isn't large enough, get_args will panic with index out of bounds
        let operations = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 100 }),
            Operation::Call(LargeInOneOut {
                args_start: LocalIndex::new(0), // Needs to access locals[0..7]
                result: LocalId::new(7),
            }),
        ];

        let mut program = create_simple_program(operations);
        // Only allocate 5 locals, but CALL needs to access index 6
        program.locals = index_vec![
            LocalId::new(0),
            LocalId::new(1),
            LocalId::new(2),
            LocalId::new(3),
            LocalId::new(7)
        ];

        let mut translator = Translator::new(program);
        // This will panic when get_args tries to access locals[6]
        let _ = translator.translate();
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn test_invalid_switch_cases_reference() {
        use crate::Translator;
        use eth_ir_data::{BasicBlockId, CasesId, Switch};

        // Create a switch that references non-existent cases
        let mut program = create_branching_program(
            vec![
                (
                    vec![Operation::LocalSetSmallConst(SetSmallConst {
                        local: LocalId::new(0),
                        value: 1,
                    })],
                    Control::Switch(Switch {
                        condition: LocalId::new(0),
                        cases: CasesId::new(999), // Non-existent cases array
                        fallback: Some(BasicBlockId::new(1)),
                    }),
                ),
                (vec![Operation::Stop], Control::LastOpTerminates),
            ],
            1,
        );
        // program.cases is empty, so accessing cases[999] will panic

        let mut translator = Translator::new(program);
        let _ = translator.translate();
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn test_invalid_branch_target() {
        use crate::Translator;
        use eth_ir_data::{BasicBlockId, Branch};

        // Create a branch to a non-existent block
        let program = create_branching_program(
            vec![
                (
                    vec![Operation::LocalSetSmallConst(SetSmallConst {
                        local: LocalId::new(0),
                        value: 1,
                    })],
                    Control::Branches(Branch {
                        condition: LocalId::new(0),
                        non_zero_target: BasicBlockId::new(999), // Non-existent block
                        zero_target: BasicBlockId::new(1),
                    }),
                ),
                (vec![Operation::Stop], Control::LastOpTerminates),
            ],
            1,
        );

        let mut translator = Translator::new(program);
        let _ = translator.translate();
    }
}
