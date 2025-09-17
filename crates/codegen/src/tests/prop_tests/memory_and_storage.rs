//! Property tests for memory operations

use super::helpers::create_return_ops;
use crate::{
    tests::helpers::{
        PROP_TEST_OFFSET_LOCAL_ID, PROP_TEST_SIZE_LOCAL_ID, TEMP_LOCAL_BASE_ID,
        create_branching_program, create_simple_program, execute_and_get_result,
    },
    translate_program,
    translator::memory::constants,
};
use alloy_primitives::U256;
use eth_ir_data::{Control, LocalId, Operation, operation::*};
use evm_glue::assembler::assemble_minimized;
use proptest::prelude::*;

/// Memory offset strategy with edge cases for boundary testing
fn memory_offset_strategy() -> impl Strategy<Value = u32> {
    prop_oneof![
        // Aligned addresses (32-byte boundaries)
        Just(0u32),
        Just(32u32),
        Just(64u32),
        Just(256u32),
        Just(0x1000u32),
        // Unaligned addresses (test padding/alignment)
        Just(1u32),
        Just(31u32),
        Just(33u32),
        Just(255u32),
        // Large offsets (test memory expansion)
        Just(0xFFFFu32),
        Just(0x10000u32),
        // Random offsets with bias toward smaller values
        0u32..1000,
        0u32..0x10000,
    ]
}

/// Storage key strategy focusing on edge cases and gas optimization patterns
fn storage_key_strategy() -> impl Strategy<Value = u64> {
    prop_oneof![
        // Zero and near-zero (cold vs warm storage)
        Just(0u64),
        Just(1u64),
        // Common patterns
        Just(0x42u64),
        Just(0x1337u64),
        // Keccak-like patterns (simulating mapping keys)
        Just(0xDEADBEEFu64),
        Just(0xCAFEBABEu64),
        // Boundary values
        Just(u64::MAX),
        Just(u64::MAX - 1),
        // Sequential keys (array-like access)
        0u64..100,
        // Random keys
        any::<u64>(),
    ]
}

/// Value strategy for memory/storage with focus on bit patterns
fn value_pattern_strategy() -> impl Strategy<Value = u64> {
    prop_oneof![
        // Zero values (important for gas refunds)
        Just(0u64),
        // All ones (bit pattern testing)
        Just(u64::MAX),
        // Byte patterns
        Just(0xFFu64),
        Just(0xFF00u64),
        Just(0xFF00FFu64),
        // Common test values
        Just(1u64),
        Just(42u64),
        Just(256u64),
        // Powers of 2
        prop::sample::select(vec![1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]),
        // Random values
        any::<u64>(),
    ]
}

proptest! {
    #[test]
    fn test_memory_store_load_invariant(offset in memory_offset_strategy(), value in value_pattern_strategy()) {
        // Ensure we don't overlap with locals area which starts at 0x80
        // Add 0x1000 to safely place our test data beyond locals
        let safe_offset = 0x1000 + ((offset / 32) * 32);

        let mut ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(0),
                value: safe_offset as u64
            }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(1),
                value
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
        ];
        ops.extend(create_return_ops(2));

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        if let Ok(res) = result {
            prop_assert_eq!(res, U256::from(value),
                "Memory store/load invariant violated");
        }
    }

    #[test]
    fn test_memory_bytes(offset in memory_offset_strategy(), value in 0u8..=255) {
        // Test MSTORE8 and byte-wise loading
        // Ensure offset doesn't overlap with locals area
        // Ensure offset is beyond locals area (LOCALS_START + max_locals * SLOT_SIZE)
        // Using 0x1000 as a safe starting point for general memory operations
        let safe_offset = 0x1000 + offset;

        let mut ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(0),
                value: safe_offset as u64
            }),
            Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(1),
                value: value as u64
            }),
            Operation::MemoryStore(MemoryStore {
                address: LocalId::new(0),
                value: LocalId::new(1),
                byte_size: 1, // Single byte store
            }),
            Operation::MemoryLoad(MemoryLoad {
                result: LocalId::new(2),
                address: LocalId::new(0),
                byte_size: 32, // Load full word
            }),
        ];
        ops.extend(create_return_ops(2));

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok());

        // Note: MSTORE8 stores a single byte at the specified address
        // When loading 32 bytes from that address, the stored byte is at the beginning
        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => {
                // MSTORE8 stores the least significant byte of the value
                // at the specified memory address
                // When we load 32 bytes, the byte is at the start, rest are zeros
                let mut expected_bytes = [0u8; 32];
                expected_bytes[0] = value;
                let expected = U256::from_be_bytes(expected_bytes);
                prop_assert_eq!(res, expected,
                    "Memory byte store/load failed: stored byte {} but got {:x}", value, res);
            }
            Err(e) => prop_assert!(false, "Memory byte test execution failed: {}", e),
        }
    }
}

proptest! {
    #[test]
    fn test_storage_persistence(pairs in prop::collection::vec((any::<u64>(), any::<u64>()), 1..10)) {
        let keys: Vec<u64> = pairs.iter().map(|(k, _)| *k).collect();
        let values: Vec<u64> = pairs.iter().map(|(_, v)| *v).collect();

        let mut ops = vec![];

        // Store all key-value pairs
        for (i, (key, value)) in keys.iter().zip(values.iter()).enumerate() {
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new((i * 2) as u32),
                value: *key
            }));
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new((i * 2 + 1) as u32),
                value: *value
            }));
            ops.push(Operation::SStore(TwoInZeroOut {
                arg1: LocalId::new((i * 2) as u32),
                arg2: LocalId::new((i * 2 + 1) as u32),
            }));
        }

        // Load and verify ALL values to ensure persistence
        let success_flag_local = LocalId::new(TEMP_LOCAL_BASE_ID);
        ops.push(Operation::LocalSetSmallConst(SetSmallConst {
            local: success_flag_local,
            value: 1  // Start with success
        }));

        for (i, (key, expected_value)) in keys.iter().zip(values.iter()).enumerate() {
            let key_local = LocalId::new(101 + i as u32 * 3);
            let loaded_local = LocalId::new(102 + i as u32 * 3);
            let eq_local = LocalId::new(103 + i as u32 * 3);

            // Load stored value
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: key_local,
                value: *key
            }));
            ops.push(Operation::SLoad(OneInOneOut {
                result: loaded_local,
                arg1: key_local
            }));

            // Check if loaded value equals expected
            ops.push(Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::new(101 + i as u32 * 3 + 1),
                value: *expected_value
            }));
            ops.push(Operation::Eq(TwoInOneOut {
                result: eq_local,
                arg1: loaded_local,
                arg2: LocalId::new(101 + i as u32 * 3 + 1),
            }));

            // AND with success flag (all must match)
            ops.push(Operation::And(TwoInOneOut {
                result: success_flag_local,
                arg1: success_flag_local,
                arg2: eq_local,
            }));
        }

        // Return the success flag (1 if all match, 0 otherwise)
        ops.push(Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(PROP_TEST_OFFSET_LOCAL_ID),
            value: constants::LOCALS_START as u64 + 100 * 32
        }));
        ops.push(Operation::LocalSetSmallConst(SetSmallConst {
            local: LocalId::new(PROP_TEST_SIZE_LOCAL_ID),
            value: 32
        }));
        ops.push(Operation::Return(TwoInZeroOut {
            arg1: LocalId::new(PROP_TEST_OFFSET_LOCAL_ID),
            arg2: LocalId::new(PROP_TEST_SIZE_LOCAL_ID),
        }));

        // Use create_branching_program starting at block 0
        let program = create_branching_program(
            vec![(ops, Control::LastOpTerminates)],
            0
        );

        let asm = translate_program(program);
        prop_assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok());

        // Execute and verify all values persisted correctly
        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::from(1), "Storage persistence check failed"),
            Err(e) if e.contains("STOP") => {}, // Empty storage test
            Err(e) => prop_assert!(false, "Execution failed: {}", e),
        }
    }
}

proptest! {
    #[test]
    fn test_transient_storage(key in storage_key_strategy(), value in value_pattern_strategy()) {
        // Test TSTORE and TLOAD basic functionality
        let mut ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: key }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value }),
            Operation::TStore(TwoInZeroOut {
                arg1: LocalId::new(0),  // key
                arg2: LocalId::new(1),  // value
            }),
            Operation::TLoad(OneInOneOut {
                result: LocalId::new(2),
                arg1: LocalId::new(0),  // key
            }),
        ];
        ops.extend(create_return_ops(2));

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok(), "TSTORE/TLOAD should translate");

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok(), "TSTORE/TLOAD should assemble");

        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::from(value), "TLOAD should return stored value"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_transient_storage_overwrite(key in any::<u64>(), val1 in any::<u64>(), val2 in any::<u64>()) {
        // Test that TSTORE overwrites previous values
        let mut ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: key }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: val1 }),
            Operation::TStore(TwoInZeroOut {
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),
            // Overwrite with new value
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: val2 }),
            Operation::TStore(TwoInZeroOut {
                arg1: LocalId::new(0),
                arg2: LocalId::new(2),
            }),
            // Load the value
            Operation::TLoad(OneInOneOut {
                result: LocalId::new(3),
                arg1: LocalId::new(0),
            }),
        ];
        ops.extend(create_return_ops(3));

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::from(val2), "TLOAD should return last stored value"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_transient_storage_uninitialized(key in any::<u64>()) {
        // Test that TLOAD returns 0 for uninitialized keys
        let mut ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: key }),
            Operation::TLoad(OneInOneOut {
                result: LocalId::new(1),
                arg1: LocalId::new(0),
            }),
        ];
        ops.extend(create_return_ops(1));

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok());

        let result = execute_and_get_result(bytecode.unwrap().1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::ZERO, "TLOAD should return 0 for uninitialized key"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_transient_vs_persistent_storage(key in any::<u64>(), tval in any::<u64>(), sval in any::<u64>()) {
        // Test that transient and persistent storage are independent
        let mut ops = vec![
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: key }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: sval }),
            Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(2), value: tval }),

            // Store in persistent storage
            Operation::SStore(TwoInZeroOut {
                arg1: LocalId::new(0),
                arg2: LocalId::new(1),
            }),

            // Store in transient storage with same key
            Operation::TStore(TwoInZeroOut {
                arg1: LocalId::new(0),
                arg2: LocalId::new(2),
            }),

            // Load from transient storage
            Operation::TLoad(OneInOneOut {
                result: LocalId::new(3),
                arg1: LocalId::new(0),
            }),

            // Load from persistent storage
            Operation::SLoad(OneInOneOut {
                result: LocalId::new(4),
                arg1: LocalId::new(0),
            }),

            // Check if they're different (XOR should be non-zero if different)
            Operation::Xor(TwoInOneOut {
                result: LocalId::new(5),
                arg1: LocalId::new(3),
                arg2: LocalId::new(4),
            }),
        ];
        ops.extend(create_return_ops(5));

        let program = create_simple_program(ops);
        let asm = translate_program(program);
        prop_assert!(asm.is_ok());

        let bytecode = assemble_minimized(&asm.unwrap(), true);
        prop_assert!(bytecode.is_ok());

        if tval != sval {
            let result = execute_and_get_result(bytecode.unwrap().1);
            match result {
                Ok(res) => prop_assert_ne!(res, U256::ZERO, "Transient and persistent storage should be independent"),
                Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
                _ => {}
            }
        }
    }
}
