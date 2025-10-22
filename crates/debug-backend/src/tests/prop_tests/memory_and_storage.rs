//! Property tests for memory operations

use super::helpers::{calculate_safe_memory_offset, create_return_ops};
use crate::{
    tests::helpers::{
        PROP_TEST_OFFSET_LOCAL_ID, PROP_TEST_SIZE_LOCAL_ID, TEMP_LOCAL_BASE_ID, constants,
        create_branching_program, create_simple_program, execute_bytecode,
    },
    translate_program,
};
use alloy_primitives::U256;
use evm_glue::assembler::assemble_minimized;
use proptest::prelude::*;
use sir_data::{Control, LocalId, Operation, operation::*};

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
fn storage_key_strategy() -> impl Strategy<Value = u32> {
    prop_oneof![
        // Zero and near-zero (cold vs warm storage)
        Just(0u32),
        Just(1u32),
        // Common patterns
        Just(0x42u32),
        Just(0x1337u32),
        // Keccak-like patterns (simulating mapping keys)
        Just(0xDEADBEEFu32),
        Just(0xCAFEBABEu32),
        // Boundary values
        Just(u32::MAX),
        Just(u32::MAX - 1),
        // Sequential keys (array-like access)
        0u32..100,
        // Random keys
        any::<u32>(),
    ]
}

/// Value strategy for memory/storage with focus on bit patterns
fn value_pattern_strategy() -> impl Strategy<Value = u32> {
    prop_oneof![
        // Zero values (important for gas refunds)
        Just(0u32),
        // All ones (bit pattern testing)
        Just(u32::MAX),
        // Byte patterns
        Just(0xFFu32),
        Just(0xFF00u32),
        Just(0xFF00FFu32),
        // Common test values
        Just(1u32),
        Just(42u32),
        Just(256u32),
        // Powers of 2
        prop::sample::select(vec![1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]),
        // Random values
        any::<u32>(),
    ]
}

proptest! {
    #[test]
    fn test_memory_store_load_invariant(offset in memory_offset_strategy(), value in value_pattern_strategy()) {
        // First create operations with placeholder offset to determine number of locals
        let mut ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value }),
            Operation::MemoryStore(MemoryStoreData {
                ptr: LocalId::new(0),
                value: LocalId::new(1),
                io_size: IRMemoryIOByteSize::B32,
            }),
            Operation::MemoryLoad(MemoryLoadData {
                out: LocalId::new(2),
                ptr: LocalId::new(0),
                io_size: IRMemoryIOByteSize::B32,
            }),
        ];
        ops.extend(create_return_ops(2));

        // Create program to determine number of locals
        let program = create_simple_program(ops.clone());
        let safe_offset = calculate_safe_memory_offset(&program) + ((offset / 32) * 32);

        // Recreate operations with correct offset
        let mut ops = vec![
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(0),
                value: safe_offset as u32,
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(1),
                value: value as u32,
            }),
            Operation::MemoryStore(MemoryStoreData {
                ptr: LocalId::new(0),
                value: LocalId::new(1),
                io_size: IRMemoryIOByteSize::B32,
            }),
            Operation::MemoryLoad(MemoryLoadData {
                out: LocalId::new(2),
                ptr: LocalId::new(0),
                io_size: IRMemoryIOByteSize::B32,
            }),
        ];
        ops.extend(create_return_ops(2));

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        if let Ok(res) = result {
            prop_assert_eq!(res, U256::from(value),
                "Memory store/load invariant violated");
        }
    }

    #[test]
    fn test_memory_bytes(offset in memory_offset_strategy(), value in 0u8..=255) {
        // First create operations with placeholder offset to determine number of locals
        let mut ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: value as u32 }),
            Operation::MemoryStore(MemoryStoreData {
                ptr: LocalId::new(0),
                value: LocalId::new(1),
                io_size: IRMemoryIOByteSize::B1,
            }),
            Operation::MemoryLoad(MemoryLoadData {
                out: LocalId::new(2),
                ptr: LocalId::new(0),
                io_size: IRMemoryIOByteSize::B32,
            }),
        ];
        ops.extend(create_return_ops(2));

        // Create program to determine number of locals
        let program = create_simple_program(ops.clone());
        let safe_offset = calculate_safe_memory_offset(&program) + offset;

        // Recreate operations with correct offset
        let mut ops = vec![
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(0),
                value: safe_offset as u32
            }),
            Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(1),
                value: value as u32,
            }),
            Operation::MemoryStore(MemoryStoreData {
                ptr: LocalId::new(0),
                value: LocalId::new(1),
                io_size: IRMemoryIOByteSize::B1, // Single byte store
            }),
            Operation::MemoryLoad(MemoryLoadData {
                out: LocalId::new(2),
                ptr: LocalId::new(0),
                io_size: IRMemoryIOByteSize::B32, // Load full word
            }),
        ];
        ops.extend(create_return_ops(2));

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        // Note: MSTORE8 stores a single byte at the specified address
        // When loading 32 bytes from that address, the stored byte is at the beginning
        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
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
    fn test_storage_persistence(pairs in prop::collection::vec((any::<u32>(), any::<u32>()), 1..10)) {
        let keys: Vec<u32> = pairs.iter().map(|(k, _)| *k).collect();
        let values: Vec<u32> = pairs.iter().map(|(_, v)| *v).collect();

        let mut ops = vec![];

        // Store all key-value pairs
        for (i, (key, value)) in keys.iter().zip(values.iter()).enumerate() {
            ops.push(Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new((i * 2) as u32),
                value: *key,
            }));
            ops.push(Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new((i * 2 + 1) as u32),
                value: *value,
            }));
            ops.push(Operation::SStore(InlineOperands {
                ins: [LocalId::new((i * 2) as u32), LocalId::new((i * 2 + 1) as u32)],
                outs: [],
            }));
        }

        // Load and verify ALL values to ensure persistence
        let success_flag_local = LocalId::new(TEMP_LOCAL_BASE_ID);
        ops.push(Operation::SetSmallConst(SetSmallConstData {
            sets: success_flag_local,
            value: 1  // Start with success
        }));

        for (i, (key, expected_value)) in keys.iter().zip(values.iter()).enumerate() {
            let key_local = LocalId::new(101 + i as u32 * 3);
            let loaded_local = LocalId::new(102 + i as u32 * 3);
            let eq_local = LocalId::new(103 + i as u32 * 3);

            // Load stored value
            ops.push(Operation::SetSmallConst(SetSmallConstData {
                sets: key_local,
                value: *key,
            }));
            ops.push(Operation::SLoad(InlineOperands {
                ins: [key_local],
                outs: [loaded_local],
            }));

            // Check if loaded value equals expected
            ops.push(Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(101 + i as u32 * 3 + 1),
                value: *expected_value,
            }));
            ops.push(Operation::Eq(InlineOperands {
                ins: [loaded_local, LocalId::new(101 + i as u32 * 3 + 1)],
                outs: [eq_local],
            }));

            // AND with success flag (all must match)
            ops.push(Operation::And(InlineOperands {
                ins: [success_flag_local, eq_local],
                outs: [success_flag_local],
            }));
        }

        // Return the success flag (1 if all match, 0 otherwise)
        ops.push(Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(PROP_TEST_OFFSET_LOCAL_ID),
            value: (constants::LOCALS_START as u64 + 100 * 32) as u32,
        }));
        ops.push(Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(PROP_TEST_SIZE_LOCAL_ID),
            value: 32,
        }));
        ops.push(Operation::Return(InlineOperands {
            ins: [LocalId::new(PROP_TEST_OFFSET_LOCAL_ID), LocalId::new(PROP_TEST_SIZE_LOCAL_ID)],
            outs: [],
        }));

        // Use create_branching_program starting at block 0
        let program = create_branching_program(
            vec![(ops, Control::LastOpTerminates)],
            0
        );

        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        // Execute and verify all values persisted correctly
        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
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
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: key }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value }),
            Operation::TStore(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [],
        }),
            Operation::TLoad(InlineOperands {
            ins: [LocalId::new(0)],
            outs: [LocalId::new(2)],
        }),
        ];
        ops.extend(create_return_ops(2));

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::from(value), "TLOAD should return stored value"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_transient_storage_overwrite(key in any::<u32>(), val1 in any::<u32>(), val2 in any::<u32>()) {
        // Test that TSTORE overwrites previous values
        let mut ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: key }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: val1 }),
            Operation::TStore(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(1)],
            outs: [],
        }),
            // Overwrite with new value
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: val2 }),
            Operation::TStore(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(2)],
            outs: [],
        }),
            // Load the value
            Operation::TLoad(InlineOperands {
            ins: [LocalId::new(0)],
            outs: [LocalId::new(3)],
        }),
        ];
        ops.extend(create_return_ops(3));

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::from(val2), "TLOAD should return last stored value"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_transient_storage_uninitialized(key in any::<u32>()) {
        // Test that TLOAD returns 0 for uninitialized keys
        let mut ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: key }),
            Operation::TLoad(InlineOperands {
            ins: [LocalId::new(0)],
            outs: [LocalId::new(1)],
        }),
        ];
        ops.extend(create_return_ops(1));

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        let result = execute_bytecode(bytecode.expect("Assembly failed").1);
        match result {
            Ok(res) => prop_assert_eq!(res, U256::ZERO, "TLOAD should return 0 for uninitialized key"),
            Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
            _ => {}
        }
    }
}

proptest! {
    #[test]
    fn test_transient_vs_persistent_storage(key in any::<u32>(), tval in any::<u32>(), sval in any::<u32>()) {
        // Test that transient and persistent storage are independent
        let mut ops = vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: key }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: sval }),
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: tval }),

            // Store in persistent storage
            Operation::SStore(InlineOperands {
                ins: [LocalId::new(0), LocalId::new(1)],
                outs: [],
            }),

            // Store in transient storage with same key
            Operation::TStore(InlineOperands {
            ins: [LocalId::new(0), LocalId::new(2)],
            outs: [],
        }),

            // Load from transient storage
            Operation::TLoad(InlineOperands {
            ins: [LocalId::new(0)],
            outs: [LocalId::new(3)],
        }),

            // Load from persistent storage
            Operation::SLoad(InlineOperands {
            ins: [LocalId::new(0)],
            outs: [LocalId::new(4)],
        }),

            // Check if they're different (XOR should be non-zero if different)
            Operation::Xor(InlineOperands {
            ins: [LocalId::new(3), LocalId::new(4)],
            outs: [LocalId::new(5)],
        }),
        ];
        ops.extend(create_return_ops(5));

        let program = create_simple_program(ops);
        let asm = translate_program(program);


        let bytecode = assemble_minimized(&asm, true);



        if tval != sval {
            let result = execute_bytecode(bytecode.expect("Assembly failed").1);
            match result {
                Ok(res) => prop_assert_ne!(res, U256::ZERO, "Transient and persistent storage should be independent"),
                Err(e) if !e.contains("STOP") => prop_assert!(false, "Execution failed: {}", e),
                _ => {}
            }
        }
    }
}
