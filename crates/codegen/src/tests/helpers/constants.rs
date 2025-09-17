/// Default gas limit for tests
pub const TEST_GAS_LIMIT: u64 = 1_000_000;

/// Default ETH balance for test accounts (1 ETH in wei)
pub const TEST_ETH_BALANCE: u64 = 1_000_000_000_000_000_000;

// === Local ID Allocations ===
// These IDs are used to avoid conflicts between different test components
// Each range is reserved for specific test scenarios
// IMPORTANT: Never reuse ID ranges - each test context must have unique IDs

/// Base ID for temporary locals used in complex test scenarios
/// Range: 100-199
pub const TEMP_LOCAL_BASE_ID: u32 = 100;

/// Local IDs for keccak256 operation tests
/// Range: 200-202
pub const KECCAK_OFFSET_LOCAL_ID: u32 = 200;
pub const KECCAK_SIZE_LOCAL_ID: u32 = 201;
pub const KECCAK_RESULT_LOCAL_ID: u32 = 202;

/// Local IDs for property test memory operations
/// Range: 210-212 (separate from keccak to avoid conflicts)
pub const PROP_TEST_OFFSET_LOCAL_ID: u32 = 210;
pub const PROP_TEST_SIZE_LOCAL_ID: u32 = 211;

/// Reserved ranges for future test expansions:
/// Range: 220-229 - Available for storage tests
/// Range: 230-239 - Available for control flow tests
/// Range: 240-249 - Available for call/create tests
/// Range: 250-255 - Available for gas metering tests

/// Storage test values
/// Key chosen to be non-zero and recognizable in debug output
pub const TEST_STORAGE_KEY: u64 = 42;
/// Value chosen to be distinct and recognizable (leet-speak for debugging)
pub const TEST_STORAGE_VALUE: u64 = 1337;

/// Memory boundary for testing edge cases (64KB - 1)
pub const TEST_MEMORY_BOUNDARY: u64 = 0xFFFF;

/// Standard test values of different magnitudes
pub const TEST_VALUE_SMALL: u64 = 42; // Common test value
pub const TEST_VALUE_MEDIUM: u64 = 100; // Decimal round number
pub const TEST_VALUE_LARGE: u64 = 256; // 2^8, tests byte boundaries

/// Common arithmetic test operands
pub const TEST_OPERAND_A: u64 = 10;
pub const TEST_OPERAND_B: u64 = 5;
pub const TEST_OPERAND_C: u64 = 20;
pub const TEST_DIVISOR: u64 = 7;

/// Expected results for common operations
pub const TEST_ADD_RESULT: u64 = 15; // 10 + 5
pub const TEST_SDIV_RESULT: u64 = 2; // 10 / 5
pub const TEST_ADDMOD_RESULT: u64 = 2; // (10 + 20) % 7
pub const TEST_MULMOD_RESULT: u64 = 2; // (5 * 6) % 7 = 30 % 7 = 2

/// Bit operation test values
pub const TEST_BIT_PATTERN_A: u64 = 0xFF;
pub const TEST_BIT_PATTERN_B: u64 = 0xF0;
pub const TEST_BIT_PATTERN_C: u64 = 0x0F;
pub const TEST_BIT_PATTERN_D: u64 = 0xAA;
pub const TEST_BIT_PATTERN_E: u64 = 0x55;

/// Shift operation test values
pub const TEST_SHIFT_AMOUNT: u64 = 4;
pub const TEST_SHIFT_VALUE: u64 = 16;
pub const TEST_SHIFT_RESULT: u64 = 256; // 16 << 4
pub const TEST_SAR_VALUE: u64 = 0x8000_0000_0000_0000;
pub const TEST_SAR_RESULT: u64 = 0x4000_0000_0000_0000;

/// Mask for extracting single byte (0xFF = 255)
pub const TEST_BYTE_MASK: u64 = 0xFF;

/// EVM word size in bytes (256 bits / 8)
pub const EVM_WORD_SIZE_BYTES: u64 = 32;
/// Starting offset for memory operations
pub const MEMORY_START_OFFSET: u64 = 0;
/// Common memory offset for tests
pub const TEST_MEMORY_OFFSET: u64 = 32;

/// Common values for control flow and misc operations
pub const TEST_REVERT_ERROR_CODE: u64 = 0xBADC0DE;
pub const TEST_RETURN_VALUE: u64 = 0xDEADBEEF;
pub const TEST_CALL_GAS: u64 = 1000;
pub const TEST_CALL_ADDRESS: u64 = 0x2000;

/// Number of iterations for performance benchmarking tests
pub const PERF_TEST_ITERATIONS: usize = 100;

/// Standard test values for binary operations
pub const BINARY_OP_TEST_CASES: [(u64, u64); 5] =
    [(10, 20), (0, 0), (u64::MAX, 1), (100, 0), (0, 100)];

/// Default test cases for unary operations
pub const UNARY_OP_TEST_CASES: [u64; 5] = [0, 1, 42, 255, u64::MAX];

/// Test values for signed arithmetic operations
pub const SIGNED_OP_TEST_CASES: [(u64, u64); 8] = [
    (10, 20),
    (20, 10),
    (7, 3),
    (100, 7),
    (0, 10),
    (10, 1),
    (u64::MAX, 2),
    (u64::MAX - 1, u64::MAX),
];
