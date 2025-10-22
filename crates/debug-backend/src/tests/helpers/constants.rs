/// Default gas limit for tests
pub const TEST_GAS_LIMIT: u64 = 1_000_000;

/// Default ETH balance for test accounts (1 ETH in wei)
pub const TEST_ETH_BALANCE: u64 = 1_000_000_000_000_000_000;

// === Memory Layout Constants ===
/// Starting address for local variables in memory
/// TODO: Will be removed when switching to stack-based locals
pub const LOCALS_START: u32 = 0x00;
/// Size of each memory slot for locals (32 bytes)
pub const SLOT_SIZE: u32 = 0x20;

// === Local ID Allocations ===
// These IDs are used to avoid conflicts between different test components
// Each range is reserved for specific test scenarios
// IMPORTANT: Never reuse ID ranges - each test context must have unique IDs

/// Base ID for temporary locals used in complex test scenarios
/// Range: 100-199
pub const TEMP_LOCAL_BASE_ID: u32 = 100;

/// Local IDs for property test memory operations
/// Range: 210-212
pub const PROP_TEST_OFFSET_LOCAL_ID: u32 = 210;
pub const PROP_TEST_SIZE_LOCAL_ID: u32 = 211;

// Reserved ranges for future test expansions:
// Range: 220-229 - Available for storage tests
// Range: 230-239 - Available for control flow tests
// Range: 240-249 - Available for call/create tests
// Range: 250-255 - Available for gas metering tests

// Storage test values

/// Key chosen to be non-zero and recognizable in debug output
pub const TEST_STORAGE_KEY: u64 = 42;
/// Value chosen to be distinct and recognizable (leet-speak for debugging)
pub const TEST_STORAGE_VALUE: u64 = 1337;

/// Standard test values of different magnitudes
pub const TEST_VALUE_SMALL: u32 = 42; // Common test value
pub const TEST_VALUE_MEDIUM: u32 = 100; // Decimal round number
pub const TEST_VALUE_LARGE: u32 = 256; // 2^8, tests byte boundaries

/// Common arithmetic test operands
pub const TEST_OPERAND_A: u32 = 10;
pub const TEST_OPERAND_B: u32 = 5;
pub const TEST_OPERAND_C: u32 = 20;
pub const TEST_DIVISOR: u32 = 7;

/// Expected results for common operations
pub const TEST_ADD_RESULT: u32 = 15; // 10 + 5
pub const TEST_SDIV_RESULT: u32 = 2; // 10 / 5
pub const TEST_ADDMOD_RESULT: u32 = 2; // (10 + 20) % 7
pub const TEST_MULMOD_RESULT: u32 = 2; // (5 * 6) % 7 = 30 % 7 = 2

/// Bit operation test values
pub const TEST_BIT_PATTERN_A: u32 = 0xFF;
pub const TEST_BIT_PATTERN_B: u32 = 0xF0;
pub const TEST_BIT_PATTERN_C: u32 = 0x0F;
pub const TEST_BIT_PATTERN_D: u32 = 0xAA;
pub const TEST_BIT_PATTERN_E: u32 = 0x55;

/// Shift operation test values
pub const TEST_SHIFT_AMOUNT: u32 = 4;
pub const TEST_SHIFT_VALUE: u32 = 16;
pub const TEST_SHIFT_RESULT: u32 = 256; // 16 << 4
pub const TEST_SAR_VALUE: u32 = 0x8000_0000;
pub const TEST_SAR_RESULT: u32 = 0x4000_0000;

/// Mask for extracting single byte (0xFF = 255)
pub const TEST_BYTE_MASK: u32 = 0xFF;

/// EVM word size in bytes (256 bits / 8)
pub const EVM_WORD_SIZE_BYTES: u32 = 32;
/// Starting offset for memory operations in tests
/// Set high enough to avoid conflicts with test locals (allows ~15 locals + free mem pointer)
pub const MEMORY_START_OFFSET: u32 = 0x200;
/// Common memory offset for tests
pub const TEST_MEMORY_OFFSET: u32 = 0x220;

/// Common values for control flow and misc operations
pub const TEST_REVERT_ERROR_CODE: u32 = 0xBADC0DE;
pub const TEST_RETURN_VALUE: u32 = 0xDEADBEEF;
pub const TEST_CALL_GAS: u32 = 1000;
pub const TEST_CALL_ADDRESS: u32 = 0x2000;

/// Number of iterations for performance benchmarking tests
pub const PERF_TEST_ITERATIONS: usize = 100;

/// Standard test values for binary operations
pub const BINARY_OP_TEST_CASES: [(u32, u32); 5] =
    [(10, 20), (0, 0), (u32::MAX, 1), (100, 0), (0, 100)];

/// Default test cases for unary operations
pub const UNARY_OP_TEST_CASES: [u32; 5] = [0, 1, 42, 255, u32::MAX];

/// Test values for signed arithmetic operations
pub const SIGNED_OP_TEST_CASES: [(u32, u32); 8] = [
    (10, 20),
    (20, 10),
    (7, 3),
    (100, 7),
    (0, 10),
    (10, 1),
    (u32::MAX, 2),
    (u32::MAX - 1, u32::MAX),
];
