# Fuzz Testing for eth-ir-codegen

This directory contains fuzz testing for the eth-ir-codegen translator using cargo-fuzz.

## Overview

The fuzz test generates IR structures directly to test the translator, bypassing the parser. This approach:
- Tests the translator in isolation
- Generates both valid and edge-case IR programs
- Ensures the translator never panics, even with unusual inputs

## Installation

Install cargo-fuzz if you haven't already:
```bash
cargo install cargo-fuzz
```

## Running the Fuzzer

```bash
cd crates/codegen/fuzz
cargo +nightly fuzz run fuzz
```

## Fuzzer Features

The fuzzer provides comprehensive testing with excellent maintainability:
- **Multiple Program Types**: Control flow, memory stress, edge cases, and complex programs
- **Control Flow Testing**: Multi-block programs with branches, switches, and internal calls
- **Edge Case Detection**: Division by zero, invalid references, out-of-bounds operations
- **Memory Stress Testing**: Various allocation sizes, zero-size allocations, over-limit allocations
- **Advanced Operations**: Signed arithmetic, bitwise operations, crypto operations
- **Multiple Translator Configurations**: Tests with different bounds checking and debug settings
- **Invalid Operation Handling**: Tests with invalid references and malformed structures
- **Modular architecture**: Separate builders for each program type (maintainable 50-line functions)
- **Composable operation generators**: Reusable components reduce code duplication
- **Centralized configuration**: Magic numbers replaced with named constants
- **Strategy pattern**: Easy to add new program generation strategies
- **Better debugging**: Isolated components make issues easier to trace

## Running with Options

```bash
# Run for 10 minutes
cargo +nightly fuzz run fuzz -- -max_total_time=600

# Run with multiple workers
cargo +nightly fuzz run fuzz -- -workers=4

# Run continuously until a crash is found
cargo +nightly fuzz run fuzz -- -runs=-1
```

## Analyzing Results

### If a crash is found

Crashes are saved in `artifacts/`:
```bash
# List crashes
ls artifacts/fuzz/

# Reproduce a specific crash
cargo +nightly fuzz run fuzz artifacts/fuzz/crash-abc123
```

### Coverage

Check what code paths are being tested:
```bash
# Build with coverage
cargo +nightly fuzz coverage fuzz

# Generate coverage report
cargo +nightly fuzz coverage fuzz -- -runs=100000
```

## Usage Recommendations

The fuzzer automatically adapts its testing strategy based on input data:
- **Basic programs**: Simple linear operations for quick validation
- **Control flow**: Multi-block programs with branches and switches
- **Memory stress**: Various allocation patterns and edge cases
- **Edge cases**: Division by zero, invalid references, boundary conditions
- **Complex programs**: Large constants, data segments, advanced operations

Perfect for:
- **Daily development**: Easy to debug and extend
- **CI/CD pipelines**: Reliable and maintainable
- **Regression testing**: Comprehensive coverage with clear failure isolation
- **New feature testing**: Easy to add new operation types and edge cases
- **Release validation**: Thorough testing of edge cases and complex scenarios

## Future Improvements

Future enhancements could include:
- Function-level fuzzing with call graphs
- Property-based testing with invariant checking
- Differential fuzzing against multiple backends
- Performance regression detection
- Automated crash triaging and minimization

The fuzzer provides comprehensive testing capabilities with excellent maintainability and extensibility through its modular architecture.