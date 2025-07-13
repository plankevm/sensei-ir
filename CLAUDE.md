# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Ethereum IR (Intermediate Representation) compiler/backend project written in Rust. It defines an IR for EVM bytecode and provides a backend for compiling this IR to actual EVM bytecode.

## Development Commands

### Code Quality
```bash
# Check if code compiles without building
cargo check

# Run clippy for linting
cargo +nightly clippy --workspace --all --all-features --locked -- -D warnings

# Format code
cargo fmt +nightly
```

### Testing
```bash
cargo test
```

## Architecture

### Workspace Structure
- **`/crates/data`** (package: `eth-ir-data`): Core IR data structures
  - Defines IR types: functions, basic blocks, operations
  - Local variable management with `LocalId` system
  - Support for EVM opcodes and control flow
  - Memory-efficient operation encoding (16 bytes max, 4-byte aligned)

### Key IR Concepts
- **Program**: Top-level container using data-oriented design with contiguous storage
- **Functions**: Entry points with basic block references and output counts
- **Basic Blocks**: Control flow units with:
  - Input/output locals (stored as ranges in program arrays)
  - Operations (stored as range in program operations array)
  - Control flow (continues, branches, switches, or terminates)
- **Operations**: All operations fit in 16 bytes with 4-byte alignment:
  - EVM opcodes (arithmetic, comparison, environmental, etc.)
  - IR memory primitives (allocation, load/store with 1-32 byte sizes)
  - Simple statements (local assignment, constants)
  - Internal calls
- **Local Variables**: Single-assignment variables identified by `LocalId`
- **Index Types**: Type-safe indices using `NonZero<u32>` for zero-cost abstractions

### Important Types
- `Program`: Contains all IR data in indexed vectors
  - `entry`: The entry point function for the program
  - `functions`: Function definitions
  - `basic_blocks`: Basic block definitions
  - `operations`: All operations
  - `large_opcode_locals`: Storage for operations with >3 arguments
  - `data_bytes`: Raw data storage
  - `large_consts`: U256 constants
  - `cases`: Switch statement cases
- `Operation`: Enum of all possible operations (≤16 bytes)
  - Includes all EVM opcodes except: SWAP*, DUP*, POP, PC, MSIZE
  - Memory operations abstracted to support 1-32 byte sizes
  - Uses specialized structs to maintain size constraints
- `Control`: Basic block termination variants
- `MemoryLoad/MemoryStore`: Flexible memory operations with `byte_size` field (1-32)
- Index types: `LocalId`, `BasicBlockId`, `FunctionId`, `OperationIndex`, etc.

### Design Decisions
- **Memory Efficiency**: Operation enum constrained to 16 bytes with 4-byte alignment
- **Data-Oriented Design**: All data stored contiguously in Program arrays, entities reference ranges
- **Type Safety**: Index types prevent mixing different kinds of indices
- **Flexibility**: Memory operations support any byte size from 1-32, not just EVM's 1 and 32


## Development Notes

- Uses Rust 2024 edition
- Dependencies include `alloy-primitives` for Ethereum types and `evm-glue` for opcodes
- The project is work-in-progress (commit: "WIP initial IR")
- No test infrastructure currently exists
