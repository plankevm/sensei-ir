# Instructions for AI Agents Working This Repository

## Project Overview

This project defines the EthIR intermediate representation for compilers looking to target the EVM. It defines the IR itself, useful analysis functions, optimizations passes as well as backends to target different EVM versions. The repository is in an early stage of development.

## Development Commands
### Check Code While Working on Tasks
```bash
# Checks if code compiles without building
cargo check --workspace
```

### Check Code to Verify Whether the Task was completed correctly

```bash
# Runs *all* tests, also ensures code compiles.
cargo test --workspace

# Format code
cargo +nightly fmt --workspace

# Run clippy for linting
cargo +nightly clippy --workspace --all --all-features --locked -- -D warnings
```

## Coding Style
### Type Driven Development
- leverage Rust enums & matches to avoid redundant control flow and nonsensical states
- leverage the "new type" pattern to create more specific versions of less-specific types (for indices/dense IDs use `crates/data/index.rs`)

### Security-Centric Development
- if a bug in a given component could result in a miscompilation or other critical flaw it's considered **security critical**
- attempt to minimize **security critical** code
- separate **security critical** and noncritical code to minimize the lines of code external auditors need to verify
- prioritize readablity & maintainablity over compile-time efficiency

### Data Oriented Design
*For components that are performance critical* leverage data oriented design, prioritize data definitions that lead to continuous, dense memory representations. This improves cache efficiency.

### Refactors and Delusions
- If something is no longer needed or unused don't simply underscore it or commented out, delete it unless truly required by an external api or user facing api that would cause breaking changes

## Workspace Structure
- **`/crates/data`** (package: `eth-ir-data`): Core IR data structure
- **`/test-utils`**: Utilities that simplify writing tests
  - IR Parser

## Documentation
### EVM Reference
- [`docs/evm_opcodes_and_precompiles.md`](docs/evm_opcodes_and_precompiles.md)
- [`docs/opcode_fork_mapping.md`](docs/opcode_fork_mapping.md)