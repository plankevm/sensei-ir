//! EVM Bytecode Generator for Eth-IR
//!
//! This crate provides a code generator that compiles Eth-IR intermediate representation
//! into EVM bytecode. The implementation is specifically designed for the Ethereum Virtual
//! Machine and makes use of EVM-specific features and constraints:
//!
//! ## Memory Layout
//! - `0x00-0x7F`: Reserved EVM scratch space (used by opcodes like SHA3, RETURNDATACOPY)
//! - `0x80+`: Local variables (temporary - will be moved to stack)
//! - After locals: Free memory pointer for dynamic allocations
//!
//! ## Design Decisions
//! While Eth-IR aims to be an abstract intermediate representation, this code generator
//! is intentionally EVM-specific to produce efficient bytecode. Key EVM assumptions:
//! - 32-byte (256-bit) word size
//! - Stack-based architecture with 1024 depth limit
//! - Memory expansion gas costs
//! - Specific opcode semantics and gas costs
//!
//! TODO: Implement stack-based locals for better performance.

mod error;
mod gas;
mod translator;

#[cfg(test)]
mod tests;

// Public exports
pub use error::{CodegenError, Result, runtime as runtime_errors};
pub use gas::{AdvancedGasEstimator, AdvancedGasReport, GasReport, SimpleGasEstimator};
pub use translator::{Translator, translate_program};
