//! Code generation from eth-ir to EVM bytecode
//!
//! This crate translates eth-ir programs to EVM assembly using evm-glue.
//!
//! Design approach:
//! - Memory-backed locals for simplicity (future: stack window optimization)
//! - Each LocalId gets a fixed memory slot
//! - Memory layout: 0x00-0x40 scratch, 0x40-0x60 free ptr, 0x60-0x80 return addr, 0x80+ locals

mod error;
mod gas;
mod translator;

#[cfg(test)]
mod tests;

// Public exports
pub use error::{CodegenError, Result, runtime as runtime_errors};
pub use gas::{AdvancedGasEstimator, AdvancedGasReport, GasReport, SimpleGasEstimator};
pub use translator::{Translator, translate_program};
