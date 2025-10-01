//! Error types and runtime error codes for code generation

use eth_ir_data::{DataId, Idx};
use std::fmt;

/// Runtime error codes encoded as a single byte in revert data
pub mod runtime {
    /// Undefined behavior (e.g., reading uninitialized memory, allocation overflow)
    pub const UNDEFINED_BEHAVIOR: u8 = 0x00;

    /// Non-exhaustive switch without matching case
    pub const SWITCH_NO_MATCH: u8 = 0x01;
}

/// Error type for code generation
#[derive(Debug)]
pub enum CodegenError {
    /// Data segment not found
    DataSegmentNotFound { segment: DataId },
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodegenError::DataSegmentNotFound { segment } => {
                write!(f, "data segment {} not found", segment.index())
            }
        }
    }
}

impl std::error::Error for CodegenError {}

/// Result type for code generation operations
pub type Result<T> = std::result::Result<T, CodegenError>;
