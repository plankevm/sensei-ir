//! EVM Bytecode Generator for Eth-IR
//!
//! Compiles Eth-IR intermediate representation into EVM bytecode.
//!
//! ## Memory Layout
//! - `0x00+`: Local variables
//! - After locals: Free memory pointer + dynamic allocations
//!
//! TODO: Implement stack-based locals for better performance.

mod translator;

#[cfg(test)]
mod tests;

pub mod runtime_errors {
    pub const UNDEFINED_BEHAVIOR: u8 = 0x00;
    pub const SWITCH_NO_MATCH: u8 = 0x01;
}

#[derive(Debug, Clone, Default)]
pub struct Config {
    pub panic_on_untranslated_blocks: bool,
}

pub use translator::{Translator, translate_program};
