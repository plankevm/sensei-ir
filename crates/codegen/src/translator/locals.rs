//! Local variable memory management
//!
//! This module manages memory allocation for local variables in the generated EVM bytecode.
//!
//! TODO: Implement stack-based local storage for better performance.
//! Currently uses memory-based storage for simplicity.

use crate::error::Result;
use alloy_primitives::U256;
use eth_ir_data::{Idx, LocalId};
use evm_glue::{assembly::Asm, opcodes::Opcode};

/// Manages memory allocation for local variables in EVM
///
/// This implementation uses EVM memory for local storage. Memory layout:
/// - 0x00-0x7F: Reserved for EVM scratch space (used by opcodes like SHA3)
/// - 0x80+: Local variables storage
/// - After locals: Free memory pointer (4 bytes)
/// - After pointer: Dynamic allocations
///
/// Uses a Vec for O(1) indexed access instead of HashMap for better performance
pub struct LocalStorage {
    /// Maps each local to its memory address (indexed by LocalId)
    /// None means the local hasn't been allocated yet
    locals: Vec<Option<u32>>,
    /// Next available memory address for locals
    next_address: u32,
}

impl LocalStorage {
    /// Create a new local storage manager
    pub fn new() -> Self {
        use super::constants;

        Self {
            locals: Vec::new(),
            // TODO: Replace memory-based locals with stack-based approach
            // Start after EVM scratch space to avoid conflicts with opcodes like SHA3
            next_address: constants::EVM_SCRATCH_SPACE_END,
        }
    }

    /// Create with pre-allocated capacity
    pub fn with_capacity(num_locals: usize) -> Self {
        use super::constants;

        Self { locals: vec![None; num_locals], next_address: constants::EVM_SCRATCH_SPACE_END }
    }

    /// Allocate memory for a local variable
    pub fn allocate(&mut self, local: LocalId) -> u32 {
        use super::constants;

        let idx = local.index();

        // Grow the vector if necessary
        if idx >= self.locals.len() {
            self.locals.resize(idx + 1, None);
        }

        // Return existing allocation if already allocated
        if let Some(address) = self.locals[idx] {
            return address;
        }

        // Allocate new address
        let address = self.next_address;
        self.locals[idx] = Some(address);
        self.next_address += constants::EVM_WORD_SIZE as u32;
        address
    }

    /// Get the memory address for a local (allocating if needed)
    pub fn get_location(&mut self, local: LocalId) -> u32 {
        self.allocate(local)
    }

    /// Get the initial value for the free memory pointer
    /// This is where dynamic allocations will start
    pub fn get_initial_free_memory_value(&self) -> u32 {
        use super::constants;
        // Dynamic memory starts after locals and the free memory pointer itself
        self.next_address + constants::EVM_WORD_SIZE as u32
    }

    /// Get the free memory pointer location
    /// We store it right after the locals
    pub fn get_free_memory_pointer_location(&self) -> Option<u32> {
        Some(self.next_address)
    }

    /// Generate assembly to load a local onto the stack
    pub fn generate_load(&mut self, local: LocalId, asm: &mut Vec<Asm>) -> Result<()> {
        let address = self.get_location(local);
        push_const(U256::from(address), asm);
        asm.push(Asm::Op(Opcode::MLOAD));
        Ok(())
    }

    /// Generate assembly to store stack value to a local
    pub fn generate_store(&mut self, local: LocalId, asm: &mut Vec<Asm>) -> Result<()> {
        let address = self.get_location(local);
        push_const(U256::from(address), asm);
        asm.push(Asm::Op(Opcode::MSTORE));
        Ok(())
    }
}

/// Helper function to push a U256 constant using optimal opcode
fn push_const(value: U256, asm: &mut Vec<Asm>) {
    if value.is_zero() {
        asm.push(Asm::Op(Opcode::PUSH0));
        return;
    }

    let trimmed = value.to_be_bytes_trimmed_vec();

    macro_rules! push_n {
        ($n:expr, $opcode:ident) => {{
            let mut arr = [0u8; $n];
            arr.copy_from_slice(&trimmed[..]);
            asm.push(Asm::Op(Opcode::$opcode(arr)));
        }};
    }

    match trimmed.len() {
        1 => asm.push(Asm::Op(Opcode::PUSH1([trimmed[0]]))),
        2 => push_n!(2, PUSH2),
        3 => push_n!(3, PUSH3),
        4 => push_n!(4, PUSH4),
        _ => {
            asm.push(Asm::Op(Opcode::PUSH32(value.to_be_bytes())));
        }
    }
}
