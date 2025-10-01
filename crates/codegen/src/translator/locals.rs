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
/// # Invariants
///
/// - All allocated addresses are aligned to `EVM_WORD_SIZE` (32 bytes)
/// - Addresses start at `EVM_SCRATCH_SPACE_END` (0x80) which is word-aligned
/// - Each allocation increments by `EVM_WORD_SIZE`, maintaining alignment
/// - Once a LocalId is allocated, calling `allocate()` again returns the same address
///
/// Uses a Vec for O(1) indexed access instead of HashMap for better performance
pub struct LocalStorage {
    /// Maps each local to its memory address (indexed by LocalId)
    /// None means the local hasn't been allocated yet
    locals: Vec<Option<u32>>,
    /// Next available memory address for locals
    /// Invariant: Always word-aligned (multiple of EVM_WORD_SIZE)
    next_address: u32,
}

impl LocalStorage {
    /// Create a new local storage manager
    ///
    /// Starts with an empty allocation table and address at `EVM_SCRATCH_SPACE_END`.
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
    ///
    /// Pre-allocates space for `num_locals` entries to avoid resizing.
    ///
    /// # Edge Cases
    ///
    /// - If `num_locals` is 0, behaves the same as `new()`
    /// - Pre-allocation is a hint; the vector will grow if needed
    pub fn with_capacity(num_locals: usize) -> Self {
        use super::constants;

        Self { locals: vec![None; num_locals], next_address: constants::EVM_SCRATCH_SPACE_END }
    }

    /// Allocate memory for a local variable
    ///
    /// Returns the memory address for the given local. If the local has already been
    /// allocated, returns the existing address. Otherwise, allocates a new word-aligned
    /// address and returns it.
    ///
    /// # Edge Cases
    ///
    /// - Calling multiple times with the same `LocalId` returns the same address (idempotent)
    /// - Automatically grows internal storage if `local.index()` exceeds current capacity
    /// - Panics if address space is exhausted (would require 134+ million locals)
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
        self.next_address = self
            .next_address
            .checked_add(constants::EVM_WORD_SIZE as u32)
            .expect("Local storage address overflow - too many locals allocated");
        address
    }

    /// Get the memory address for a local (allocating if needed)
    ///
    /// Convenience method that delegates to `allocate()`.
    ///
    /// # Edge Cases
    ///
    /// - Same behavior as `allocate()` - see that method for details
    pub fn get_location(&mut self, local: LocalId) -> u32 {
        self.allocate(local)
    }

    /// Get the initial value for the free memory pointer
    ///
    /// Returns the address where dynamic allocations should begin, which is
    /// one word after all allocated locals (to account for the free memory pointer itself).
    ///
    /// # Edge Cases
    ///
    /// - If no locals have been allocated, returns `EVM_SCRATCH_SPACE_END + EVM_WORD_SIZE`
    /// - Panics if the calculation would overflow (requires 134+ million locals)
    pub fn get_initial_free_memory_value(&self) -> u32 {
        use super::constants;
        // Dynamic memory starts after locals and the free memory pointer itself
        self.next_address
            .checked_add(constants::EVM_WORD_SIZE as u32)
            .expect("Free memory pointer calculation overflow")
    }

    /// Get the free memory pointer location
    ///
    /// Returns the address where the free memory pointer is stored, which is
    /// immediately after all allocated locals.
    ///
    /// # Edge Cases
    ///
    /// - If no locals have been allocated, returns `EVM_SCRATCH_SPACE_END` (0x80)
    pub fn get_free_memory_pointer_location(&self) -> u32 {
        self.next_address
    }

    /// Generate assembly to load a local onto the stack
    ///
    /// Generates: `PUSH<address> MLOAD`
    ///
    /// # Edge Cases
    ///
    /// - If `asm` is empty, works correctly (no special case needed)
    /// - Allocates the local if not already allocated
    pub fn generate_load(&mut self, local: LocalId, asm: &mut Vec<Asm>) -> Result<()> {
        let address = self.get_location(local);
        super::helpers::push_const_impl(U256::from(address), asm);
        asm.push(Asm::Op(Opcode::MLOAD));
        Ok(())
    }

    /// Generate assembly to store stack value to a local
    ///
    /// Generates: `PUSH<address> MSTORE`
    ///
    /// # Edge Cases
    ///
    /// - If `asm` is empty, works correctly (no special case needed)
    /// - Allocates the local if not already allocated
    pub fn generate_store(&mut self, local: LocalId, asm: &mut Vec<Asm>) -> Result<()> {
        let address = self.get_location(local);
        super::helpers::push_const_impl(U256::from(address), asm);
        asm.push(Asm::Op(Opcode::MSTORE));
        Ok(())
    }
}
