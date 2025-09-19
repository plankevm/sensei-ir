//! Memory layout management for local variables
//!
//! Memory layout:
//! - 0x00-0x40: Scratch space (used by hashing operations)
//! - 0x40-0x60: Free memory pointer
//! - 0x60-0x80: Zero slot (reserved for returns)
//! - 0x80+: Local variables (32 bytes each)

use eth_ir_data::LocalId;
use std::collections::HashMap;

/// Type alias for EVM memory addresses
pub type MemoryAddress = u32;

/// Constants for memory layout
pub mod constants {
    use super::MemoryAddress;

    /// Location of the free memory pointer (standard Solidity convention)
    pub const FREE_MEM_PTR: MemoryAddress = 0x40;

    /// Location where return addresses are stored for internal calls
    /// TODO: Will be removed when stack window optimization is implemented
    pub const RETURN_ADDR_SLOT: MemoryAddress = 0x60;

    /// Start of local variable memory region
    pub const LOCALS_START: MemoryAddress = 0x80;

    /// Size of each memory slot for locals (32 bytes)
    pub const SLOT_SIZE: MemoryAddress = 0x20;
}

/// Manages memory allocation for local variables
pub struct MemoryLayout {
    /// Maps LocalId to memory address
    locals: HashMap<LocalId, MemoryAddress>,
    /// Next available memory address for locals
    next_addr: MemoryAddress,
}

impl MemoryLayout {
    pub fn new() -> Self {
        Self { locals: HashMap::new(), next_addr: constants::LOCALS_START }
    }

    /// Allocate memory for a local variable
    pub fn allocate_local(&mut self, local: LocalId) -> MemoryAddress {
        if let Some(&addr) = self.locals.get(&local) {
            addr
        } else {
            let addr = self.next_addr;
            self.locals.insert(local, addr);
            self.next_addr += constants::SLOT_SIZE;
            addr
        }
    }

    /// Get the memory address for a local
    pub fn get_local_address(&self, local: LocalId) -> Option<MemoryAddress> {
        self.locals.get(&local).copied()
    }

    /// Get the address where free memory starts (after all locals)
    pub fn get_free_memory_start(&self) -> MemoryAddress {
        self.next_addr
    }
}
