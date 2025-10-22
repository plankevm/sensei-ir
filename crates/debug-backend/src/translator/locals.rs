//! Local variable memory management
//!
//! TODO: Implement stack-based local storage for better performance.

use alloy_primitives::U256;
use evm_glue::{assembly::Asm, opcodes::Opcode};
use sir_data::{Idx, IndexVec, LocalId};

/// Memory allocation for local variables
/// Memory layout:
/// - 0x00+: Locals (word-aligned)
/// - After locals: Free memory pointer
/// - After pointer: Dynamic allocations
pub struct LocalStorage {
    locals: IndexVec<LocalId, Option<u32>>,
    next_address: u32,
    free_memory_pointer_location: Option<u32>,
}

impl LocalStorage {
    pub fn with_capacity(num_locals: usize) -> Self {
        Self {
            locals: IndexVec::from_vec(vec![None; num_locals]),
            next_address: 0,
            free_memory_pointer_location: None,
        }
    }

    /// Allocate word-aligned memory address for a local (idempotent)
    pub fn allocate(&mut self, local: LocalId) -> u32 {
        use super::constants;

        let idx = local.index();

        if idx >= self.locals.len() {
            self.locals.resize(idx + 1, None);
        }

        if let Some(address) = self.locals[idx] {
            return address;
        }

        let address = self.next_address;
        self.locals[idx] = Some(address);
        self.next_address += constants::EVM_WORD_SIZE as u32;
        address
    }

    pub fn finalize_allocations(&mut self) {
        self.free_memory_pointer_location = Some(self.next_address);
    }

    pub fn get_initial_free_memory_value(&self) -> u32 {
        use super::constants;
        let ptr_loc = self
            .free_memory_pointer_location
            .expect("finalize_allocations() must be called before getting free memory values");
        ptr_loc
            .checked_add(constants::EVM_WORD_SIZE as u32)
            .expect("Free memory pointer calculation overflow")
    }

    pub fn get_free_memory_pointer_location(&self) -> u32 {
        self.free_memory_pointer_location.expect(
            "finalize_allocations() must be called before getting free memory pointer location",
        )
    }

    pub fn generate_load(&mut self, local: LocalId, asm: &mut Vec<Asm>) {
        let address = self.allocate(local);
        Self::push_address(address, asm);
        asm.push(Asm::Op(Opcode::MLOAD));
    }

    pub fn generate_store(&mut self, local: LocalId, asm: &mut Vec<Asm>) {
        let address = self.locals[local].expect("Generating store for unallocated local");
        Self::push_address(address, asm);
        asm.push(Asm::Op(Opcode::MSTORE));
    }

    /// Push an address constant using the smallest PUSH opcode
    fn push_address(address: u32, asm: &mut Vec<Asm>) {
        let value = U256::from(address);

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
            _ => unreachable!("Memory addresses fit in 4 bytes"),
        }
    }
}
