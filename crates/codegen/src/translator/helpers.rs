//! Helper methods for assembly generation

use super::{Translator, marks::MarkId};
use crate::error::{CodegenError, Result};
use alloy_primitives::U256;
use eth_ir_data::LocalId;
use evm_glue::assembly::{Asm, MarkRef, RefType};

impl Translator {
    /// Push a constant using the smallest PUSH opcode
    pub(super) fn push_const(&mut self, value: U256) {
        use evm_glue::opcodes::Opcode;

        // Get the minimal byte representation
        if value.is_zero() {
            self.asm.push(Asm::Op(Opcode::PUSH0));
        } else {
            // Get minimal big-endian byte representation
            let trimmed = value.to_be_bytes_trimmed_vec();
            let len = trimmed.len();

            // Use the appropriate PUSH opcode based on the number of bytes
            match len {
                1 => self.asm.push(Asm::Op(Opcode::PUSH1([trimmed[0]]))),
                2 => {
                    let mut arr = [0u8; 2];
                    arr.copy_from_slice(&trimmed);
                    self.asm.push(Asm::Op(Opcode::PUSH2(arr)));
                }
                3 => {
                    let mut arr = [0u8; 3];
                    arr.copy_from_slice(&trimmed);
                    self.asm.push(Asm::Op(Opcode::PUSH3(arr)));
                }
                4 => {
                    let mut arr = [0u8; 4];
                    arr.copy_from_slice(&trimmed);
                    self.asm.push(Asm::Op(Opcode::PUSH4(arr)));
                }
                5 => {
                    let mut arr = [0u8; 5];
                    arr.copy_from_slice(&trimmed);
                    self.asm.push(Asm::Op(Opcode::PUSH5(arr)));
                }
                6 => {
                    let mut arr = [0u8; 6];
                    arr.copy_from_slice(&trimmed);
                    self.asm.push(Asm::Op(Opcode::PUSH6(arr)));
                }
                7 => {
                    let mut arr = [0u8; 7];
                    arr.copy_from_slice(&trimmed);
                    self.asm.push(Asm::Op(Opcode::PUSH7(arr)));
                }
                8 => {
                    let mut arr = [0u8; 8];
                    arr.copy_from_slice(&trimmed);
                    self.asm.push(Asm::Op(Opcode::PUSH8(arr)));
                }
                // For larger values, just use PUSH32 with full bytes
                _ => {
                    let arr: [u8; 32] = value.to_be_bytes();
                    self.asm.push(Asm::Op(Opcode::PUSH32(arr)));
                }
            }
        }
    }

    /// Validate that a range of locals exists
    pub(super) fn validate_local_range(&self, start: usize, required: usize) -> Result<()> {
        let end = start + required;
        if end > self.program.locals.len() {
            return Err(CodegenError::InvalidLocalRange {
                range: start..end,
                locals_len: self.program.locals.len(),
            });
        }
        Ok(())
    }

    /// Load a local from memory to stack
    pub(super) fn load_local(&mut self, local: LocalId) -> Result<()> {
        use evm_glue::opcodes::Opcode;

        if let Some(addr) = self.memory.get_local_address(local) {
            // Push memory address
            self.push_const(U256::from(addr));
            // Load from memory
            self.asm.push(Asm::Op(Opcode::MLOAD));
            Ok(())
        } else {
            Err(CodegenError::LocalNotFound { local })
        }
    }

    /// Store stack value to local in memory
    pub(super) fn store_local(&mut self, local: LocalId) -> Result<()> {
        use evm_glue::opcodes::Opcode;

        if let Some(addr) = self.memory.get_local_address(local) {
            // Stack: [value]
            // Push memory address
            self.push_const(U256::from(addr));
            // Stack: [value, addr]
            self.asm.push(Asm::Op(Opcode::MSTORE));
            Ok(())
        } else {
            Err(CodegenError::LocalNotFound { local })
        }
    }

    /// Emit a jump destination mark
    pub(super) fn emit_mark(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        // Emit the mark for evm-glue to track
        self.asm.push(Asm::Mark(mark_id));

        // Emit JUMPDEST opcode - required by EVM at jump targets
        self.asm.push(Asm::Op(Opcode::JUMPDEST));
    }

    /// Emit an unconditional jump
    pub(super) fn emit_jump(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        // Push the mark reference
        self.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(mark_id),
            is_pushed: true,
            set_size: None,
        }));
        // Jump to it
        self.asm.push(Asm::Op(Opcode::JUMP));
    }

    /// Emit a conditional jump (non-zero = jump)
    pub(super) fn emit_jumpi(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        // Stack should have: [condition]
        // Push the mark reference
        self.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(mark_id),
            is_pushed: true,
            set_size: None,
        }));
        // Stack: [condition, destination]
        // Conditional jump
        self.asm.push(Asm::Op(Opcode::JUMPI));
    }

    /// Emit a runtime error revert
    pub(super) fn emit_runtime_error(&mut self, error_code: u8) {
        use evm_glue::opcodes::Opcode;

        // Store error code at memory address 0x00
        self.push_const(U256::from(error_code));
        self.push_const(U256::from(0x00));
        self.asm.push(Asm::Op(Opcode::MSTORE8));

        // REVERT with the error code
        // offset = 0x00, size = 1
        self.push_const(U256::from(1)); // size
        self.push_const(U256::from(0)); // offset
        self.asm.push(Asm::Op(Opcode::REVERT));
    }
}
