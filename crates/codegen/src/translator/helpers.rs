//! Helper methods for assembly generation

use super::{Translator, marks::MarkId};
use crate::error::{CodegenError, Result};
use alloy_primitives::U256;
use eth_ir_data::{LocalId, operation::HasArgs};
use evm_glue::assembly::{Asm, MarkRef, RefType};

impl Translator {
    /// Push a constant using the smallest PUSH opcode
    pub(super) fn push_const(&mut self, value: U256) {
        // Pushing a constant invalidates our last_loaded tracking
        self.state.last_loaded = None;

        use evm_glue::opcodes::Opcode;

        // Get the minimal byte representation
        if value.is_zero() {
            self.state.asm.push(Asm::Op(Opcode::PUSH0));
            return;
        }

        // Get minimal big-endian byte representation
        let trimmed = value.to_be_bytes_trimmed_vec();

        // Helper macro to create array and push opcode
        macro_rules! push_n {
            ($n:expr, $opcode:ident) => {{
                let mut arr = [0u8; $n];
                arr.copy_from_slice(&trimmed[..]);
                self.state.asm.push(Asm::Op(Opcode::$opcode(arr)));
            }};
        }

        // Use the appropriate PUSH opcode based on the number of bytes
        match trimmed.len() {
            1 => self.state.asm.push(Asm::Op(Opcode::PUSH1([trimmed[0]]))),
            2 => push_n!(2, PUSH2),
            3 => push_n!(3, PUSH3),
            4 => push_n!(4, PUSH4),
            5 => push_n!(5, PUSH5),
            6 => push_n!(6, PUSH6),
            7 => push_n!(7, PUSH7),
            8 => push_n!(8, PUSH8),
            9..=32 => {
                // For values larger than 8 bytes, use PUSH32 with full representation
                let arr: [u8; 32] = value.to_be_bytes();
                self.state.asm.push(Asm::Op(Opcode::PUSH32(arr)));
            }
            _ => {
                // This should never happen as U256 is max 32 bytes
                debug_assert!(false, "U256 value has more than 32 bytes");
                let arr: [u8; 32] = value.to_be_bytes();
                self.state.asm.push(Asm::Op(Opcode::PUSH32(arr)));
            }
        }
    }

    /// Validate that a range of locals exists
    pub(super) fn validate_local_range(&self, start: usize, required: usize) -> Result<()> {
        let end = start + required;
        if end > self.program.program.locals.len() {
            return Err(CodegenError::InvalidLocalRange {
                range: start..end,
                locals_len: self.program.program.locals.len(),
            });
        }
        Ok(())
    }

    /// Load a local onto the stack
    pub(super) fn load_local(&mut self, local: LocalId) -> Result<()> {
        // TODO: Implement proper redundant load optimization after analyzing data flow
        // For now, always load from memory to ensure correctness
        self.state.last_loaded = None;
        self.state.locals.generate_load(local, &mut self.state.asm)
    }

    /// Store stack value to a local
    pub(super) fn store_local(&mut self, local: LocalId) -> Result<()> {
        // Storing invalidates our last_loaded tracking
        self.state.last_loaded = None;
        self.state.locals.generate_store(local, &mut self.state.asm)
    }

    /// Emit a jump destination mark
    pub(super) fn emit_mark(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        // Emit the mark for evm-glue to track
        self.state.asm.push(Asm::Mark(mark_id));

        // Emit JUMPDEST opcode - required by EVM at jump targets
        self.state.asm.push(Asm::Op(Opcode::JUMPDEST));
    }

    /// Emit an unconditional jump
    pub(super) fn emit_jump(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        // Push the mark reference
        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(mark_id),
            is_pushed: true,
            set_size: None,
        }));
        // Jump to it
        self.state.asm.push(Asm::Op(Opcode::JUMP));
    }

    /// Emit a conditional jump (non-zero = jump)
    pub(super) fn emit_jumpi(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        // Stack should have: [condition]
        // Push the mark reference
        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(mark_id),
            is_pushed: true,
            set_size: None,
        }));
        // Stack: [condition, destination]
        // Conditional jump
        self.state.asm.push(Asm::Op(Opcode::JUMPI));
    }

    /// Load multiple locals onto the stack in sequence
    pub(super) fn load_locals_sequence(&mut self, locals: &[LocalId]) -> Result<()> {
        for &local in locals {
            self.load_local(local)?;
        }
        Ok(())
    }

    /// Emit a simple zero-input-one-output operation
    pub(super) fn emit_zero_in_one_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        result: LocalId,
    ) -> Result<()> {
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(result)
    }

    /// Emit a simple one-input-one-output operation
    pub(super) fn emit_one_in_one_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        arg: LocalId,
        result: LocalId,
    ) -> Result<()> {
        self.load_local(arg)?;
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(result)
    }

    /// Emit a three-input-zero-output operation for memory copy operations
    pub(super) fn emit_three_in_zero_copy_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        dest_offset: LocalId,
        src_offset: LocalId,
        size: LocalId,
    ) -> Result<()> {
        self.load_local(dest_offset)?;
        self.load_local(src_offset)?;
        self.load_local(size)?;
        self.state.asm.push(Asm::Op(opcode));
        Ok(())
    }

    /// Emit a two-input-zero-output operation
    pub(super) fn emit_two_in_zero_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        arg1: LocalId,
        arg2: LocalId,
        reversed: bool,
    ) -> Result<()> {
        if reversed {
            self.load_local(arg2)?;
            self.load_local(arg1)?;
        } else {
            self.load_local(arg1)?;
            self.load_local(arg2)?;
        }
        self.state.asm.push(Asm::Op(opcode));
        Ok(())
    }

    /// Emit a three-argument operation with result (like ADDMOD, MULMOD)
    /// Arguments are loaded in reverse order (arg3, arg2, arg1)
    pub(super) fn emit_three_arg_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        args: &[LocalId],
        result: LocalId,
    ) -> Result<()> {
        // Load in reverse order for stack
        self.load_local(args[2])?;
        self.load_local(args[1])?;
        self.load_local(args[0])?;
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(result)
    }

    /// Emit a call operation (CALL, CALLCODE, DELEGATECALL, STATICCALL)
    /// Handles the common pattern of loading args and storing result
    pub(super) fn emit_call_operation<const N: u32>(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        large_in_one: &eth_ir_data::operation::LargeInOneOut<N>,
        num_args: usize,
    ) -> Result<()> {
        let args = large_in_one.get_args(&self.program.program.locals);
        self.load_locals_sequence(&args[0..num_args])?;
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(large_in_one.result) // Store success (0 or 1)
    }

    /// Emit a LOG operation with multiple topics
    /// Loads topics in reverse order for correct stack arrangement
    pub(super) fn emit_log_operation(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        offset: LocalId,
        size: LocalId,
        topics: &[LocalId],
    ) -> Result<()> {
        // Load topics in reverse order
        for &topic in topics.iter().rev() {
            self.load_local(topic)?;
        }
        // Load size and offset
        self.load_local(size)?;
        self.load_local(offset)?;
        self.state.asm.push(Asm::Op(opcode));
        Ok(())
    }

    /// Emit a CREATE operation (CREATE or CREATE2)
    pub(super) fn emit_create_operation<const N: u32>(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        large_in_one: &eth_ir_data::operation::LargeInOneOut<N>,
        num_args: usize,
    ) -> Result<()> {
        let args = large_in_one.get_args(&self.program.program.locals);
        self.load_locals_sequence(&args[0..num_args])?;
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(large_in_one.result) // Store new contract address (or 0)
    }

    /// Perform dynamic memory allocation
    /// Returns the allocated pointer and updates free memory pointer
    pub(super) fn allocate_memory(&mut self, size_local: LocalId, zero_memory: bool) -> Result<()> {
        use super::constants;
        use evm_glue::opcodes::Opcode;

        // Get free memory pointer location
        let ptr_loc = self
            .state
            .locals
            .get_free_memory_pointer_location()
            .expect("Dynamic allocation requires free memory pointer");

        // Load current free memory pointer value
        self.push_const(U256::from(ptr_loc));
        self.state.asm.push(Asm::Op(Opcode::MLOAD));

        // Stack: [current_ptr]
        // Keep a copy for return value
        self.state.asm.push(Asm::Op(Opcode::DUP1));

        // Load size and add to get new pointer
        self.load_local(size_local)?;

        // Check for overflow before adding
        // Stack: [size, current_ptr, current_ptr]

        // Optional: Check if size is unreasonably large
        if self.state.enable_bounds_checking {
            self.state.asm.push(Asm::Op(Opcode::DUP1)); // [size, size, current_ptr, current_ptr]
            self.push_const(U256::from(constants::MAX_ALLOCATION_SIZE));
            self.state.asm.push(Asm::Op(Opcode::GT)); // [size > 1MB, size, current_ptr, current_ptr]

            let size_ok = self.state.marks.allocate_mark();
            self.state.asm.push(Asm::Op(Opcode::ISZERO));
            self.emit_jumpi(size_ok);
            self.emit_runtime_error(crate::error::runtime::MEMORY_ALLOCATION_FAILED);
            self.emit_mark(size_ok);
        }

        self.state.asm.push(Asm::Op(Opcode::DUP2)); // [current_ptr, size, current_ptr, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::ADD)); // [new_ptr, current_ptr, current_ptr]

        // Check if new_ptr < current_ptr (overflow occurred)
        self.state.asm.push(Asm::Op(Opcode::DUP1)); // [new_ptr, new_ptr, current_ptr, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::DUP3)); // [current_ptr, new_ptr, new_ptr, current_ptr, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::LT)); // [overflow?, new_ptr, current_ptr, current_ptr]

        // If overflow, revert
        if self.state.enable_bounds_checking {
            let no_overflow = self.state.marks.allocate_mark();
            self.state.asm.push(Asm::Op(Opcode::ISZERO));
            self.emit_jumpi(no_overflow);
            self.emit_runtime_error(crate::error::runtime::MEMORY_ALLOCATION_FAILED);
            self.emit_mark(no_overflow);
        } else {
            self.state.asm.push(Asm::Op(Opcode::POP)); // Remove overflow check result
        }

        // Stack: [new_ptr, current_ptr, current_ptr]
        // Store new pointer back to memory
        self.push_const(U256::from(ptr_loc));
        self.state.asm.push(Asm::Op(Opcode::MSTORE));

        // Stack: [current_ptr, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::POP)); // Remove duplicate

        // Stack: [current_ptr]
        if zero_memory {
            // Prepare stack for zeroing: need [ptr, size]
            self.state.asm.push(Asm::Op(Opcode::DUP1));
            self.load_local(size_local)?;
            // Stack: [size, current_ptr, current_ptr]
            self.state.asm.push(Asm::Op(Opcode::SWAP1));
            // Stack: [current_ptr, size, current_ptr]
            self.emit_memory_zeroing_loop();
            // Stack: [current_ptr]
        }

        // Stack: [current_ptr] - the allocated pointer
        Ok(())
    }

    /// Emit code to zero memory
    /// Stack before: [ptr, size]
    /// Stack after: []
    /// Zeros exactly 'size' bytes starting at 'ptr'
    pub(super) fn emit_memory_zeroing_loop(&mut self) {
        use evm_glue::opcodes::Opcode;

        // Simple approach: use CALLDATACOPY to zero memory
        // CALLDATACOPY with offset beyond CALLDATASIZE copies zeros

        // Stack: [ptr, size]
        // We need: CALLDATACOPY(destOffset, dataOffset, size)
        // where dataOffset > CALLDATASIZE to get zeros

        self.state.asm.push(Asm::Op(Opcode::SWAP1)); // [size, ptr]
        self.state.asm.push(Asm::Op(Opcode::DUP1)); // [size, size, ptr]
        self.state.asm.push(Asm::Op(Opcode::SWAP2)); // [ptr, size, size]

        // Push a large offset (beyond any reasonable calldata)
        self.push_const(U256::from(0xFFFFFFFFu32)); // [bigOffset, ptr, size, size]
        self.state.asm.push(Asm::Op(Opcode::SWAP1)); // [ptr, bigOffset, size, size]

        // Stack is now [ptr, bigOffset, size, size]
        // CALLDATACOPY expects [destOffset, dataOffset, size]
        self.state.asm.push(Asm::Op(Opcode::CALLDATACOPY));

        // Stack: [size]
        self.state.asm.push(Asm::Op(Opcode::POP));
        // Stack: []
    }

    /// Emit a runtime error revert
    pub(super) fn emit_runtime_error(&mut self, error_code: u8) {
        use evm_glue::opcodes::Opcode;

        // Store error code at memory address 0x00
        self.push_const(U256::from(error_code));
        self.push_const(U256::from(0x00));
        self.state.asm.push(Asm::Op(Opcode::MSTORE8));

        // REVERT with the error code
        // offset = 0x00, size = 1
        self.push_const(U256::from(1)); // size
        self.push_const(U256::from(0)); // offset
        self.state.asm.push(Asm::Op(Opcode::REVERT));
    }

    // === Undefined Behavior Checking ===
    // These methods handle runtime checks for undefined behavior in eth-ir programs

    /// Check if a memory address is within valid bounds
    ///
    /// Returns true if bounds checking is enabled and the address needs checking
    pub(super) fn should_check_memory_bounds(&self) -> bool {
        self.state.enable_bounds_checking
    }

    /// Emit a bounds check for memory operations
    ///
    /// Assumes the address is on top of the stack
    /// Stack: [address, ...]
    pub(super) fn emit_memory_bounds_check(&mut self) {
        if !self.should_check_memory_bounds() {
            return;
        }

        use crate::error::runtime;
        use evm_glue::opcodes::Opcode;

        // Check if address > MAX_MEMORY_SIZE
        // Stack: [address]
        self.state.asm.push(Asm::Op(Opcode::DUP1)); // [address, address]

        // Push MAX_MEMORY_SIZE
        self.push_const(U256::from(super::constants::MAX_MEMORY_SIZE));
        // Stack: [MAX_MEMORY_SIZE, address, address]

        // Check if address > MAX_MEMORY_SIZE
        self.state.asm.push(Asm::Op(Opcode::GT)); // [address > MAX, address]

        // If true, revert with INDEX_OUT_OF_BOUNDS error
        let safe_mark = self.state.marks.allocate_mark();
        self.state.asm.push(Asm::Op(Opcode::ISZERO)); // [address <= MAX, address]
        self.emit_jumpi(safe_mark);

        // Address is out of bounds - emit error
        self.emit_runtime_error(runtime::INDEX_OUT_OF_BOUNDS);

        // Safe path continues here
        self.emit_mark(safe_mark);
        // Stack: [address]
    }
}
