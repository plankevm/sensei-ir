//! Helper methods for assembly generation

use super::{Translator, marks::MarkId};
use crate::error::Result;
use alloy_primitives::U256;
use eth_ir_data::{LocalId, operation::HasArgs};
use evm_glue::assembly::{Asm, MarkRef, RefType};

impl Translator {
    /// Push a constant using the smallest PUSH opcode
    pub(super) fn push_const(&mut self, value: U256) {
        push_const_impl(value, &mut self.state.asm);
    }
}

/// Push a U256 constant using the most optimal PUSH opcode
///
/// Selects the smallest PUSH instruction based on the value's byte size:
/// - PUSH0 for zero
/// - PUSH1-PUSH32 for non-zero values, sized to fit the actual value
///
/// This minimizes bytecode size and gas costs for PUSH operations.
pub(super) fn push_const_impl(value: U256, asm: &mut Vec<Asm>) {
    use evm_glue::opcodes::Opcode;

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
        5 => push_n!(5, PUSH5),
        6 => push_n!(6, PUSH6),
        7 => push_n!(7, PUSH7),
        8 => push_n!(8, PUSH8),
        9..=32 => {
            // For values larger than 8 bytes, use PUSH32 with full representation
            asm.push(Asm::Op(Opcode::PUSH32(value.to_be_bytes())));
        }
        _ => unreachable!("U256 is max 32 bytes by definition"),
    }
}

impl Translator {
    /// Load a local onto the stack
    pub(super) fn load_local(&mut self, local: LocalId) -> Result<()> {
        self.state.locals.generate_load(local, &mut self.state.asm)
    }

    /// Store stack value to a local
    pub(super) fn store_local(&mut self, local: LocalId) -> Result<()> {
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
        use evm_glue::opcodes::Opcode;

        // Get free memory pointer location
        let ptr_loc = self.state.locals.get_free_memory_pointer_location();

        // Load current free memory pointer value
        self.push_const(U256::from(ptr_loc));
        self.state.asm.push(Asm::Op(Opcode::MLOAD));

        // Stack: [current_ptr]
        // Keep a copy for return value
        self.state.asm.push(Asm::Op(Opcode::DUP1));

        // Load size and add to get new pointer
        self.load_local(size_local)?;

        // Stack: [size, current_ptr, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::DUP2)); // [current_ptr, size, current_ptr, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::ADD)); // [new_ptr, current_ptr, current_ptr]

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

    /// Emit code to zero memory efficiently
    ///
    /// Uses CALLDATACOPY with an out-of-bounds offset to zero memory.
    /// When dataOffset >= CALLDATASIZE, CALLDATACOPY writes zeros (EVM spec).
    ///
    /// We use offset 0xFFFFFFFF (4GB), which is impossible to reach in practice:
    /// - Current block gas limits allow ~7.5MB max calldata
    /// - 4GB calldata would cost trillions of gas
    /// - EVM memory expansion costs make this physically impossible
    ///
    /// This is a standard optimization used by EVM compilers (e.g., Solidity).
    ///
    /// Stack: [ptr, size] → []
    pub(super) fn emit_memory_zeroing_loop(&mut self) {
        use evm_glue::opcodes::Opcode;

        // Stack: [ptr, size]
        // We need: CALLDATACOPY(destOffset, dataOffset, size)
        // where dataOffset > CALLDATASIZE to get zeros

        self.state.asm.push(Asm::Op(Opcode::SWAP1)); // [size, ptr]
        self.state.asm.push(Asm::Op(Opcode::DUP1)); // [size, size, ptr]
        self.state.asm.push(Asm::Op(Opcode::SWAP2)); // [ptr, size, size]

        // Push offset beyond any possible calldata size
        self.push_const(U256::from(super::constants::CALLDATACOPY_ZERO_OFFSET)); // [bigOffset, ptr, size, size]
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
}
