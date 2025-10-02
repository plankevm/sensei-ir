//! Helper methods for assembly generation

use super::{Translator, marks::MarkId};
use alloy_primitives::U256;
use eth_ir_data::{LocalId, operation::HasArgs};
use evm_glue::assembly::{Asm, MarkRef, RefType};

impl Translator {
    /// Push a constant using the smallest PUSH opcode (PUSH0/PUSH1-PUSH32)
    pub(super) fn push_const(&mut self, value: U256) {
        use evm_glue::opcodes::Opcode;

        if value.is_zero() {
            self.state.asm.push(Asm::Op(Opcode::PUSH0));
            return;
        }

        let trimmed = value.to_be_bytes_trimmed_vec();

        macro_rules! push_n {
            ($n:expr, $opcode:ident) => {{
                let mut arr = [0u8; $n];
                arr.copy_from_slice(&trimmed[..]);
                self.state.asm.push(Asm::Op(Opcode::$opcode(arr)));
            }};
        }

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
                self.state.asm.push(Asm::Op(Opcode::PUSH32(value.to_be_bytes())));
            }
            _ => unreachable!("U256 is max 32 bytes by definition"),
        }
    }

    pub(super) fn load_local(&mut self, local: LocalId) {
        self.state.locals.generate_load(local, &mut self.state.asm);
    }

    pub(super) fn store_local(&mut self, local: LocalId) {
        self.state.locals.generate_store(local, &mut self.state.asm);
    }

    pub(super) fn emit_mark(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        self.state.asm.push(Asm::Mark(mark_id));
        self.state.asm.push(Asm::Op(Opcode::JUMPDEST));
    }

    pub(super) fn emit_jump(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(mark_id),
            is_pushed: true,
            set_size: None,
        }));
        // Stack: [destination]
        self.state.asm.push(Asm::Op(Opcode::JUMP));
    }

    /// Conditional jump (jumps if non-zero)
    pub(super) fn emit_jumpi(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        // Stack: [condition]
        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(mark_id),
            is_pushed: true,
            set_size: None,
        }));
        // Stack: [condition, destination]
        self.state.asm.push(Asm::Op(Opcode::JUMPI));
    }

    /// Load multiple locals onto the stack in reverse order
    /// Loads locals[n-1], locals[n-2], ..., locals[0] so that locals[0] ends up on top
    pub(super) fn load_locals_sequence(&mut self, locals: &[LocalId]) {
        for &local in locals.iter().rev() {
            self.load_local(local);
        }
    }

    /// Emit a simple zero-input-one-output operation
    pub(super) fn emit_zero_in_one_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        result: LocalId,
    ) {
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(result)
    }

    /// Emit a simple one-input-one-output operation
    pub(super) fn emit_one_in_one_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        arg: LocalId,
        result: LocalId,
    ) {
        self.load_local(arg);
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(result)
    }

    /// Emit a three-input-zero-output operation for memory copy operations
    /// Loads in reverse order so dest_offset ends up on top of stack
    pub(super) fn emit_three_in_zero_copy_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        dest_offset: LocalId,
        src_offset: LocalId,
        size: LocalId,
    ) {
        self.load_local(size);
        self.load_local(src_offset);
        self.load_local(dest_offset);
        self.state.asm.push(Asm::Op(opcode));
    }

    /// Emit a two-input-zero-output operation
    /// Loads arg2 then arg1 so arg1 ends up on top of stack
    pub(super) fn emit_two_in_zero_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        arg1: LocalId,
        arg2: LocalId,
    ) {
        self.load_local(arg2);
        self.load_local(arg1);
        self.state.asm.push(Asm::Op(opcode));
    }

    /// Emit a three-input-zero-output operation
    /// Arguments are loaded in reverse order (arg3, arg2, arg1)
    pub(super) fn emit_three_in_zero_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        args: &[LocalId],
    ) {
        self.load_local(args[2]);
        self.load_local(args[1]);
        self.load_local(args[0]);
        self.state.asm.push(Asm::Op(opcode));
    }

    /// Emit a three-input-one-output operation (like ADDMOD, MULMOD)
    pub(super) fn emit_three_in_one_op(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        args: &[LocalId],
        result: LocalId,
    ) {
        self.emit_three_in_zero_op(opcode, args);
        self.store_local(result)
    }

    pub(super) fn emit_call_operation<const N: u32>(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        large_in_one: &eth_ir_data::operation::LargeInOneOut<N>,
        num_args: usize,
    ) {
        let args = large_in_one.get_args(&self.program.locals);
        self.load_locals_sequence(&args[0..num_args]);
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(large_in_one.result)
    }

    pub(super) fn emit_log_operation(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        offset: LocalId,
        size: LocalId,
        topics: &[LocalId],
    ) {
        for &topic in topics.iter().rev() {
            self.load_local(topic);
        }
        self.load_local(size);
        self.load_local(offset);
        self.state.asm.push(Asm::Op(opcode));
    }

    pub(super) fn emit_create_operation<const N: u32>(
        &mut self,
        opcode: evm_glue::opcodes::Opcode,
        large_in_one: &eth_ir_data::operation::LargeInOneOut<N>,
        num_args: usize,
    ) {
        let args = large_in_one.get_args(&self.program.locals);
        self.load_locals_sequence(&args[0..num_args]);
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(large_in_one.result)
    }

    pub(super) fn emit_allocate_memory(
        &mut self,
        size_local: LocalId,
        result_local: LocalId,
        zero_memory: bool,
    ) {
        if zero_memory {
            self.emit_allocate_zeroed_memory(size_local, result_local);
        } else {
            self.emit_allocate_uninitialized_memory(size_local, result_local);
        }
    }

    fn emit_allocate_uninitialized_memory(&mut self, size_local: LocalId, result_local: LocalId) {
        use evm_glue::opcodes::Opcode;

        let ptr_loc = self.state.locals.get_free_memory_pointer_location();
        self.push_const(U256::from(ptr_loc));
        self.state.asm.push(Asm::Op(Opcode::MLOAD)); // Stack: [current_ptr]
        self.state.asm.push(Asm::Op(Opcode::DUP1)); // Stack: [current_ptr, current_ptr]
        self.load_local(size_local); // Stack: [size, current_ptr, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::ADD)); // Stack: [new_ptr, current_ptr]
        self.push_const(U256::from(ptr_loc));
        self.state.asm.push(Asm::Op(Opcode::MSTORE)); // Stack: [current_ptr]
        self.store_local(result_local)
    }

    fn emit_allocate_zeroed_memory(&mut self, size_local: LocalId, result_local: LocalId) {
        use evm_glue::opcodes::Opcode;

        let ptr_loc = self.state.locals.get_free_memory_pointer_location();
        self.push_const(U256::from(ptr_loc));
        self.state.asm.push(Asm::Op(Opcode::MLOAD)); // Stack: [current_ptr]
        self.load_local(size_local); // Stack: [size, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::DUP1)); // Stack: [size, size, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::CALLDATASIZE)); // Stack: [cdsize, size, size, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::DUP4)); // Stack: [current_ptr, cdsize, size, size, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::CALLDATACOPY)); // Stack: [size, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::DUP2)); // Stack: [current_ptr, size, current_ptr]
        self.state.asm.push(Asm::Op(Opcode::ADD)); // Stack: [new_ptr, current_ptr]
        self.push_const(U256::from(ptr_loc));
        self.state.asm.push(Asm::Op(Opcode::MSTORE)); // Stack: [current_ptr]
        self.store_local(result_local)
    }

    pub(super) fn emit_runtime_error(&mut self, error_code: u8) {
        use evm_glue::opcodes::Opcode;

        self.push_const(U256::from(error_code));
        self.push_const(U256::from(0x00));
        self.state.asm.push(Asm::Op(Opcode::MSTORE8));

        self.push_const(U256::from(1));
        self.push_const(U256::from(0));
        self.state.asm.push(Asm::Op(Opcode::REVERT));
    }
}
