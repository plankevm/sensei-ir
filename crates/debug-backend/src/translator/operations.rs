//! Operation translation
//! Contains translate_operation with flattened operation handling

use super::Translator;

use super::constants::EVM_WORD_SIZE;
use alloy_primitives::U256;
use evm_glue::{
    assembly::{Asm, MarkRef, RefType},
    opcodes::Opcode,
};
use sir_data::{index::*, operation::*};

/// Maps operation kinds with direct 1:1 EVM opcode translations
fn get_direct_opcode(kind: OperationKind) -> Option<Opcode> {
    use Opcode as Op;
    use OperationKind as OK;

    match kind {
        // Arithmetic
        OK::Add => Some(Op::ADD),
        OK::Mul => Some(Op::MUL),
        OK::Sub => Some(Op::SUB),
        OK::Div => Some(Op::DIV),
        OK::SDiv => Some(Op::SDIV),
        OK::Mod => Some(Op::MOD),
        OK::SMod => Some(Op::SMOD),
        OK::AddMod => Some(Op::ADDMOD),
        OK::MulMod => Some(Op::MULMOD),
        OK::Exp => Some(Op::EXP),
        OK::SignExtend => Some(Op::SIGNEXTEND),

        // Comparison & Bitwise
        OK::Lt => Some(Op::LT),
        OK::Gt => Some(Op::GT),
        OK::SLt => Some(Op::SLT),
        OK::SGt => Some(Op::SGT),
        OK::Eq => Some(Op::EQ),
        OK::IsZero => Some(Op::ISZERO),
        OK::And => Some(Op::AND),
        OK::Or => Some(Op::OR),
        OK::Xor => Some(Op::XOR),
        OK::Not => Some(Op::NOT),
        OK::Byte => Some(Op::BYTE),
        OK::Shl => Some(Op::SHL),
        OK::Shr => Some(Op::SHR),
        OK::Sar => Some(Op::SAR),

        // Cryptographic
        OK::Keccak256 => Some(Op::SHA3),

        // Environmental Information - zero input
        OK::Address => Some(Op::ADDRESS),
        OK::Caller => Some(Op::CALLER),
        OK::Origin => Some(Op::ORIGIN),
        OK::CallValue => Some(Op::CALLVALUE),
        OK::CallDataSize => Some(Op::CALLDATASIZE),
        OK::GasPrice => Some(Op::GASPRICE),
        OK::Gas => Some(Op::GAS),
        OK::Coinbase => Some(Op::COINBASE),
        OK::Timestamp => Some(Op::TIMESTAMP),
        OK::Number => Some(Op::NUMBER),
        OK::Difficulty => Some(Op::PREVRANDAO),
        OK::GasLimit => Some(Op::GASLIMIT),
        OK::ChainId => Some(Op::CHAINID),
        OK::SelfBalance => Some(Op::SELFBALANCE),
        OK::BaseFee => Some(Op::BASEFEE),
        OK::BlobBaseFee => Some(Op::BLOBBASEFEE),
        OK::CodeSize => Some(Op::CODESIZE),
        OK::ReturnDataSize => Some(Op::RETURNDATASIZE),

        // Environmental Information - one input
        OK::Balance => Some(Op::BALANCE),
        OK::CallDataLoad => Some(Op::CALLDATALOAD),
        OK::ExtCodeSize => Some(Op::EXTCODESIZE),
        OK::ExtCodeHash => Some(Op::EXTCODEHASH),
        OK::BlockHash => Some(Op::BLOCKHASH),
        OK::BlobHash => Some(Op::BLOBHASH),

        // State
        OK::SLoad => Some(Op::SLOAD),
        OK::SStore => Some(Op::SSTORE),
        OK::TLoad => Some(Op::TLOAD),
        OK::TStore => Some(Op::TSTORE),

        // Copy operations
        OK::CallDataCopy => Some(Op::CALLDATACOPY),
        OK::CodeCopy => Some(Op::CODECOPY),
        OK::ReturnDataCopy => Some(Op::RETURNDATACOPY),
        OK::ExtCodeCopy => Some(Op::EXTCODECOPY),
        OK::MemoryCopy => Some(Op::MCOPY),

        // Logging operations
        OK::Log0 => Some(Op::LOG0),
        OK::Log1 => Some(Op::LOG1),
        OK::Log2 => Some(Op::LOG2),
        OK::Log3 => Some(Op::LOG3),
        OK::Log4 => Some(Op::LOG4),

        // System calls
        OK::Create => Some(Op::CREATE),
        OK::Create2 => Some(Op::CREATE2),
        OK::Call => Some(Op::CALL),
        OK::CallCode => Some(Op::CALLCODE),
        OK::DelegateCall => Some(Op::DELEGATECALL),
        OK::StaticCall => Some(Op::STATICCALL),
        OK::SelfDestruct => Some(Op::SELFDESTRUCT),

        // Control flow
        OK::Return => Some(Op::RETURN),
        OK::Revert => Some(Op::REVERT),
        OK::Stop => Some(Op::STOP),
        OK::Invalid => Some(Op::INVALID),

        // IR-specific operations without direct opcode mapping
        OK::SetCopy
        | OK::SetSmallConst
        | OK::SetLargeConst
        | OK::SetDataOffset
        | OK::DynamicAllocZeroed
        | OK::DynamicAllocAnyBytes
        | OK::AcquireFreePointer
        | OK::StaticAllocZeroed
        | OK::StaticAllocAnyBytes
        | OK::MemoryLoad
        | OK::MemoryStore
        | OK::InternalCall
        | OK::Noop
        | OK::RuntimeStartOffset
        | OK::InitEndOffset
        | OK::RuntimeLength => None,
    }
}

/// Visitor for translating operations with direct opcode mappings
struct DirectOpcodeTranslator<'a> {
    translator: &'a mut Translator,
    opcode: Opcode,
}

impl<'a> DirectOpcodeTranslator<'a> {
    fn new(translator: &'a mut Translator, opcode: Opcode) -> Self {
        Self { translator, opcode }
    }

    /// Load inputs from allocated arena in reverse order
    fn load_allocated_ins(&mut self, ins_start: LocalIndex, count: usize) {
        // Extract LocalIds from the arena using the same pattern as original code
        let end = ins_start + (count as u32);
        let args: Vec<_> = self.translator.program.locals[ins_start..end].raw.to_vec();

        // Load using load_locals_sequence which loads in reverse
        self.translator.load_locals_sequence(&args);
    }
}

impl<'a> OpVisitor<()> for DirectOpcodeTranslator<'a> {
    fn visit_inline_operands<const INS: usize, const OUTS: usize>(
        &mut self,
        data: &InlineOperands<INS, OUTS>,
    ) {
        // Load inputs in reverse order so first input ends up on top
        for &input in data.ins.iter().rev() {
            self.translator.load_local(input);
        }

        // Emit the opcode
        self.translator.state.asm.push(Asm::Op(self.opcode));

        // Store outputs
        for &output in data.outs.iter() {
            self.translator.store_local(output);
        }
    }

    fn visit_allocated_ins<const INS: usize, const OUTS: usize>(
        &mut self,
        data: &AllocatedIns<INS, OUTS>,
    ) {
        // Load inputs from arena
        self.load_allocated_ins(data.ins_start, INS);

        // Emit the opcode
        self.translator.state.asm.push(Asm::Op(self.opcode));

        // Store outputs
        for &output in data.outs.iter() {
            self.translator.store_local(output);
        }
    }

    fn visit_static_alloc(&mut self, _data: &StaticAllocData) {
        unreachable!("StaticAlloc operations don't have direct opcode mappings")
    }

    fn visit_memory_load(&mut self, _data: &MemoryLoadData) {
        unreachable!("MemoryLoad operations don't have direct opcode mappings")
    }

    fn visit_memory_store(&mut self, _data: &MemoryStoreData) {
        unreachable!("MemoryStore operations don't have direct opcode mappings")
    }

    fn visit_set_small_const(&mut self, _data: &SetSmallConstData) {
        unreachable!("SetSmallConst operations don't have direct opcode mappings")
    }

    fn visit_set_large_const(&mut self, _data: &SetLargeConstData) {
        unreachable!("SetLargeConst operations don't have direct opcode mappings")
    }

    fn visit_set_data_offset(&mut self, _data: &SetDataOffsetData) {
        unreachable!("SetDataOffset operations don't have direct opcode mappings")
    }

    fn visit_icall(&mut self, _data: &InternalCallData) {
        unreachable!("InternalCall operations don't have direct opcode mappings")
    }
}

impl Translator {
    /// Translate an operation by index
    pub(super) fn translate_operation_by_index(
        &mut self,
        op_idx: OperationIndex,
        work_queue: &mut Vec<BasicBlockId>,
    ) {
        let op = self.program.operations[op_idx].clone();
        let kind = op.kind();

        // Try direct opcode translation first
        if let Some(opcode) = get_direct_opcode(kind) {
            let mut visitor = DirectOpcodeTranslator::new(self, opcode);
            op.visit_data(&mut visitor);
            return;
        }

        // Bespoke translations for operations without uniform opcode mappings
        match &op {
            // Local variable operations
            Operation::SetCopy(one_in_one) => {
                self.load_local(one_in_one.ins[0]);
                self.store_local(one_in_one.outs[0])
            }
            Operation::SetSmallConst(set_const) => {
                self.push_const(U256::from(set_const.value));
                self.store_local(set_const.sets)
            }
            Operation::SetLargeConst(set_large) => {
                let value = self.program.large_consts[set_large.value];
                self.push_const(value);
                self.store_local(set_large.sets)
            }
            Operation::SetDataOffset(set) => self.translate_local_set_data_offset(set),

            // Internal call - special control flow handling
            Operation::InternalCall(call) => self.translate_internal_call(call, work_queue),

            // Memory allocation operations
            Operation::AcquireFreePointer(zero_in_one) => {
                let ptr_loc = self.state.locals.get_free_memory_pointer_location();
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MLOAD));
                self.store_local(zero_in_one.outs[0])
            }
            Operation::DynamicAllocZeroed(one_in_one) => {
                self.emit_allocate_memory(one_in_one.ins[0], one_in_one.outs[0], true)
            }
            Operation::DynamicAllocAnyBytes(one_in_one) => {
                self.emit_allocate_memory(one_in_one.ins[0], one_in_one.outs[0], false)
            }
            Operation::StaticAllocZeroed(static_alloc) => {
                // StaticAllocZeroed uses a compile-time constant size and zeroes the memory
                let ptr_loc = self.state.locals.get_free_memory_pointer_location();
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MLOAD)); // Stack: [current_ptr]
                self.push_const(U256::from(static_alloc.size)); // Stack: [size, current_ptr]
                self.state.asm.push(Asm::Op(Opcode::DUP1)); // Stack: [size, size, current_ptr]
                self.state.asm.push(Asm::Op(Opcode::CALLDATASIZE)); // Stack: [cdsize, size, size, current_ptr]
                self.state.asm.push(Asm::Op(Opcode::DUP4)); // Stack: [current_ptr, cdsize, size, size, current_ptr]
                self.state.asm.push(Asm::Op(Opcode::CALLDATACOPY)); // Stack: [size, current_ptr]
                self.state.asm.push(Asm::Op(Opcode::DUP2)); // Stack: [current_ptr, size, current_ptr]
                self.state.asm.push(Asm::Op(Opcode::ADD)); // Stack: [new_ptr, current_ptr]
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MSTORE)); // Stack: [current_ptr]
                self.store_local(static_alloc.ptr_out)
            }
            Operation::StaticAllocAnyBytes(static_alloc) => {
                // StaticAllocAnyBytes uses a compile-time constant size without zeroing
                let ptr_loc = self.state.locals.get_free_memory_pointer_location();
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MLOAD)); // Stack: [current_ptr]
                self.state.asm.push(Asm::Op(Opcode::DUP1)); // Stack: [current_ptr, current_ptr]
                self.push_const(U256::from(static_alloc.size)); // Stack: [size, current_ptr, current_ptr]
                self.state.asm.push(Asm::Op(Opcode::ADD)); // Stack: [new_ptr, current_ptr]
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MSTORE)); // Stack: [current_ptr]
                self.store_local(static_alloc.ptr_out)
            }

            Operation::MemoryLoad(load) => {
                debug_assert_eq!(
                    load.size as u8,
                    EVM_WORD_SIZE as u8,
                    "MemoryLoad io_size must be {} bits for well-formed IR",
                    EVM_WORD_SIZE * 8
                );
                self.load_local(load.ptr);
                self.state.asm.push(Asm::Op(Opcode::MLOAD));
                self.store_local(load.out)
            }
            Operation::MemoryStore(store) => {
                debug_assert!(
                    store.size as u8 == 1 || store.size as u8 == EVM_WORD_SIZE as u8,
                    "MemoryStore io_size must be 8 or {} bits for well-formed IR, got {}",
                    EVM_WORD_SIZE * 8,
                    store.size as u8 * 8
                );
                self.load_local(store.value);
                self.load_local(store.ptr);
                if store.size as u8 == 1 {
                    self.state.asm.push(Asm::Op(Opcode::MSTORE8));
                } else {
                    self.state.asm.push(Asm::Op(Opcode::MSTORE));
                }
            }

            // Control flow - noop has no translation
            Operation::Noop(_) => {}

            // Bytecode introspection operations
            Operation::RuntimeStartOffset(zero_in_one) => self.translate_runtime_introspection(
                RefType::Direct(self.state.runtime_start_mark),
                zero_in_one.outs[0],
            ),
            Operation::InitEndOffset(zero_in_one) => self.translate_runtime_introspection(
                RefType::Direct(self.state.init_end_mark),
                zero_in_one.outs[0],
            ),
            Operation::RuntimeLength(zero_in_one) => self.translate_runtime_introspection(
                RefType::Delta(self.state.runtime_start_mark, self.state.runtime_end_mark),
                zero_in_one.outs[0],
            ),

            // All other operations should be handled by direct opcode translation
            _ => unreachable!(
                "Operation {:?} should have been handled by direct opcode translation",
                kind
            ),
        }
    }

    /// Translate SetDataOffset operation
    /// This operation sets a local to the runtime offset of a data segment
    fn translate_local_set_data_offset(&mut self, set: &SetDataOffsetData) {
        let mark = *self
            .state
            .data_marks
            .get(set.segment_id)
            .expect("Invalid IR: SetDataOffset references non-existent data segment");

        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(mark),
            is_pushed: true,
            set_size: None,
        }));

        self.store_local(set.sets)
    }

    /// Translate InternalCall operation
    /// Sets up a return address, jumps to the target function, and emits the return mark
    fn translate_internal_call(
        &mut self,
        call: &InternalCallData,
        work_queue: &mut Vec<BasicBlockId>,
    ) {
        let return_mark = self.state.marks.allocate_mark();

        self.emit_code_offset_push(return_mark);

        // Jump to target function
        let func_entry_block = self.program.functions[call.function].entry();
        let block_mark = self.state.marks.get_block_mark(func_entry_block);
        self.emit_jump(block_mark);

        work_queue.push(func_entry_block);

        self.emit_mark(return_mark);
    }

    /// Translate runtime introspection operations (RuntimeStartOffset, InitEndOffset,
    /// RuntimeLength). These operations push a mark reference and store the result
    fn translate_runtime_introspection(&mut self, ref_type: RefType, result: LocalId) {
        self.state.asm.push(Asm::Ref(MarkRef { ref_type, is_pushed: true, set_size: None }));
        self.store_local(result)
    }
}
