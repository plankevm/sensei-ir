//! Operation translation
//! Contains translate_operation with flattened operation handling

use super::Translator;

use alloy_primitives::U256;
use eth_ir_data::operation::HasArgs;
use evm_glue::assembly::{Asm, MarkRef, RefType};

impl Translator {
    /// Translate a two-argument operation to EVM assembly
    /// Pushes arg2 then arg1 so that arg1 is on top for the operation.
    /// This matches EVM convention where for non-commutative ops like SUB, DIV, shifts:
    /// the first argument in the IR operation ends up on top of the stack.
    fn translate_two_arg_op(
        &mut self,
        two_in_one: &eth_ir_data::operation::TwoInOneOut,
        opcode: evm_glue::opcodes::Opcode,
    ) {
        self.load_local(two_in_one.arg2);
        self.load_local(two_in_one.arg1);
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(two_in_one.result);
    }

    /// Translate a one-argument operation to EVM assembly
    fn translate_one_arg_op(
        &mut self,
        one_in_one: &eth_ir_data::operation::OneInOneOut,
        opcode: evm_glue::opcodes::Opcode,
    ) {
        self.load_local(one_in_one.arg1);
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(one_in_one.result);
    }

    /// Translate an operation by index
    pub(super) fn translate_operation_by_index(
        &mut self,
        op_idx: eth_ir_data::OperationIndex,
        work_queue: &mut Vec<eth_ir_data::BasicBlockId>,
    ) {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        let op = self.program.operations[op_idx].clone();

        match &op {
            // Arithmetic - two argument operations
            Operation::Add(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::ADD),
            Operation::Sub(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SUB),
            Operation::Mul(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::MUL),
            Operation::Div(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::DIV),
            Operation::SDiv(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SDIV),
            Operation::Mod(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::MOD),
            Operation::SMod(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SMOD),
            Operation::Exp(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::EXP),
            Operation::SignExtend(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::SIGNEXTEND)
            }

            // Arithmetic - three argument modular operations
            Operation::AddMod(large_in_one) => {
                let args = large_in_one.get_args(&self.program.locals);
                self.emit_three_in_one_op(Opcode::ADDMOD, &args[0..3], large_in_one.result)
            }
            Operation::MulMod(large_in_one) => {
                let args = large_in_one.get_args(&self.program.locals);
                self.emit_three_in_one_op(Opcode::MULMOD, &args[0..3], large_in_one.result)
            }

            // Bitwise operations
            Operation::And(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::AND),
            Operation::Or(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::OR),
            Operation::Xor(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::XOR),
            Operation::Not(one_in_one) => self.translate_one_arg_op(one_in_one, Opcode::NOT),
            Operation::Byte(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::BYTE),
            Operation::Shl(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SHL),
            Operation::Shr(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SHR),
            Operation::Sar(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SAR),

            // Comparison operations
            Operation::Lt(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::LT),
            Operation::Gt(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::GT),
            Operation::SLt(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SLT),
            Operation::SGt(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SGT),
            Operation::Eq(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::EQ),
            Operation::IsZero(one_in_one) => self.translate_one_arg_op(one_in_one, Opcode::ISZERO),

            // Cryptographic operations
            Operation::Keccak256(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SHA3),

            // Local variable operations
            Operation::LocalSet(one_in_one) => {
                self.load_local(one_in_one.arg1);
                self.store_local(one_in_one.result)
            }
            Operation::LocalSetSmallConst(set_const) => {
                self.push_const(U256::from(set_const.value));
                self.store_local(set_const.local)
            }
            Operation::LocalSetLargeConst(set_large) => {
                let value = self.program.large_consts[set_large.cid];
                self.push_const(value);
                self.store_local(set_large.local)
            }
            Operation::LocalSetDataOffset(set) => self.translate_local_set_data_offset(set),

            Operation::Call(large_in_one) => self.emit_call_operation(
                Opcode::CALL,
                large_in_one,
                super::constants::CALL_ARG_COUNT,
            ),
            Operation::CallCode(large_in_one) => self.emit_call_operation(
                Opcode::CALLCODE,
                large_in_one,
                super::constants::CALLCODE_ARG_COUNT,
            ),
            Operation::DelegateCall(large_in_one) => self.emit_call_operation(
                Opcode::DELEGATECALL,
                large_in_one,
                super::constants::DELEGATECALL_ARG_COUNT,
            ),
            Operation::StaticCall(large_in_one) => self.emit_call_operation(
                Opcode::STATICCALL,
                large_in_one,
                super::constants::STATICCALL_ARG_COUNT,
            ),
            Operation::InternalCall(call) => self.translate_internal_call(call, work_queue),

            // Contract creation operations
            Operation::Create(large_in_one) => self.emit_create_operation(
                Opcode::CREATE,
                large_in_one,
                super::constants::CREATE_ARG_COUNT,
            ),
            Operation::Create2(large_in_one) => self.emit_create_operation(
                Opcode::CREATE2,
                large_in_one,
                super::constants::CREATE2_ARG_COUNT,
            ),
            Operation::SelfDestruct(one_in_zero) => {
                self.load_local(one_in_zero.arg1);
                self.state.asm.push(Asm::Op(Opcode::SELFDESTRUCT));
            }

            // Environmental information - zero input operations
            Operation::Address(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::ADDRESS, zero_in_one.result)
            }
            Operation::Caller(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::CALLER, zero_in_one.result)
            }
            Operation::Origin(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::ORIGIN, zero_in_one.result)
            }
            Operation::CallValue(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::CALLVALUE, zero_in_one.result)
            }
            Operation::CallDataSize(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::CALLDATASIZE, zero_in_one.result)
            }
            Operation::GasPrice(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::GASPRICE, zero_in_one.result)
            }
            Operation::Gas(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::GAS, zero_in_one.result)
            }
            Operation::Coinbase(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::COINBASE, zero_in_one.result)
            }
            Operation::Timestamp(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::TIMESTAMP, zero_in_one.result)
            }
            Operation::Number(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::NUMBER, zero_in_one.result)
            }
            Operation::Difficulty(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::PREVRANDAO, zero_in_one.result)
            }
            Operation::GasLimit(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::GASLIMIT, zero_in_one.result)
            }
            Operation::ChainId(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::CHAINID, zero_in_one.result)
            }
            Operation::SelfBalance(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::SELFBALANCE, zero_in_one.result)
            }
            Operation::BaseFee(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::BASEFEE, zero_in_one.result)
            }
            Operation::BlobBaseFee(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::BLOBBASEFEE, zero_in_one.result)
            }
            Operation::CodeSize(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::CODESIZE, zero_in_one.result)
            }
            Operation::ReturnDataSize(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::RETURNDATASIZE, zero_in_one.result)
            }

            // Environmental information - one input operations
            Operation::Balance(one_in_one) => {
                self.emit_one_in_one_op(Opcode::BALANCE, one_in_one.arg1, one_in_one.result)
            }
            Operation::CallDataLoad(one_in_one) => {
                self.emit_one_in_one_op(Opcode::CALLDATALOAD, one_in_one.arg1, one_in_one.result)
            }
            Operation::ExtCodeSize(one_in_one) => {
                self.emit_one_in_one_op(Opcode::EXTCODESIZE, one_in_one.arg1, one_in_one.result)
            }
            Operation::ExtCodeHash(one_in_one) => {
                self.emit_one_in_one_op(Opcode::EXTCODEHASH, one_in_one.arg1, one_in_one.result)
            }
            Operation::BlockHash(one_in_one) => {
                self.emit_one_in_one_op(Opcode::BLOCKHASH, one_in_one.arg1, one_in_one.result)
            }
            Operation::BlobHash(one_in_one) => {
                self.emit_one_in_one_op(Opcode::BLOBHASH, one_in_one.arg1, one_in_one.result)
            }

            Operation::SLoad(one_in_one) => {
                self.emit_one_in_one_op(Opcode::SLOAD, one_in_one.arg1, one_in_one.result)
            }
            Operation::SStore(two_in_zero) => {
                self.emit_two_in_zero_op(Opcode::SSTORE, two_in_zero.arg1, two_in_zero.arg2)
            }
            Operation::TLoad(one_in_one) => {
                self.emit_one_in_one_op(Opcode::TLOAD, one_in_one.arg1, one_in_one.result)
            }
            Operation::TStore(two_in_zero) => {
                self.emit_two_in_zero_op(Opcode::TSTORE, two_in_zero.arg1, two_in_zero.arg2)
            }

            Operation::AcquireFreePointer(zero_in_one) => {
                let ptr_loc = self.state.locals.get_free_memory_pointer_location();
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MLOAD));
                self.store_local(zero_in_one.result)
            }
            Operation::DynamicAllocZeroed(one_in_one) => {
                self.emit_allocate_memory(one_in_one.arg1, one_in_one.result, true)
            }
            Operation::DynamicAllocAnyBytes(one_in_one) => {
                self.emit_allocate_memory(one_in_one.arg1, one_in_one.result, false)
            }
            Operation::LocalAllocZeroed(one_in_one) => {
                self.emit_allocate_memory(one_in_one.arg1, one_in_one.result, true)
            }
            Operation::LocalAllocAnyBytes(one_in_one) => {
                self.emit_allocate_memory(one_in_one.arg1, one_in_one.result, false)
            }
            Operation::DynamicAllocUsingFreePointer(two_in_zero) => {
                self.load_local(two_in_zero.arg1);
                self.load_local(two_in_zero.arg2);
                self.state.asm.push(Asm::Op(Opcode::ADD));
                let ptr_loc = self.state.locals.get_free_memory_pointer_location();
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MSTORE));
            }

            Operation::MemoryLoad(load) => {
                use super::constants::EVM_WORD_SIZE;
                debug_assert_eq!(
                    load.byte_size, EVM_WORD_SIZE as u8,
                    "MemoryLoad byte_size must be {} for well-formed IR",
                    EVM_WORD_SIZE
                );
                self.load_local(load.address);
                self.state.asm.push(Asm::Op(Opcode::MLOAD));
                self.store_local(load.result)
            }
            Operation::MemoryStore(store) => {
                use super::constants::EVM_WORD_SIZE;
                debug_assert!(
                    store.byte_size == 1 || store.byte_size == EVM_WORD_SIZE as u8,
                    "MemoryStore byte_size must be 1 or {} for well-formed IR, got {}",
                    EVM_WORD_SIZE,
                    store.byte_size
                );
                self.load_local(store.value);
                self.load_local(store.address);
                if store.byte_size == 1 {
                    self.state.asm.push(Asm::Op(Opcode::MSTORE8));
                } else {
                    self.state.asm.push(Asm::Op(Opcode::MSTORE));
                }
            }

            // Data copy operations
            Operation::CallDataCopy(three_in_zero) => self.emit_three_in_zero_copy_op(
                Opcode::CALLDATACOPY,
                three_in_zero.arg1,
                three_in_zero.arg2,
                three_in_zero.arg3,
            ),
            Operation::CodeCopy(three_in_zero) => self.emit_three_in_zero_copy_op(
                Opcode::CODECOPY,
                three_in_zero.arg1,
                three_in_zero.arg2,
                three_in_zero.arg3,
            ),
            Operation::ReturnDataCopy(three_in_zero) => self.emit_three_in_zero_copy_op(
                Opcode::RETURNDATACOPY,
                three_in_zero.arg1,
                three_in_zero.arg2,
                three_in_zero.arg3,
            ),
            Operation::ExtCodeCopy(large_in_zero) => {
                let args = large_in_zero.get_args(&self.program.locals);
                self.load_local(args[0]);
                self.load_local(args[1]);
                self.load_local(args[2]);
                self.load_local(args[3]);
                self.state.asm.push(Asm::Op(Opcode::EXTCODECOPY));
            }
            Operation::MCopy(three_in_zero) => {
                self.load_local(three_in_zero.arg3);
                self.load_local(three_in_zero.arg2);
                self.load_local(three_in_zero.arg1);
                self.state.asm.push(Asm::Op(Opcode::MCOPY));
            }

            // Logging operations
            Operation::Log0(two_in_zero) => {
                self.emit_two_in_zero_op(Opcode::LOG0, two_in_zero.arg1, two_in_zero.arg2)
            }
            Operation::Log1(three_in_zero) => self.emit_log_operation(
                Opcode::LOG1,
                three_in_zero.arg1,
                three_in_zero.arg2,
                &[three_in_zero.arg3],
            ),
            Operation::Log2(large_in_zero) => {
                let args = large_in_zero.get_args(&self.program.locals);
                self.emit_log_operation(Opcode::LOG2, args[0], args[1], &args[2..4])
            }
            Operation::Log3(large_in_zero) => {
                let args = large_in_zero.get_args(&self.program.locals);
                self.emit_log_operation(Opcode::LOG3, args[0], args[1], &args[2..5])
            }
            Operation::Log4(large_in_zero) => {
                let args = large_in_zero.get_args(&self.program.locals);
                self.emit_log_operation(Opcode::LOG4, args[0], args[1], &args[2..6])
            }

            // Control flow operations
            Operation::Return(two_in_zero) => {
                self.emit_two_in_zero_op(Opcode::RETURN, two_in_zero.arg1, two_in_zero.arg2)
            }
            Operation::Revert(two_in_zero) => {
                self.emit_two_in_zero_op(Opcode::REVERT, two_in_zero.arg1, two_in_zero.arg2)
            }
            Operation::Stop => {
                self.state.asm.push(Asm::Op(Opcode::STOP));
            }
            Operation::Invalid => {
                self.state.asm.push(Asm::Op(Opcode::INVALID));
            }
            Operation::NoOp => {}

            // Bytecode introspection operations
            Operation::RuntimeStartOffset(zero_in_one) => self.translate_runtime_introspection(
                RefType::Direct(self.state.runtime_start_mark),
                zero_in_one.result,
            ),
            Operation::InitEndOffset(zero_in_one) => self.translate_runtime_introspection(
                RefType::Direct(self.state.init_end_mark),
                zero_in_one.result,
            ),
            Operation::RuntimeLength(zero_in_one) => self.translate_runtime_introspection(
                RefType::Delta(self.state.runtime_end_mark, self.state.runtime_start_mark),
                zero_in_one.result,
            ),
        }
    }

    /// Translate LocalSetDataOffset operation
    /// This operation sets a local to the runtime offset of a data segment
    fn translate_local_set_data_offset(&mut self, set: &eth_ir_data::operation::SetDataOffset) {
        let mark = *self
            .state
            .data_marks
            .get(set.segment_id)
            .expect("Invalid IR: LocalSetDataOffset references non-existent data segment");

        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(mark),
            is_pushed: true,
            set_size: None,
        }));

        self.store_local(set.local)
    }

    /// Translate InternalCall operation
    /// Sets up a return address, jumps to the target function, and emits the return mark
    fn translate_internal_call(
        &mut self,
        call: &eth_ir_data::operation::InternalCall,
        work_queue: &mut Vec<eth_ir_data::BasicBlockId>,
    ) {
        let return_mark = self.state.marks.allocate_mark();

        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(return_mark),
            is_pushed: true,
            set_size: None,
        }));

        // Jump to target function
        let func_entry_block = self.program.functions[call.function].entry;
        let block_mark = self.state.marks.get_block_mark(func_entry_block);
        self.emit_jump(block_mark);

        work_queue.push(func_entry_block);

        self.emit_mark(return_mark);
    }

    /// Translate runtime introspection operations (RuntimeStartOffset, InitEndOffset,
    /// RuntimeLength). These operations push a mark reference and store the result
    fn translate_runtime_introspection(&mut self, ref_type: RefType, result: eth_ir_data::LocalId) {
        self.state.asm.push(Asm::Ref(MarkRef { ref_type, is_pushed: true, set_size: None }));
        self.store_local(result)
    }
}
