//! Operation translation
//! Contains translate_operation and category-specific translation methods

use super::Translator;
use crate::error::{CodegenError, Result};
use alloy_primitives::U256;
use eth_ir_data::operation::HasArgs;
use evm_glue::assembly::{Asm, MarkRef, RefType};

impl Translator {
    /// Translate a two-argument operation to EVM assembly
    ///
    /// # Argument Order
    ///
    /// The `reversed` parameter controls stack push order:
    /// - `false`: Push arg1, then arg2 (for commutative ops: ADD, MUL, AND, OR, XOR, EQ)
    /// - `true`: Push arg2, then arg1 (for non-commutative ops: SUB, DIV, shifts, comparisons)
    ///
    /// This is necessary because the EVM stack is LIFO. For `result = arg1 OP arg2`:
    /// - Commutative: Order doesn't matter
    /// - Non-commutative: Must push arg2 first so arg1 is on top
    fn translate_two_arg_op(
        &mut self,
        two_in_one: &eth_ir_data::operation::TwoInOneOut,
        opcode: evm_glue::opcodes::Opcode,
        reversed: bool, // true = push arg2 first (for non-commutative ops)
    ) -> Result<()> {
        if reversed {
            self.load_local(two_in_one.arg2)?;
            self.load_local(two_in_one.arg1)?;
        } else {
            self.load_local(two_in_one.arg1)?;
            self.load_local(two_in_one.arg2)?;
        }
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(two_in_one.result)?;
        Ok(())
    }

    /// Translate a one-argument operation to EVM assembly
    fn translate_one_arg_op(
        &mut self,
        one_in_one: &eth_ir_data::operation::OneInOneOut,
        opcode: evm_glue::opcodes::Opcode,
    ) -> Result<()> {
        self.load_local(one_in_one.arg1)?;
        self.state.asm.push(Asm::Op(opcode));
        self.store_local(one_in_one.result)?;
        Ok(())
    }

    /// Translate an operation by index
    ///
    /// This dispatches to category-specific handlers without cloning the operation,
    /// working directly with a reference from the program's operation arena.
    pub(super) fn translate_operation_by_index(
        &mut self,
        op_idx: eth_ir_data::OperationIndex,
    ) -> Result<()> {
        use eth_ir_data::Operation;

        // Clone the Arc (cheap, just increments ref count) to get independent access
        // This allows us to borrow the operation immutably while calling methods with &mut self
        let program = self.program.clone();
        let op = &program.program.operations[op_idx];

        match op {
            // Arithmetic operations
            Operation::Add(_)
            | Operation::Sub(_)
            | Operation::Mul(_)
            | Operation::Div(_)
            | Operation::SDiv(_)
            | Operation::Mod(_)
            | Operation::SMod(_)
            | Operation::Exp(_)
            | Operation::SignExtend(_)
            | Operation::AddMod(_)
            | Operation::MulMod(_) => self.translate_arithmetic_op(op),

            // Bitwise operations
            Operation::And(_)
            | Operation::Or(_)
            | Operation::Xor(_)
            | Operation::Not(_)
            | Operation::Byte(_)
            | Operation::Shl(_)
            | Operation::Shr(_)
            | Operation::Sar(_) => self.translate_bitwise_op(op),

            // Comparison operations
            Operation::Lt(_)
            | Operation::Gt(_)
            | Operation::SLt(_)
            | Operation::SGt(_)
            | Operation::Eq(_)
            | Operation::IsZero(_) => self.translate_comparison_op(op),

            // Cryptographic operations
            Operation::Keccak256(_) => self.translate_crypto_op(op),

            // Local variable operations
            Operation::LocalSet(_)
            | Operation::LocalSetSmallConst(_)
            | Operation::LocalSetLargeConst(_)
            | Operation::LocalSetDataOffset(_) => self.translate_local_op(op),

            // Call and creation operations
            Operation::Call(_)
            | Operation::CallCode(_)
            | Operation::DelegateCall(_)
            | Operation::StaticCall(_)
            | Operation::InternalCall(_)
            | Operation::Create(_)
            | Operation::Create2(_)
            | Operation::SelfDestruct(_) => self.translate_call_create_op(op),

            // Environmental and blockchain information
            Operation::Address(_)
            | Operation::Caller(_)
            | Operation::Origin(_)
            | Operation::CallValue(_)
            | Operation::CallDataSize(_)
            | Operation::GasPrice(_)
            | Operation::Gas(_)
            | Operation::Balance(_)
            | Operation::CallDataLoad(_)
            | Operation::ExtCodeSize(_)
            | Operation::ExtCodeHash(_)
            | Operation::Coinbase(_)
            | Operation::Timestamp(_)
            | Operation::Number(_)
            | Operation::Difficulty(_)
            | Operation::GasLimit(_)
            | Operation::ChainId(_)
            | Operation::SelfBalance(_)
            | Operation::BaseFee(_)
            | Operation::BlobBaseFee(_)
            | Operation::BlockHash(_)
            | Operation::BlobHash(_) => self.translate_environmental_op(op),

            // Storage operations
            Operation::SLoad(_)
            | Operation::SStore(_)
            | Operation::TLoad(_)
            | Operation::TStore(_) => self.translate_storage_op(op),

            // Memory operations
            Operation::MemoryLoad(_)
            | Operation::MemoryStore(_)
            | Operation::AcquireFreePointer(_)
            | Operation::DynamicAllocZeroed(_)
            | Operation::DynamicAllocAnyBytes(_)
            | Operation::LocalAllocZeroed(_)
            | Operation::LocalAllocAnyBytes(_)
            | Operation::DynamicAllocUsingFreePointer(_) => self.translate_memory_op(op),

            // I/O and data copy operations
            Operation::CallDataCopy(_)
            | Operation::CodeCopy(_)
            | Operation::ReturnDataCopy(_)
            | Operation::ExtCodeCopy(_)
            | Operation::MCopy(_)
            | Operation::CodeSize(_)
            | Operation::ReturnDataSize(_)
            | Operation::Log0(_)
            | Operation::Log1(_)
            | Operation::Log2(_)
            | Operation::Log3(_)
            | Operation::Log4(_) => self.translate_io_op(op),

            // Control flow and terminal operations
            Operation::Return(_)
            | Operation::Revert(_)
            | Operation::Stop
            | Operation::Invalid
            | Operation::NoOp => self.translate_control_op(op),

            // Bytecode introspection
            Operation::RuntimeStartOffset(_)
            | Operation::InitEndOffset(_)
            | Operation::RuntimeLength(_) => self.translate_introspection_op(op),
        }
    }

    /// Translate arithmetic operations (Add, Sub, Mul, Div, Mod, Exp, etc.)
    fn translate_arithmetic_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            Operation::Add(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::ADD, false),
            Operation::Sub(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SUB, true),
            Operation::Mul(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::MUL, false),
            Operation::Div(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::DIV, true),
            Operation::SDiv(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::SDIV, true)
            }
            Operation::Mod(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::MOD, true),
            Operation::SMod(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::SMOD, true)
            }
            Operation::Exp(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::EXP, true),
            Operation::SignExtend(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::SIGNEXTEND, true)
            }
            Operation::AddMod(large_in_one) => {
                // Args: [a, b, modulus]
                let args = large_in_one.get_args(&self.program.program.locals);
                self.emit_three_arg_op(Opcode::ADDMOD, &args[0..3], large_in_one.result)
            }
            Operation::MulMod(large_in_one) => {
                // Args: [a, b, modulus]
                let args = large_in_one.get_args(&self.program.program.locals);
                self.emit_three_arg_op(Opcode::MULMOD, &args[0..3], large_in_one.result)
            }
            // SAFETY: Dispatch in translate_operation() guarantees only arithmetic operations reach
            // here
            _ => unreachable!("Non-arithmetic operation in translate_arithmetic_op"),
        }
    }

    /// Translate bitwise operations (And, Or, Xor, Not, Byte, Shl, Shr, Sar)
    fn translate_bitwise_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            Operation::And(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::AND, false),
            Operation::Or(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::OR, false),
            Operation::Xor(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::XOR, false),
            Operation::Not(one_in_one) => self.translate_one_arg_op(one_in_one, Opcode::NOT),
            Operation::Byte(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::BYTE, true)
            }
            Operation::Shl(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SHL, true),
            Operation::Shr(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SHR, true),
            Operation::Sar(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SAR, true),
            // SAFETY: Dispatch in translate_operation() guarantees only bitwise operations reach
            // here
            _ => unreachable!("Non-bitwise operation in translate_bitwise_op"),
        }
    }

    /// Translate comparison operations (Lt, Gt, Eq, IsZero, etc.)
    fn translate_comparison_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            Operation::Lt(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::LT, true),
            Operation::Gt(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::GT, true),
            Operation::SLt(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SLT, true),
            Operation::SGt(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::SGT, true),
            Operation::Eq(two_in_one) => self.translate_two_arg_op(two_in_one, Opcode::EQ, false),
            Operation::IsZero(one_in_one) => self.translate_one_arg_op(one_in_one, Opcode::ISZERO),
            // SAFETY: Dispatch in translate_operation() guarantees only comparison operations reach
            // here
            _ => unreachable!("Non-comparison operation in translate_comparison_op"),
        }
    }

    /// Translate cryptographic operations (Keccak256)
    fn translate_crypto_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            Operation::Keccak256(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::SHA3, false)
            }
            // SAFETY: Dispatch in translate_operation() guarantees only crypto operations reach
            // here
            _ => unreachable!("Non-crypto operation in translate_crypto_op"),
        }
    }

    /// Translate local variable operations (LocalSet, LocalSetSmallConst, LocalSetLargeConst,
    /// LocalSetDataOffset)
    fn translate_local_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;

        match op {
            Operation::LocalSet(one_in_one) => {
                self.load_local(one_in_one.arg1)?;
                self.store_local(one_in_one.result)
            }
            Operation::LocalSetSmallConst(set_const) => {
                let value = U256::from(set_const.value);
                self.push_const(value);
                self.store_local(set_const.local)
            }
            Operation::LocalSetLargeConst(set_large) => {
                let value = self.program.program.large_consts[set_large.cid];
                self.push_const(value);
                self.store_local(set_large.local)
            }
            Operation::LocalSetDataOffset(set) => {
                let mark = self
                    .state
                    .data_marks
                    .get(&set.segment_id)
                    .copied()
                    .ok_or(CodegenError::DataSegmentNotFound { segment: set.segment_id })?;

                self.state.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(mark),
                    is_pushed: true,
                    set_size: None,
                }));

                self.store_local(set.local)
            }
            // SAFETY: Dispatch in translate_operation() guarantees only local operations reach here
            _ => unreachable!("Non-local operation in translate_local_op"),
        }
    }

    /// Translate call and contract creation operations
    fn translate_call_create_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            Operation::Call(large_in_one) => {
                // Args: [gas, address, value, argsOffset, argsSize, retOffset, retSize]
                self.emit_call_operation(Opcode::CALL, large_in_one, 7)
            }
            Operation::CallCode(large_in_one) => {
                // Args: [gas, address, value, argsOffset, argsSize, retOffset, retSize]
                self.emit_call_operation(Opcode::CALLCODE, large_in_one, 7)
            }
            Operation::DelegateCall(large_in_one) => {
                // Args: [gas, address, argsOffset, argsSize, retOffset, retSize]
                self.emit_call_operation(Opcode::DELEGATECALL, large_in_one, 6)
            }
            Operation::StaticCall(large_in_one) => {
                // Args: [gas, address, argsOffset, argsSize, retOffset, retSize]
                self.emit_call_operation(Opcode::STATICCALL, large_in_one, 6)
            }
            Operation::InternalCall(call) => {
                let return_mark = self.state.marks.allocate_mark();

                self.state.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(return_mark),
                    is_pushed: true,
                    set_size: None,
                }));

                let func_entry_block = self.program.program.functions[call.function].entry;
                let block_mark = self.state.marks.get_block_mark(func_entry_block);
                self.emit_jump(block_mark);

                self.emit_mark(return_mark);
                Ok(())
            }
            Operation::Create(large_in_one) => {
                // Args: [value, offset, size]
                self.emit_create_operation(Opcode::CREATE, large_in_one, 3)
            }
            Operation::Create2(large_in_one) => {
                // Args: [value, offset, size, salt]
                self.emit_create_operation(Opcode::CREATE2, large_in_one, 4)
            }
            Operation::SelfDestruct(one_in_zero) => {
                self.load_local(one_in_zero.arg1)?;
                self.state.asm.push(Asm::Op(Opcode::SELFDESTRUCT));
                Ok(())
            }
            // SAFETY: Dispatch in translate_operation() guarantees only call/create operations
            // reach here
            _ => unreachable!("Non-call/create operation in translate_call_create_op"),
        }
    }

    /// Translate environmental and blockchain information operations
    fn translate_environmental_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
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
            Operation::BlockHash(one_in_one) => {
                self.emit_one_in_one_op(Opcode::BLOCKHASH, one_in_one.arg1, one_in_one.result)
            }
            Operation::BlobHash(one_in_one) => {
                self.emit_one_in_one_op(Opcode::BLOBHASH, one_in_one.arg1, one_in_one.result)
            }
            // SAFETY: Dispatch in translate_operation() guarantees only environmental operations
            // reach here
            _ => unreachable!("Non-environmental operation in translate_environmental_op"),
        }
    }

    /// Translate storage operations (SLoad, SStore, TLoad, TStore)
    fn translate_storage_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            Operation::SLoad(one_in_one) => {
                self.emit_one_in_one_op(Opcode::SLOAD, one_in_one.arg1, one_in_one.result)
            }
            Operation::SStore(two_in_zero) => {
                self.emit_two_in_zero_op(Opcode::SSTORE, two_in_zero.arg1, two_in_zero.arg2, true)
            }
            Operation::TLoad(one_in_one) => {
                self.emit_one_in_one_op(Opcode::TLOAD, one_in_one.arg1, one_in_one.result)
            }
            Operation::TStore(two_in_zero) => {
                self.emit_two_in_zero_op(Opcode::TSTORE, two_in_zero.arg1, two_in_zero.arg2, true)
            }
            // SAFETY: Dispatch in translate_operation() guarantees only storage operations reach
            // here
            _ => unreachable!("Non-storage operation in translate_storage_op"),
        }
    }

    /// Translate memory operations (MemoryLoad, MemoryStore, allocations)
    fn translate_memory_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            Operation::AcquireFreePointer(zero_in_one) => {
                let ptr_loc = self.state.locals.get_free_memory_pointer_location();
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MLOAD));
                self.store_local(zero_in_one.result)
            }
            Operation::DynamicAllocZeroed(one_in_one) => {
                self.allocate_memory(one_in_one.arg1, true)?;
                self.store_local(one_in_one.result)
            }
            Operation::DynamicAllocAnyBytes(one_in_one) => {
                self.allocate_memory(one_in_one.arg1, false)?;
                self.store_local(one_in_one.result)
            }
            Operation::LocalAllocZeroed(one_in_one) => {
                self.allocate_memory(one_in_one.arg1, true)?;
                self.store_local(one_in_one.result)
            }
            Operation::LocalAllocAnyBytes(one_in_one) => {
                self.allocate_memory(one_in_one.arg1, false)?;
                self.store_local(one_in_one.result)
            }
            Operation::DynamicAllocUsingFreePointer(two_in_zero) => {
                self.load_local(two_in_zero.arg1)?;
                self.load_local(two_in_zero.arg2)?;
                self.state.asm.push(Asm::Op(Opcode::ADD));

                let ptr_loc = self.state.locals.get_free_memory_pointer_location();
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MSTORE));
                Ok(())
            }
            Operation::MemoryLoad(load) => {
                use super::constants::EVM_WORD_SIZE;
                // Well-formed IR should only contain valid byte sizes
                debug_assert_eq!(
                    load.byte_size, EVM_WORD_SIZE as u8,
                    "MemoryLoad byte_size must be {} for well-formed IR",
                    EVM_WORD_SIZE
                );
                self.load_local(load.address)?;
                self.state.asm.push(Asm::Op(Opcode::MLOAD));
                self.store_local(load.result)
            }
            Operation::MemoryStore(store) => {
                use super::constants::EVM_WORD_SIZE;
                // Well-formed IR should only contain valid byte sizes (1 or 32)
                debug_assert!(
                    store.byte_size == 1 || store.byte_size == EVM_WORD_SIZE as u8,
                    "MemoryStore byte_size must be 1 or {} for well-formed IR, got {}",
                    EVM_WORD_SIZE,
                    store.byte_size
                );

                self.load_local(store.value)?;
                self.load_local(store.address)?;

                if store.byte_size == 1 {
                    self.state.asm.push(Asm::Op(Opcode::MSTORE8));
                } else {
                    self.state.asm.push(Asm::Op(Opcode::MSTORE));
                }
                Ok(())
            }
            // SAFETY: Dispatch in translate_operation() guarantees only memory operations reach
            // here
            _ => unreachable!("Non-memory operation in translate_memory_op"),
        }
    }

    /// Translate I/O and data copy operations (CallDataCopy, CodeCopy, Log0-4, etc.)
    fn translate_io_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            Operation::Log0(two_in_zero) => {
                self.emit_two_in_zero_op(Opcode::LOG0, two_in_zero.arg1, two_in_zero.arg2, true)
            }
            Operation::Log1(three_in_zero) => self.emit_log_operation(
                Opcode::LOG1,
                three_in_zero.arg1,
                three_in_zero.arg2,
                &[three_in_zero.arg3],
            ),
            Operation::Log2(large_in_zero) => {
                // Args: [offset, size, topic1, topic2]
                let args = large_in_zero.get_args(&self.program.program.locals);
                self.emit_log_operation(Opcode::LOG2, args[0], args[1], &args[2..4])
            }
            Operation::Log3(large_in_zero) => {
                // Args: [offset, size, topic1, topic2, topic3]
                let args = large_in_zero.get_args(&self.program.program.locals);
                self.emit_log_operation(Opcode::LOG3, args[0], args[1], &args[2..5])
            }
            Operation::Log4(large_in_zero) => {
                // Args: [offset, size, topic1, topic2, topic3, topic4]
                let args = large_in_zero.get_args(&self.program.program.locals);
                self.emit_log_operation(Opcode::LOG4, args[0], args[1], &args[2..6])
            }
            Operation::CodeSize(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::CODESIZE, zero_in_one.result)
            }
            Operation::ReturnDataSize(zero_in_one) => {
                self.emit_zero_in_one_op(Opcode::RETURNDATASIZE, zero_in_one.result)
            }
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
                // Args: [address, destOffset, offset, size]
                let args = large_in_zero.get_args(&self.program.program.locals);
                self.load_local(args[0])?;
                self.load_local(args[1])?;
                self.load_local(args[2])?;
                self.load_local(args[3])?;
                self.state.asm.push(Asm::Op(Opcode::EXTCODECOPY));
                Ok(())
            }
            Operation::MCopy(three_in_zero) => {
                self.load_local(three_in_zero.arg3)?;
                self.load_local(three_in_zero.arg2)?;
                self.load_local(three_in_zero.arg1)?;
                self.state.asm.push(Asm::Op(Opcode::MCOPY));
                Ok(())
            }
            // SAFETY: Dispatch in translate_operation() guarantees only I/O operations reach here
            _ => unreachable!("Non-I/O operation in translate_io_op"),
        }
    }

    /// Translate control flow and terminal operations (Return, Revert, Stop, Invalid, NoOp)
    fn translate_control_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            Operation::Return(two_in_zero) => {
                if self.state.is_translating_init {
                    self.state.init_has_return = true;
                }
                self.emit_two_in_zero_op(Opcode::RETURN, two_in_zero.arg1, two_in_zero.arg2, true)
            }
            Operation::Revert(two_in_zero) => {
                self.emit_two_in_zero_op(Opcode::REVERT, two_in_zero.arg1, two_in_zero.arg2, true)
            }
            Operation::Stop => {
                self.state.asm.push(Asm::Op(Opcode::STOP));
                Ok(())
            }
            Operation::Invalid => {
                self.state.asm.push(Asm::Op(Opcode::INVALID));
                Ok(())
            }
            Operation::NoOp => Ok(()),
            // SAFETY: Dispatch in translate_operation() guarantees only control operations reach
            // here
            _ => unreachable!("Non-control operation in translate_control_op"),
        }
    }

    /// Translate bytecode introspection operations (RuntimeStartOffset, InitEndOffset,
    /// RuntimeLength)
    fn translate_introspection_op(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;

        match op {
            Operation::RuntimeStartOffset(zero_in_one) => {
                self.state.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(self.state.runtime_start_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result)
            }
            Operation::InitEndOffset(zero_in_one) => {
                self.state.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(self.state.init_end_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result)
            }
            Operation::RuntimeLength(zero_in_one) => {
                self.state.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Delta(
                        self.state.runtime_end_mark,
                        self.state.runtime_start_mark,
                    ),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result)
            }
            // SAFETY: Dispatch in translate_operation() guarantees only introspection operations
            // reach here
            _ => unreachable!("Non-introspection operation in translate_introspection_op"),
        }
    }
}
