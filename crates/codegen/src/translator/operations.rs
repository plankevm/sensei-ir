//! Operation translation
//! Contains translate_operation, translate_two_arg_op, and translate_one_arg_op

use super::Translator;
use crate::error::{CodegenError, Result, runtime};
use alloy_primitives::U256;
use eth_ir_data::{Idx, operation::HasArgs};
use evm_glue::assembly::{Asm, MarkRef, RefType};

impl Translator {
    /// Translate a two-argument operation to EVM assembly
    /// `reversed` swaps argument order for SUB, DIV, MOD, etc.
    pub(super) fn translate_two_arg_op(
        &mut self,
        two_in_one: &eth_ir_data::operation::TwoInOneOut,
        opcode: evm_glue::opcodes::Opcode,
        reversed: bool,
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
    pub(super) fn translate_one_arg_op(
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
    pub(super) fn translate_operation_by_index(
        &mut self,
        op_idx: eth_ir_data::OperationIndex,
    ) -> Result<()> {
        // We need to clone to avoid borrow checker issues
        let op = self.program.program.operations[op_idx].clone();
        self.translate_operation(&op)
    }

    /// Main dispatch function for translating IR operations to EVM
    pub(super) fn translate_operation(&mut self, op: &eth_ir_data::Operation) -> Result<()> {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            // Arithmetic operations
            Operation::Add(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::ADD, false)?
            }

            Operation::Sub(two_in_one) => {
                // SUB: arg1 - arg2 (reversed stack order)
                self.translate_two_arg_op(two_in_one, Opcode::SUB, true)?
            }

            Operation::Mul(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::MUL, false)?
            }

            Operation::Div(two_in_one) => {
                // DIV: arg1 / arg2 (reversed stack order)
                self.translate_two_arg_op(two_in_one, Opcode::DIV, true)?
            }

            Operation::SDiv(two_in_one) => {
                // SDIV: signed division
                self.translate_two_arg_op(two_in_one, Opcode::SDIV, true)?
            }

            Operation::Mod(two_in_one) => {
                // MOD: arg1 % arg2 (reversed stack order)
                self.translate_two_arg_op(two_in_one, Opcode::MOD, true)?
            }

            Operation::SMod(two_in_one) => {
                // SMOD: signed modulo
                self.translate_two_arg_op(two_in_one, Opcode::SMOD, true)?
            }

            Operation::Exp(two_in_one) => {
                // EXP: base^exponent
                self.translate_two_arg_op(two_in_one, Opcode::EXP, true)?
            }

            Operation::SignExtend(two_in_one) => {
                // SIGNEXTEND takes byte_position (top) and value (second)
                // arg1 is byte_position, arg2 is value
                self.load_local(two_in_one.arg2)?; // value (deeper)
                self.load_local(two_in_one.arg1)?; // byte_position (top)
                self.state.asm.push(Asm::Op(Opcode::SIGNEXTEND));
                self.store_local(two_in_one.result)?;
            }

            Operation::AddMod(large_in_one) => {
                // ADDMOD: (a + b) % N
                let args = large_in_one.get_args(&self.program.program.locals);
                self.emit_three_arg_op(Opcode::ADDMOD, &args[0..3], large_in_one.result)?;
            }

            Operation::MulMod(large_in_one) => {
                // MULMOD: (a * b) % N
                let args = large_in_one.get_args(&self.program.program.locals);
                self.emit_three_arg_op(Opcode::MULMOD, &args[0..3], large_in_one.result)?;
            }

            // Bitwise operations
            Operation::And(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::AND, false)?
            }

            Operation::Or(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::OR, false)?
            }

            Operation::Xor(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::XOR, false)?
            }

            Operation::Not(one_in_one) => self.translate_one_arg_op(one_in_one, Opcode::NOT)?,

            Operation::Byte(two_in_one) => {
                // BYTE takes position (top) and value (second)
                // arg1 is position, arg2 is value
                self.translate_two_arg_op(two_in_one, Opcode::BYTE, true)?
            }

            Operation::Shl(two_in_one) => {
                // SHL takes shift amount (top) and value (second)
                // arg1 is shift amount, arg2 is value
                self.translate_two_arg_op(two_in_one, Opcode::SHL, true)?
            }

            Operation::Shr(two_in_one) => {
                // SHR takes shift amount (top) and value (second)
                // arg1 is shift amount, arg2 is value
                self.translate_two_arg_op(two_in_one, Opcode::SHR, true)?
            }

            Operation::Sar(two_in_one) => {
                // SAR takes shift amount (top) and value (second)
                // arg1 is shift amount, arg2 is value
                self.translate_two_arg_op(two_in_one, Opcode::SAR, true)?
            }

            // Hash operations
            Operation::Keccak256(two_in_one) => {
                // Keccak256 (SHA3 opcode)
                self.translate_two_arg_op(two_in_one, Opcode::SHA3, false)?
            }

            // Comparison operations
            Operation::Lt(two_in_one) => {
                // LT: arg1 < arg2
                self.translate_two_arg_op(two_in_one, Opcode::LT, true)?
            }

            Operation::Gt(two_in_one) => {
                // GT: arg1 > arg2
                self.translate_two_arg_op(two_in_one, Opcode::GT, true)?
            }

            Operation::SLt(two_in_one) => {
                // SLT: signed <
                self.translate_two_arg_op(two_in_one, Opcode::SLT, true)?
            }

            Operation::SGt(two_in_one) => {
                // SGT: signed >
                self.translate_two_arg_op(two_in_one, Opcode::SGT, true)?
            }

            Operation::Eq(two_in_one) => {
                self.translate_two_arg_op(two_in_one, Opcode::EQ, false)?
            }

            Operation::IsZero(one_in_one) => {
                self.translate_one_arg_op(one_in_one, Opcode::ISZERO)?
            }

            // Local assignment
            Operation::LocalSet(one_in_one) => {
                self.load_local(one_in_one.arg1)?;
                self.store_local(one_in_one.result)?;
            }

            // Set local to small constant
            Operation::LocalSetSmallConst(set_const) => {
                let value = U256::from(set_const.value);
                self.push_const(value);
                self.store_local(set_const.local)?;
            }

            // Set local to large constant from the constants array
            Operation::LocalSetLargeConst(set_large) => {
                // Check if the constant exists
                if set_large.cid.index() >= self.program.program.large_consts.len() {
                    return Err(CodegenError::InvalidLargeConstReference {
                        constant: set_large.cid,
                    });
                }
                // Get the constant from the large_consts array
                let value = self.program.program.large_consts[set_large.cid];
                self.push_const(value);
                self.store_local(set_large.local)?;
            }

            // External call operations
            Operation::Call(large_in_one) => {
                // CALL takes 7 args: gas, address, value, argsOffset, argsSize, retOffset, retSize
                self.validate_local_range(large_in_one.args_start.get() as usize, 7)?;
                self.emit_call_operation(Opcode::CALL, large_in_one, 7)?;
            }

            Operation::CallCode(large_in_one) => {
                // CALLCODE takes 7 args: gas, address, value, argsOffset, argsSize, retOffset,
                // retSize
                self.emit_call_operation(Opcode::CALLCODE, large_in_one, 7)?;
            }

            Operation::DelegateCall(large_in_one) => {
                // DELEGATECALL takes 6 args: gas, address, argsOffset, argsSize, retOffset, retSize
                self.emit_call_operation(Opcode::DELEGATECALL, large_in_one, 6)?;
            }

            Operation::StaticCall(large_in_one) => {
                // STATICCALL takes 6 args: gas, address, argsOffset, argsSize, retOffset, retSize
                self.emit_call_operation(Opcode::STATICCALL, large_in_one, 6)?;
            }

            // Internal call operations
            Operation::InternalCall(call) => {
                // Internal function call - Transfer control to another function
                // Push return address on stack and jump to the target function
                // The callee will use InternalReturn to jump back

                // Validate function reference
                if call.function.index() >= self.program.program.functions.len() {
                    return Err(CodegenError::InvalidFunctionReference { function: call.function });
                }

                let return_mark = self.state.marks.allocate_mark();

                // Push return address onto stack
                self.state.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(return_mark),
                    is_pushed: true,
                    set_size: None,
                }));

                // Jump to the target function
                let func_entry_block = self.program.program.functions[call.function].entry;
                let block_mark = self.state.marks.get_block_mark(func_entry_block);
                self.emit_jump(block_mark);

                // Mark where we return to
                self.emit_mark(return_mark);
            }

            // Return operation
            Operation::Return(two_in_zero) => {
                // If we're translating init code, mark that we've seen a RETURN
                if self.state.is_translating_init {
                    self.state.init_has_return = true;
                }
                // RETURN pops: offset (top), then size (second)
                // arg1=offset, arg2=size, reversed=true gives us [size, offset]
                self.emit_two_in_zero_op(Opcode::RETURN, two_in_zero.arg1, two_in_zero.arg2, true)?;
            }

            // Environmental information operations
            Operation::Address(zero_in_one) => {
                // Get address of currently executing contract
                self.emit_zero_in_one_op(Opcode::ADDRESS, zero_in_one.result)?;
            }

            Operation::Caller(zero_in_one) => {
                // Get caller address (msg.sender)
                self.emit_zero_in_one_op(Opcode::CALLER, zero_in_one.result)?;
            }

            Operation::Origin(zero_in_one) => {
                // Get transaction origin (tx.origin)
                self.emit_zero_in_one_op(Opcode::ORIGIN, zero_in_one.result)?;
            }

            Operation::CallValue(zero_in_one) => {
                // Get msg.value (wei sent with call)
                self.emit_zero_in_one_op(Opcode::CALLVALUE, zero_in_one.result)?;
            }

            Operation::CallDataSize(zero_in_one) => {
                // Get size of calldata
                self.emit_zero_in_one_op(Opcode::CALLDATASIZE, zero_in_one.result)?;
            }

            Operation::GasPrice(zero_in_one) => {
                // Get gas price of transaction
                self.emit_zero_in_one_op(Opcode::GASPRICE, zero_in_one.result)?;
            }

            Operation::Gas(zero_in_one) => {
                // Get remaining gas
                self.emit_zero_in_one_op(Opcode::GAS, zero_in_one.result)?;
            }

            Operation::Balance(one_in_one) => {
                // Get balance of address
                self.emit_one_in_one_op(Opcode::BALANCE, one_in_one.arg1, one_in_one.result)?;
            }

            Operation::CallDataLoad(one_in_one) => {
                // Load word from calldata at offset
                self.emit_one_in_one_op(Opcode::CALLDATALOAD, one_in_one.arg1, one_in_one.result)?;
            }

            Operation::ExtCodeSize(one_in_one) => {
                // Get code size of external contract
                self.emit_one_in_one_op(Opcode::EXTCODESIZE, one_in_one.arg1, one_in_one.result)?;
            }

            Operation::ExtCodeHash(one_in_one) => {
                // Get code hash of external contract
                self.emit_one_in_one_op(Opcode::EXTCODEHASH, one_in_one.arg1, one_in_one.result)?;
            }

            // Block information operations
            Operation::Coinbase(zero_in_one) => {
                // Get block coinbase (miner) address
                self.emit_zero_in_one_op(Opcode::COINBASE, zero_in_one.result)?;
            }

            Operation::Timestamp(zero_in_one) => {
                // Get block timestamp
                self.emit_zero_in_one_op(Opcode::TIMESTAMP, zero_in_one.result)?;
            }

            Operation::Number(zero_in_one) => {
                // Get block number
                self.emit_zero_in_one_op(Opcode::NUMBER, zero_in_one.result)?;
            }

            Operation::Difficulty(zero_in_one) => {
                // Get block difficulty (prevrandao after merge)
                self.emit_zero_in_one_op(Opcode::PREVRANDAO, zero_in_one.result)?;
            }

            Operation::GasLimit(zero_in_one) => {
                // Get block gas limit
                self.emit_zero_in_one_op(Opcode::GASLIMIT, zero_in_one.result)?;
            }

            Operation::ChainId(zero_in_one) => {
                // Get chain ID
                self.emit_zero_in_one_op(Opcode::CHAINID, zero_in_one.result)?;
            }

            Operation::SelfBalance(zero_in_one) => {
                // Get balance of current contract
                self.emit_zero_in_one_op(Opcode::SELFBALANCE, zero_in_one.result)?;
            }

            Operation::BaseFee(zero_in_one) => {
                // Get base fee
                self.emit_zero_in_one_op(Opcode::BASEFEE, zero_in_one.result)?;
            }

            Operation::BlobBaseFee(zero_in_one) => {
                // Get blob base fee
                self.emit_zero_in_one_op(Opcode::BLOBBASEFEE, zero_in_one.result)?;
            }

            Operation::BlockHash(one_in_one) => {
                // Get block hash for given block number
                self.emit_one_in_one_op(Opcode::BLOCKHASH, one_in_one.arg1, one_in_one.result)?;
            }

            Operation::BlobHash(one_in_one) => {
                // Get blob hash at index
                self.emit_one_in_one_op(Opcode::BLOBHASH, one_in_one.arg1, one_in_one.result)?;
            }

            // Storage operations
            Operation::SLoad(one_in_one) => {
                // Load value from storage
                self.emit_one_in_one_op(Opcode::SLOAD, one_in_one.arg1, one_in_one.result)?;
            }

            Operation::SStore(two_in_zero) => {
                // Store value to storage
                // SSTORE pops: key (top), then value (second)
                // arg1=key, arg2=value, reversed=true gives us [value, key]
                self.emit_two_in_zero_op(Opcode::SSTORE, two_in_zero.arg1, two_in_zero.arg2, true)?;
            }

            Operation::TLoad(one_in_one) => {
                // Load value from transient storage
                self.emit_one_in_one_op(Opcode::TLOAD, one_in_one.arg1, one_in_one.result)?;
            }

            Operation::TStore(two_in_zero) => {
                // Store value to transient storage
                // TSTORE pops: key (top), then value (second)
                // arg1=key, arg2=value, reversed=true gives us [value, key]
                self.emit_two_in_zero_op(Opcode::TSTORE, two_in_zero.arg1, two_in_zero.arg2, true)?;
            }

            // Logging operations
            Operation::Log0(two_in_zero) => {
                // LOG0 pops: offset (top), then size (second)
                // arg1=offset, arg2=size, reversed=true gives us [size, offset]
                self.emit_two_in_zero_op(Opcode::LOG0, two_in_zero.arg1, two_in_zero.arg2, true)?;
            }

            Operation::Log1(three_in_zero) => {
                // LOG1: offset, size, topic1
                self.emit_log_operation(
                    Opcode::LOG1,
                    three_in_zero.arg1,
                    three_in_zero.arg2,
                    &[three_in_zero.arg3],
                )?;
            }

            Operation::Log2(large_in_zero) => {
                // LOG2: offset, size, topic1, topic2
                let args = large_in_zero.get_args(&self.program.program.locals);
                self.emit_log_operation(Opcode::LOG2, args[0], args[1], &args[2..4])?;
            }

            Operation::Log3(large_in_zero) => {
                // LOG3: offset, size, topic1, topic2, topic3
                let args = large_in_zero.get_args(&self.program.program.locals);
                self.emit_log_operation(Opcode::LOG3, args[0], args[1], &args[2..5])?;
            }

            Operation::Log4(large_in_zero) => {
                // LOG4: offset, size, topic1, topic2, topic3, topic4
                let args = large_in_zero.get_args(&self.program.program.locals);
                self.emit_log_operation(Opcode::LOG4, args[0], args[1], &args[2..6])?;
            }

            // Error handling
            Operation::Revert(two_in_zero) => {
                // REVERT pops: offset (top), then size (second)
                // arg1=offset, arg2=size, reversed=true gives us [size, offset]
                self.emit_two_in_zero_op(Opcode::REVERT, two_in_zero.arg1, two_in_zero.arg2, true)?;
            }

            // Contract creation and destruction
            Operation::Create(large_in_one) => {
                // CREATE: value, offset, size
                self.emit_create_operation(Opcode::CREATE, large_in_one, 3)?;
            }

            Operation::Create2(large_in_one) => {
                // CREATE2: value, offset, size, salt
                self.emit_create_operation(Opcode::CREATE2, large_in_one, 4)?;
            }

            Operation::SelfDestruct(one_in_zero) => {
                // SELFDESTRUCT: beneficiary address
                self.load_local(one_in_zero.arg1)?; // beneficiary
                self.state.asm.push(Asm::Op(Opcode::SELFDESTRUCT));
            }

            // Simple operations
            Operation::CodeSize(zero_in_one) => {
                // Get size of current contract's code
                self.emit_zero_in_one_op(Opcode::CODESIZE, zero_in_one.result)?;
            }

            Operation::ReturnDataSize(zero_in_one) => {
                // Get size of return data from last call
                self.emit_zero_in_one_op(Opcode::RETURNDATASIZE, zero_in_one.result)?;
            }

            // Copy operations
            Operation::CallDataCopy(three_in_zero) => {
                // Copy calldata to memory: destOffset, dataOffset, size
                self.emit_three_in_zero_copy_op(
                    Opcode::CALLDATACOPY,
                    three_in_zero.arg1,
                    three_in_zero.arg2,
                    three_in_zero.arg3,
                )?;
            }

            Operation::CodeCopy(three_in_zero) => {
                // Copy code to memory: destOffset, codeOffset, size
                self.emit_three_in_zero_copy_op(
                    Opcode::CODECOPY,
                    three_in_zero.arg1,
                    three_in_zero.arg2,
                    three_in_zero.arg3,
                )?;
            }

            Operation::ReturnDataCopy(three_in_zero) => {
                // Copy return data to memory: destOffset, dataOffset, size
                self.emit_three_in_zero_copy_op(
                    Opcode::RETURNDATACOPY,
                    three_in_zero.arg1,
                    three_in_zero.arg2,
                    three_in_zero.arg3,
                )?;
            }

            Operation::ExtCodeCopy(large_in_zero) => {
                // Copy external contract code to memory: address, destOffset, codeOffset, size
                let args = large_in_zero.get_args(&self.program.program.locals);
                self.load_local(args[0])?; // external contract address
                self.load_local(args[1])?; // memory destination offset
                self.load_local(args[2])?; // code source offset
                self.load_local(args[3])?; // size
                self.state.asm.push(Asm::Op(Opcode::EXTCODECOPY));
            }

            Operation::MCopy(three_in_zero) => {
                // Memory to memory copy
                // MCOPY pops: destination (top), source (second), size (third)
                // We push in order: size, source, destination to get [size, source, destination]
                self.load_local(three_in_zero.arg3)?; // size
                self.load_local(three_in_zero.arg2)?; // source offset
                self.load_local(three_in_zero.arg1)?; // destination offset
                self.state.asm.push(Asm::Op(Opcode::MCOPY));
            }

            Operation::NoOp => {
                // No operation - do nothing
            }

            // Terminal operations
            Operation::Stop => {
                self.state.asm.push(Asm::Op(Opcode::STOP));
            }

            Operation::Invalid => {
                self.state.asm.push(Asm::Op(Opcode::INVALID));
            }

            // Bytecode introspection operations
            Operation::RuntimeStartOffset(zero_in_one) => {
                // Push byte offset where runtime starts in deployment bytecode
                self.state.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(self.state.runtime_start_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result)?;
            }

            Operation::InitEndOffset(zero_in_one) => {
                // Push byte offset where init code ends
                self.state.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(self.state.init_end_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result)?;
            }

            Operation::RuntimeLength(zero_in_one) => {
                // Push length of runtime code (not including data)
                // This is a Delta reference: runtime_end - runtime_start
                self.state.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Delta(
                        self.state.runtime_end_mark,
                        self.state.runtime_start_mark,
                    ),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result)?;
            }

            // Data segment reference
            Operation::LocalSetDataOffset(set) => {
                // Push the byte offset of this data segment
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

                self.store_local(set.local)?;
            }

            // Memory management
            Operation::AcquireFreePointer(zero_in_one) => {
                // Load current free memory pointer value
                let ptr_loc = self
                    .state
                    .locals
                    .get_free_memory_pointer_location()
                    .expect("AcquireFreePointer requires free memory pointer");
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MLOAD));
                self.store_local(zero_in_one.result)?;
            }

            Operation::DynamicAllocZeroed(one_in_one) => {
                // Allocate memory and zero it
                self.allocate_memory(one_in_one.arg1, true)?;
                self.store_local(one_in_one.result)?;
            }

            Operation::DynamicAllocAnyBytes(one_in_one) => {
                // Allocate memory without zeroing
                self.allocate_memory(one_in_one.arg1, false)?;
                self.store_local(one_in_one.result)?;
            }

            Operation::LocalAllocZeroed(one_in_one) => {
                // LocalAlloc is the same as DynamicAlloc
                self.allocate_memory(one_in_one.arg1, true)?;
                self.store_local(one_in_one.result)?;
            }

            Operation::LocalAllocAnyBytes(one_in_one) => {
                // LocalAlloc is the same as DynamicAlloc
                self.allocate_memory(one_in_one.arg1, false)?;
                self.store_local(one_in_one.result)?;
            }

            Operation::DynamicAllocUsingFreePointer(two_in_zero) => {
                // Takes: current free pointer and size
                // Updates free pointer to current + size
                // Precondition: arg1 should equal current free pointer

                // Load new value (current + size)
                self.load_local(two_in_zero.arg1)?;
                self.load_local(two_in_zero.arg2)?;
                self.state.asm.push(Asm::Op(Opcode::ADD));

                // Store to free memory pointer
                let ptr_loc = self
                    .state
                    .locals
                    .get_free_memory_pointer_location()
                    .expect("DynamicAllocUsingFreePointer requires free memory pointer");
                self.push_const(U256::from(ptr_loc));
                self.state.asm.push(Asm::Op(Opcode::MSTORE));
            }

            // Memory operations
            Operation::MemoryLoad(load) => {
                // Load from memory - only support 32 byte loads (EVM MLOAD)
                if load.byte_size != 32 {
                    // Invalid byte_size for MemoryLoad - emit runtime error
                    if self.state.enable_debug_assertions {
                        panic!("MemoryLoad with invalid byte_size: {}", load.byte_size);
                    }
                    self.emit_runtime_error(runtime::UNDEFINED_BEHAVIOR);
                } else {
                    self.load_local(load.address)?;
                    self.state.asm.push(Asm::Op(Opcode::MLOAD));
                    self.store_local(load.result)?;
                }
            }

            Operation::MemoryStore(store) => {
                // Store to memory - only support EVM native sizes
                match store.byte_size {
                    32 => {
                        // MSTORE - stores 32 bytes
                        // MSTORE pops: offset (top), then value (second)
                        // We push: value first, then offset, resulting in [value, offset]
                        self.load_local(store.value)?;
                        self.load_local(store.address)?;

                        // Emit bounds check if enabled
                        self.emit_memory_bounds_check();

                        self.state.asm.push(Asm::Op(Opcode::MSTORE));
                    }
                    1 => {
                        // MSTORE8 - stores 1 byte (lowest byte of the value)
                        // MSTORE8 pops: offset (top), then value (second)
                        // We push: value first, then offset, resulting in [value, offset]
                        self.load_local(store.value)?;
                        self.load_local(store.address)?;

                        // Emit bounds check if enabled
                        self.emit_memory_bounds_check();

                        self.state.asm.push(Asm::Op(Opcode::MSTORE8));
                    }
                    _ => {
                        // Invalid byte_size is undefined behavior
                        if self.state.enable_debug_assertions {
                            panic!("MemoryStore with invalid byte_size: {}", store.byte_size);
                        }
                        // Emit runtime error for undefined behavior
                        self.emit_runtime_error(runtime::UNDEFINED_BEHAVIOR);
                    }
                }
            }
        }

        Ok(())
    }
}
