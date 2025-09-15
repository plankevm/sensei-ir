//! Operation translation
//! Contains translate_operation, translate_two_arg_op, and translate_one_arg_op

use super::Translator;
use crate::error::{CodegenError, Result};
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
        self.asm.push(Asm::Op(opcode));
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
        self.asm.push(Asm::Op(opcode));
        self.store_local(one_in_one.result)?;
        Ok(())
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
                self.asm.push(Asm::Op(Opcode::SIGNEXTEND));
                self.store_local(two_in_one.result)?;
            }

            Operation::AddMod(large_in_one) => {
                // ADDMOD: (a + b) % N
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[2])?; // N (modulus) - pushed first, popped last
                self.load_local(args[1])?; // b - pushed second
                self.load_local(args[0])?; // a - pushed last, popped first
                self.asm.push(Asm::Op(Opcode::ADDMOD));
                self.store_local(large_in_one.result)?;
            }

            Operation::MulMod(large_in_one) => {
                // MULMOD: (a * b) % N
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[2])?; // N (modulus) - pushed first, popped last
                self.load_local(args[1])?; // b - pushed second
                self.load_local(args[0])?; // a - pushed last, popped first
                self.asm.push(Asm::Op(Opcode::MULMOD));
                self.store_local(large_in_one.result)?;
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
                self.push_const(U256::from(set_const.value));
                self.store_local(set_const.local)?;
            }

            // Set local to large constant from the constants array
            Operation::LocalSetLargeConst(set_large) => {
                // Check if the constant exists
                if set_large.cid.index() >= self.program.large_consts.len() {
                    return Err(CodegenError::InvalidLargeConstReference {
                        constant: set_large.cid,
                    });
                }
                // Get the constant from the large_consts array
                let value = self.program.large_consts[set_large.cid];
                self.push_const(value);
                self.store_local(set_large.local)?;
            }

            // External call operations
            Operation::Call(large_in_one) => {
                // CALL takes 7 args: gas, address, value, argsOffset, argsSize, retOffset, retSize
                self.validate_local_range(large_in_one.args_start.get() as usize, 7)?;
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0])?; // gas
                self.load_local(args[1])?; // address
                self.load_local(args[2])?; // value
                self.load_local(args[3])?; // argsOffset
                self.load_local(args[4])?; // argsSize
                self.load_local(args[5])?; // retOffset
                self.load_local(args[6])?; // retSize
                self.asm.push(Asm::Op(Opcode::CALL));
                self.store_local(large_in_one.result)?; // Store success (0 or 1)
            }

            Operation::CallCode(large_in_one) => {
                // CALLCODE takes 7 args: gas, address, value, argsOffset, argsSize, retOffset,
                // retSize
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0])?; // gas
                self.load_local(args[1])?; // address
                self.load_local(args[2])?; // value
                self.load_local(args[3])?; // argsOffset
                self.load_local(args[4])?; // argsSize
                self.load_local(args[5])?; // retOffset
                self.load_local(args[6])?; // retSize
                self.asm.push(Asm::Op(Opcode::CALLCODE));
                self.store_local(large_in_one.result)?; // Store success (0 or 1)
            }

            Operation::DelegateCall(large_in_one) => {
                // DELEGATECALL takes 6 args: gas, address, argsOffset, argsSize, retOffset, retSize
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0])?; // gas
                self.load_local(args[1])?; // address
                self.load_local(args[2])?; // argsOffset
                self.load_local(args[3])?; // argsSize
                self.load_local(args[4])?; // retOffset
                self.load_local(args[5])?; // retSize
                self.asm.push(Asm::Op(Opcode::DELEGATECALL));
                self.store_local(large_in_one.result)?; // Store success (0 or 1)
            }

            Operation::StaticCall(large_in_one) => {
                // STATICCALL takes 6 args: gas, address, argsOffset, argsSize, retOffset, retSize
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0])?; // gas
                self.load_local(args[1])?; // address
                self.load_local(args[2])?; // argsOffset
                self.load_local(args[3])?; // argsSize
                self.load_local(args[4])?; // retOffset
                self.load_local(args[5])?; // retSize
                self.asm.push(Asm::Op(Opcode::STATICCALL));
                self.store_local(large_in_one.result)?; // Store success (0 or 1)
            }

            // Internal call operations
            Operation::InternalCall(call) => {
                // TODO: Update calling convention for stack window approach
                // Future: Args will be in stack window slots, not memory
                // For now: Use memory-backed approach but structure it to be compatible

                // Create a mark for the return point
                let return_mark = self.marks.allocate_mark();

                // Save return address to memory (future: will stay on stack)
                // Using 0x60 as return address slot (ZERO_SLOT in memory layout)
                self.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(return_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.push_const(U256::from(super::memory::constants::ZERO_SLOT));
                self.asm.push(Asm::Op(Opcode::MSTORE));

                // Arguments are already in memory at args_start
                // Future: Will be in stack window, copied/spilled as needed

                // Jump to the function's entry block
                let func_entry_block = self.program.functions[call.function].entry;
                let block_mark = self.marks.get_block_mark(func_entry_block);
                self.emit_jump(block_mark);

                // Emit the return point mark
                self.emit_mark(return_mark);

                // After return, outputs will be in memory at outputs_start
                // Future: Will be in stack window slots
            }

            // Return operation
            Operation::Return(two_in_zero) => {
                // If we're translating init code, mark that we've seen a RETURN
                if self.is_translating_init {
                    self.init_has_return = true;
                }
                // RETURN expects offset on top of stack, size below
                self.load_local(two_in_zero.arg2)?; // size
                self.load_local(two_in_zero.arg1)?; // offset
                self.asm.push(Asm::Op(Opcode::RETURN));
            }

            // Environmental information operations
            Operation::Address(zero_in_one) => {
                // Get address of currently executing contract
                self.asm.push(Asm::Op(Opcode::ADDRESS));
                self.store_local(zero_in_one.result)?;
            }

            Operation::Caller(zero_in_one) => {
                // Get caller address (msg.sender)
                self.asm.push(Asm::Op(Opcode::CALLER));
                self.store_local(zero_in_one.result)?;
            }

            Operation::Origin(zero_in_one) => {
                // Get transaction origin (tx.origin)
                self.asm.push(Asm::Op(Opcode::ORIGIN));
                self.store_local(zero_in_one.result)?;
            }

            Operation::CallValue(zero_in_one) => {
                // Get msg.value (wei sent with call)
                self.asm.push(Asm::Op(Opcode::CALLVALUE));
                self.store_local(zero_in_one.result)?;
            }

            Operation::CallDataSize(zero_in_one) => {
                // Get size of calldata
                self.asm.push(Asm::Op(Opcode::CALLDATASIZE));
                self.store_local(zero_in_one.result)?;
            }

            Operation::GasPrice(zero_in_one) => {
                // Get gas price of transaction
                self.asm.push(Asm::Op(Opcode::GASPRICE));
                self.store_local(zero_in_one.result)?;
            }

            Operation::Gas(zero_in_one) => {
                // Get remaining gas
                self.asm.push(Asm::Op(Opcode::GAS));
                self.store_local(zero_in_one.result)?;
            }

            Operation::Balance(one_in_one) => {
                // Get balance of address
                self.load_local(one_in_one.arg1)?;
                self.asm.push(Asm::Op(Opcode::BALANCE));
                self.store_local(one_in_one.result)?;
            }

            Operation::CallDataLoad(one_in_one) => {
                // Load word from calldata at offset
                self.load_local(one_in_one.arg1)?;
                self.asm.push(Asm::Op(Opcode::CALLDATALOAD));
                self.store_local(one_in_one.result)?;
            }

            Operation::ExtCodeSize(one_in_one) => {
                // Get code size of external contract
                self.load_local(one_in_one.arg1)?;
                self.asm.push(Asm::Op(Opcode::EXTCODESIZE));
                self.store_local(one_in_one.result)?;
            }

            Operation::ExtCodeHash(one_in_one) => {
                // Get code hash of external contract
                self.load_local(one_in_one.arg1)?;
                self.asm.push(Asm::Op(Opcode::EXTCODEHASH));
                self.store_local(one_in_one.result)?;
            }

            // Block information operations
            Operation::Coinbase(zero_in_one) => {
                // Get block coinbase (miner) address
                self.asm.push(Asm::Op(Opcode::COINBASE));
                self.store_local(zero_in_one.result)?;
            }

            Operation::Timestamp(zero_in_one) => {
                // Get block timestamp
                self.asm.push(Asm::Op(Opcode::TIMESTAMP));
                self.store_local(zero_in_one.result)?;
            }

            Operation::Number(zero_in_one) => {
                // Get block number
                self.asm.push(Asm::Op(Opcode::NUMBER));
                self.store_local(zero_in_one.result)?;
            }

            Operation::Difficulty(zero_in_one) => {
                // Get block difficulty (prevrandao after merge)
                self.asm.push(Asm::Op(Opcode::PREVRANDAO));
                self.store_local(zero_in_one.result)?;
            }

            Operation::GasLimit(zero_in_one) => {
                // Get block gas limit
                self.asm.push(Asm::Op(Opcode::GASLIMIT));
                self.store_local(zero_in_one.result)?;
            }

            Operation::ChainId(zero_in_one) => {
                // Get chain ID
                self.asm.push(Asm::Op(Opcode::CHAINID));
                self.store_local(zero_in_one.result)?;
            }

            Operation::SelfBalance(zero_in_one) => {
                // Get balance of current contract
                self.asm.push(Asm::Op(Opcode::SELFBALANCE));
                self.store_local(zero_in_one.result)?;
            }

            Operation::BaseFee(zero_in_one) => {
                // Get base fee
                self.asm.push(Asm::Op(Opcode::BASEFEE));
                self.store_local(zero_in_one.result)?;
            }

            Operation::BlobBaseFee(zero_in_one) => {
                // Get blob base fee
                self.asm.push(Asm::Op(Opcode::BLOBBASEFEE));
                self.store_local(zero_in_one.result)?;
            }

            Operation::BlockHash(one_in_one) => {
                // Get block hash for given block number
                self.load_local(one_in_one.arg1)?;
                self.asm.push(Asm::Op(Opcode::BLOCKHASH));
                self.store_local(one_in_one.result)?;
            }

            Operation::BlobHash(one_in_one) => {
                // Get blob hash at index
                self.load_local(one_in_one.arg1)?;
                self.asm.push(Asm::Op(Opcode::BLOBHASH));
                self.store_local(one_in_one.result)?;
            }

            // Storage operations
            Operation::SLoad(one_in_one) => {
                // Load value from storage
                self.load_local(one_in_one.arg1)?; // storage key
                self.asm.push(Asm::Op(Opcode::SLOAD));
                self.store_local(one_in_one.result)?;
            }

            Operation::SStore(two_in_zero) => {
                // Store value to storage
                self.load_local(two_in_zero.arg1)?; // storage key
                self.load_local(two_in_zero.arg2)?; // value
                self.asm.push(Asm::Op(Opcode::SSTORE));
            }

            Operation::TLoad(one_in_one) => {
                // Load value from transient storage
                self.load_local(one_in_one.arg1)?; // storage key
                self.asm.push(Asm::Op(Opcode::TLOAD));
                self.store_local(one_in_one.result)?;
            }

            Operation::TStore(two_in_zero) => {
                // Store value to transient storage
                self.load_local(two_in_zero.arg1)?; // storage key
                self.load_local(two_in_zero.arg2)?; // value
                self.asm.push(Asm::Op(Opcode::TSTORE));
            }

            // Logging operations
            Operation::Log0(two_in_zero) => {
                // LOG0: offset, size
                self.load_local(two_in_zero.arg1)?; // memory offset
                self.load_local(two_in_zero.arg2)?; // size
                self.asm.push(Asm::Op(Opcode::LOG0));
            }

            Operation::Log1(three_in_zero) => {
                // LOG1 expects stack: [topic, size, offset] (from bottom to top)
                self.load_local(three_in_zero.arg3)?; // topic1
                self.load_local(three_in_zero.arg2)?; // size
                self.load_local(three_in_zero.arg1)?; // memory offset
                self.asm.push(Asm::Op(Opcode::LOG1));
            }

            Operation::Log2(large_in_zero) => {
                // LOG2 expects stack: [topic2, topic1, size, offset] (from bottom to top)
                let args = large_in_zero.get_args(&self.program.locals);
                self.load_local(args[3])?; // topic2
                self.load_local(args[2])?; // topic1
                self.load_local(args[1])?; // size
                self.load_local(args[0])?; // memory offset
                self.asm.push(Asm::Op(Opcode::LOG2));
            }

            Operation::Log3(large_in_zero) => {
                // LOG3 expects stack: [topic3, topic2, topic1, size, offset] (from bottom to top)
                let args = large_in_zero.get_args(&self.program.locals);
                self.load_local(args[4])?; // topic3
                self.load_local(args[3])?; // topic2
                self.load_local(args[2])?; // topic1
                self.load_local(args[1])?; // size
                self.load_local(args[0])?; // memory offset
                self.asm.push(Asm::Op(Opcode::LOG3));
            }

            Operation::Log4(large_in_zero) => {
                // LOG4 expects stack: [topic4, topic3, topic2, topic1, size, offset] (from bottom
                // to top)
                let args = large_in_zero.get_args(&self.program.locals);
                self.load_local(args[5])?; // topic4
                self.load_local(args[4])?; // topic3
                self.load_local(args[3])?; // topic2
                self.load_local(args[2])?; // topic1
                self.load_local(args[1])?; // size
                self.load_local(args[0])?; // memory offset
                self.asm.push(Asm::Op(Opcode::LOG4));
            }

            // Error handling
            Operation::Revert(two_in_zero) => {
                // REVERT expects offset on top of stack, size below
                self.load_local(two_in_zero.arg2)?; // size
                self.load_local(two_in_zero.arg1)?; // offset
                self.asm.push(Asm::Op(Opcode::REVERT));
            }

            // Contract creation and destruction
            Operation::Create(large_in_one) => {
                // CREATE: value, offset, size
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0])?; // value to send
                self.load_local(args[1])?; // memory offset of init code
                self.load_local(args[2])?; // size of init code
                self.asm.push(Asm::Op(Opcode::CREATE));
                self.store_local(large_in_one.result)?; // new contract address (or 0 on failure)
            }

            Operation::Create2(large_in_one) => {
                // CREATE2: value, offset, size, salt
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0])?; // value to send
                self.load_local(args[1])?; // memory offset of init code
                self.load_local(args[2])?; // size of init code
                self.load_local(args[3])?; // salt
                self.asm.push(Asm::Op(Opcode::CREATE2));
                self.store_local(large_in_one.result)?; // new contract address (or 0 on failure)
            }

            Operation::SelfDestruct(one_in_zero) => {
                // SELFDESTRUCT: beneficiary address
                self.load_local(one_in_zero.arg1)?; // beneficiary
                self.asm.push(Asm::Op(Opcode::SELFDESTRUCT));
            }

            // Simple operations
            Operation::CodeSize(zero_in_one) => {
                // Get size of current contract's code
                self.asm.push(Asm::Op(Opcode::CODESIZE));
                self.store_local(zero_in_one.result)?;
            }

            Operation::ReturnDataSize(zero_in_one) => {
                // Get size of return data from last call
                self.asm.push(Asm::Op(Opcode::RETURNDATASIZE));
                self.store_local(zero_in_one.result)?;
            }

            // Copy operations
            Operation::CallDataCopy(three_in_zero) => {
                // Copy calldata to memory: destOffset, dataOffset, size
                self.load_local(three_in_zero.arg1)?; // memory destination offset
                self.load_local(three_in_zero.arg2)?; // calldata source offset
                self.load_local(three_in_zero.arg3)?; // size
                self.asm.push(Asm::Op(Opcode::CALLDATACOPY));
            }

            Operation::CodeCopy(three_in_zero) => {
                // Copy code to memory: destOffset, codeOffset, size
                self.load_local(three_in_zero.arg1)?; // memory destination offset
                self.load_local(three_in_zero.arg2)?; // code source offset
                self.load_local(three_in_zero.arg3)?; // size
                self.asm.push(Asm::Op(Opcode::CODECOPY));
            }

            Operation::ReturnDataCopy(three_in_zero) => {
                // Copy return data to memory: destOffset, dataOffset, size
                self.load_local(three_in_zero.arg1)?; // memory destination offset
                self.load_local(three_in_zero.arg2)?; // return data source offset
                self.load_local(three_in_zero.arg3)?; // size
                self.asm.push(Asm::Op(Opcode::RETURNDATACOPY));
            }

            Operation::ExtCodeCopy(large_in_zero) => {
                // Copy external contract code to memory: address, destOffset, codeOffset, size
                let args = large_in_zero.get_args(&self.program.locals);
                self.load_local(args[0])?; // external contract address
                self.load_local(args[1])?; // memory destination offset
                self.load_local(args[2])?; // code source offset
                self.load_local(args[3])?; // size
                self.asm.push(Asm::Op(Opcode::EXTCODECOPY));
            }

            Operation::MCopy(three_in_zero) => {
                // Memory to memory copy: destOffset, srcOffset, size
                self.load_local(three_in_zero.arg1)?; // destination offset
                self.load_local(three_in_zero.arg2)?; // source offset
                self.load_local(three_in_zero.arg3)?; // size
                self.asm.push(Asm::Op(Opcode::MCOPY));
            }

            Operation::NoOp => {
                // No operation - do nothing
            }

            // Terminal operations
            Operation::Stop => {
                self.asm.push(Asm::Op(Opcode::STOP));
            }

            Operation::Invalid => {
                self.asm.push(Asm::Op(Opcode::INVALID));
            }

            // Bytecode introspection operations
            Operation::RuntimeStartOffset(zero_in_one) => {
                // Push byte offset where runtime starts in deployment bytecode
                self.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(self.runtime_start_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result)?;
            }

            Operation::InitEndOffset(zero_in_one) => {
                // Push byte offset where init code ends
                self.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(self.init_end_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result)?;
            }

            Operation::RuntimeLength(zero_in_one) => {
                // Push length of runtime code (not including data)
                // This is a Delta reference: runtime_end - runtime_start
                self.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Delta(self.runtime_end_mark, self.runtime_start_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result)?;
            }

            // Data segment reference
            Operation::LocalSetDataOffset(set) => {
                // Push the byte offset of this data segment
                let mark = self
                    .data_marks
                    .get(&set.segment_id)
                    .copied()
                    .ok_or(CodegenError::DataSegmentNotFound { segment: set.segment_id })?;

                self.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(mark),
                    is_pushed: true,
                    set_size: None,
                }));

                self.store_local(set.local)?;
            }

            // Memory management
            Operation::AcquireFreePointer(zero_in_one) => {
                // Load free memory pointer from 0x40
                self.push_const(U256::from(super::memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.store_local(zero_in_one.result)?;
            }

            Operation::DynamicAllocZeroed(one_in_one) => {
                // Allocate memory and zero it
                // Input: size, Output: pointer to allocated memory

                // Load current free memory pointer
                self.push_const(U256::from(super::memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::DUP1)); // Duplicate for later use
                self.asm.push(Asm::Op(Opcode::MLOAD)); // Load current free pointer
                self.asm.push(Asm::Op(Opcode::DUP1)); // This will be our return value

                // Calculate new free pointer (current + size)
                self.load_local(one_in_one.arg1)?; // Load size
                self.asm.push(Asm::Op(Opcode::DUP1)); // Keep size for zeroing
                self.asm.push(Asm::Op(Opcode::DUP3)); // Get current pointer
                self.asm.push(Asm::Op(Opcode::ADD)); // new_ptr = current + size

                // Store new free pointer
                self.asm.push(Asm::Op(Opcode::SWAP3)); // Move 0x40 to near top
                self.asm.push(Asm::Op(Opcode::MSTORE)); // Store new free pointer

                // Stack now: [ptr, size]
                // Zero out memory with a simple loop (32 bytes at a time)

                // Create loop marks
                let loop_start = self.marks.allocate_mark();
                let loop_end = self.marks.allocate_mark();

                // Calculate end pointer (ptr + size)
                self.asm.push(Asm::Op(Opcode::DUP1)); // [ptr, ptr, size]
                self.asm.push(Asm::Op(Opcode::DUP3)); // [size, ptr, ptr, size]
                self.asm.push(Asm::Op(Opcode::ADD)); // [end_ptr, ptr, size]

                // Stack: [end_ptr, ptr, size]
                // Swap to get [ptr, end_ptr, size] for loop
                self.asm.push(Asm::Op(Opcode::SWAP1)); // [ptr, end_ptr, size]

                // Loop start
                self.emit_mark(loop_start);

                // Stack: [current_ptr, end_ptr, size]
                // Check if we've reached the end
                self.asm.push(Asm::Op(Opcode::DUP2)); // [end_ptr, current_ptr, end_ptr, size]
                self.asm.push(Asm::Op(Opcode::DUP2)); // [current_ptr, end_ptr, current_ptr, end_ptr, size]
                self.asm.push(Asm::Op(Opcode::LT)); // [current < end, current_ptr, end_ptr, size]
                self.asm.push(Asm::Op(Opcode::ISZERO)); // [current >= end, current_ptr, end_ptr, size]
                self.emit_jumpi(loop_end);

                // Store 32 bytes of zeros at current position
                self.asm.push(Asm::Op(Opcode::PUSH0)); // [0, current_ptr, end_ptr, size]
                self.asm.push(Asm::Op(Opcode::DUP2)); // [current_ptr, 0, current_ptr, end_ptr, size]
                self.asm.push(Asm::Op(Opcode::MSTORE)); // Write 32 zero bytes

                // Move to next 32-byte chunk
                self.push_const(U256::from(32)); // [32, current_ptr, end_ptr, size]
                self.asm.push(Asm::Op(Opcode::ADD)); // [current_ptr+32, end_ptr, size]

                // Continue loop
                self.emit_jump(loop_start);

                // Loop end - clean up stack
                self.emit_mark(loop_end);
                self.asm.push(Asm::Op(Opcode::POP)); // Remove current_ptr
                self.asm.push(Asm::Op(Opcode::POP)); // Remove end_ptr

                // Store the allocated pointer in result
                self.store_local(one_in_one.result)?;
            }

            Operation::DynamicAllocAnyBytes(one_in_one) => {
                // Allocate memory without zeroing
                // Same as DynamicAllocZeroed but without zeroing

                // Load current free memory pointer
                self.push_const(U256::from(super::memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::DUP1));
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.asm.push(Asm::Op(Opcode::DUP1));

                // Calculate new free pointer
                self.load_local(one_in_one.arg1)?;
                self.asm.push(Asm::Op(Opcode::ADD));

                // Store new free pointer
                self.asm.push(Asm::Op(Opcode::SWAP1));
                self.asm.push(Asm::Op(Opcode::MSTORE));

                // Store result
                self.store_local(one_in_one.result)?;
            }

            Operation::LocalAllocZeroed(one_in_one) => {
                // Same implementation as DynamicAllocZeroed
                // Could use a different memory region in the future

                self.push_const(U256::from(super::memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::DUP1));
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.asm.push(Asm::Op(Opcode::DUP1));

                self.load_local(one_in_one.arg1)?;
                self.asm.push(Asm::Op(Opcode::DUP1));
                self.asm.push(Asm::Op(Opcode::DUP3));
                self.asm.push(Asm::Op(Opcode::ADD));

                self.asm.push(Asm::Op(Opcode::SWAP3));
                self.asm.push(Asm::Op(Opcode::MSTORE));

                // Zero out memory with a simple loop
                let loop_start = self.marks.allocate_mark();
                let loop_end = self.marks.allocate_mark();

                self.asm.push(Asm::Op(Opcode::DUP1));
                self.asm.push(Asm::Op(Opcode::DUP3));
                self.asm.push(Asm::Op(Opcode::ADD));
                self.asm.push(Asm::Op(Opcode::SWAP1));

                self.emit_mark(loop_start);

                self.asm.push(Asm::Op(Opcode::DUP2));
                self.asm.push(Asm::Op(Opcode::DUP2));
                self.asm.push(Asm::Op(Opcode::LT));
                self.asm.push(Asm::Op(Opcode::ISZERO));
                self.emit_jumpi(loop_end);

                self.asm.push(Asm::Op(Opcode::PUSH0));
                self.asm.push(Asm::Op(Opcode::DUP2));
                self.asm.push(Asm::Op(Opcode::MSTORE));

                self.push_const(U256::from(32));
                self.asm.push(Asm::Op(Opcode::ADD));

                self.emit_jump(loop_start);

                self.emit_mark(loop_end);
                self.asm.push(Asm::Op(Opcode::POP));
                self.asm.push(Asm::Op(Opcode::POP));

                self.store_local(one_in_one.result)?;
            }

            Operation::LocalAllocAnyBytes(one_in_one) => {
                // For now, treat the same as DynamicAllocAnyBytes

                self.push_const(U256::from(super::memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::DUP1));
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.asm.push(Asm::Op(Opcode::DUP1));

                self.load_local(one_in_one.arg1)?;
                self.asm.push(Asm::Op(Opcode::ADD));

                self.asm.push(Asm::Op(Opcode::SWAP1));
                self.asm.push(Asm::Op(Opcode::MSTORE));

                self.store_local(one_in_one.result)?;
            }

            Operation::DynamicAllocUsingFreePointer(two_in_zero) => {
                // Takes: current free pointer and size
                // Updates free pointer to current + size

                // Load current free pointer value
                self.load_local(two_in_zero.arg1)?;

                // Add size to get new free pointer
                self.load_local(two_in_zero.arg2)?;
                self.asm.push(Asm::Op(Opcode::ADD));

                // Store new free pointer
                self.push_const(U256::from(super::memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::MSTORE));
            }

            // Memory operations
            Operation::MemoryLoad(load) => {
                // Load from memory with variable byte size
                self.load_local(load.address)?;

                if load.byte_size == 32 {
                    // Full word load
                    self.asm.push(Asm::Op(Opcode::MLOAD));
                } else {
                    // Partial load - load full word then mask
                    self.asm.push(Asm::Op(Opcode::MLOAD));

                    // Shift right to align bytes to the right
                    // Shift amount = (32 - byte_size) * 8
                    let shift_bits = (32 - load.byte_size as u32) * 8;
                    if shift_bits > 0 {
                        self.push_const(U256::from(shift_bits));
                        self.asm.push(Asm::Op(Opcode::SHR));
                    }
                }

                self.store_local(load.result)?;
            }

            Operation::MemoryStore(store) => {
                // Store to memory with variable byte size

                if store.byte_size == 32 {
                    // Full word store
                    self.load_local(store.value)?;
                    self.load_local(store.address)?;
                    self.asm.push(Asm::Op(Opcode::MSTORE));
                } else if store.byte_size == 1 {
                    // Single byte store
                    self.load_local(store.value)?;
                    self.load_local(store.address)?;
                    self.asm.push(Asm::Op(Opcode::MSTORE8));
                } else {
                    // Partial store - need to preserve other bytes
                    // This is complex: load existing, mask, merge, store
                    // For now, simplified version that may overwrite adjacent bytes

                    // Shift value left to align with memory position
                    self.load_local(store.value)?;
                    let shift_bits = (32 - store.byte_size as u32) * 8;
                    if shift_bits > 0 {
                        self.push_const(U256::from(shift_bits));
                        self.asm.push(Asm::Op(Opcode::SHL));
                    }
                    // Store shifted value at address
                    self.load_local(store.address)?;
                    self.asm.push(Asm::Op(Opcode::MSTORE));
                }
            }
        }

        Ok(())
    }
}
