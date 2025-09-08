//! Code generation from eth-ir to EVM bytecode
//!
//! This crate translates eth-ir programs to EVM assembly using evm-glue.
//!
//! Design approach:
//! - Memory-backed locals for simplicity (future: stack window optimization)
//! - Each LocalId gets a fixed memory slot
//! - Memory layout: 0x00-0x40 scratch, 0x40-0x60 free ptr, 0x60-0x80 return addr, 0x80+ locals

mod marks;
mod memory;

use alloy_primitives::U256;
use eth_ir_data::{BasicBlockId, Control, DataId, EthIRProgram, LocalId, operation::HasArgs};
use evm_glue::assembly::{Asm, MarkRef, RefType};
use marks::{MarkAllocator, MarkId};
use memory::MemoryLayout;
use std::collections::HashSet;

/// Main translator from IR to EVM assembly
pub struct IrToEvm {
    /// The IR program being translated
    program: EthIRProgram,

    /// Memory layout for locals
    memory: MemoryLayout,

    /// Mark allocator for jump labels
    marks: MarkAllocator,

    /// Tracks which blocks have been translated
    translated_blocks: HashSet<BasicBlockId>,

    /// Generated assembly instructions
    asm: Vec<Asm>,

    /// Section marks for bytecode layout
    init_end_mark: MarkId,
    runtime_start_mark: MarkId,
    runtime_end_mark: MarkId,

    /// Marks for data segments
    data_marks: std::collections::HashMap<DataId, MarkId>,
}

impl IrToEvm {
    /// Create a new translator for the given IR program
    pub fn new(program: EthIRProgram) -> Self {
        let mut marks = MarkAllocator::new();

        // Pre-allocate section marks
        let init_end_mark = marks.allocate_mark();
        let runtime_start_mark = init_end_mark; // Runtime immediately follows init
        let runtime_end_mark = marks.allocate_mark();

        Self {
            program,
            memory: MemoryLayout::new(),
            marks,
            translated_blocks: HashSet::new(),
            asm: Vec::new(),
            init_end_mark,
            runtime_start_mark,
            runtime_end_mark,
            data_marks: std::collections::HashMap::new(),
        }
    }

    /// Translate the IR program to EVM assembly
    pub fn translate(&mut self) {
        // Pre-allocate memory for all locals
        self.allocate_all_locals();

        // Pre-allocate marks for data segments
        for (segment_id, _) in self.program.data_segments_start.iter_enumerated() {
            let mark = self.marks.allocate_mark();
            self.data_marks.insert(segment_id, mark);
        }

        // Generate init code first
        self.generate_init_code();

        // Mark where init ends and runtime begins
        self.asm.push(Asm::Mark(self.init_end_mark));

        // Generate runtime code
        self.generate_runtime_code();

        // Mark where runtime ends
        self.asm.push(Asm::Mark(self.runtime_end_mark));

        // Embed data segments
        self.embed_data_segments();
    }

    /// Generate init code
    fn generate_init_code(&mut self) {
        // Initialize EVM state
        self.emit_initialization();

        // Translate init_entry function
        let init_entry_block = self.program.functions[self.program.init_entry].entry;
        let init_func_mark = self.marks.get_function_mark(self.program.init_entry);
        self.emit_mark(init_func_mark);
        self.translate_block(init_entry_block);

        // If init doesn't end with RETURN, add deployment return
        // TODO: Check if last operation was RETURN
        // For now, always add deployment return
        self.emit_deployment_return();
    }

    /// Generate runtime code
    fn generate_runtime_code(&mut self) {
        // If there's a main entry, translate it
        if let Some(main_entry) = self.program.main_entry {
            let main_entry_block = self.program.functions[main_entry].entry;
            let main_mark = self.marks.get_function_mark(main_entry);
            self.emit_mark(main_mark);
            self.translate_block(main_entry_block);
        }
    }

    /// Emit deployment return (copies runtime+data and returns it)
    fn emit_deployment_return(&mut self) {
        use evm_glue::opcodes::Opcode;

        // TODO: Calculate actual size of runtime + data
        // For now, use a placeholder

        // PUSH 0 (memory destination)
        self.push_const(U256::from(0));

        // PUSH runtime_start (source in code)
        self.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(self.runtime_start_mark),
            is_pushed: true,
            set_size: None,
        }));

        // PUSH size (runtime + data length)
        // TODO: This should be calculated properly
        self.push_const(U256::from(0x1000)); // Placeholder

        // CODECOPY
        self.asm.push(Asm::Op(Opcode::CODECOPY));

        // PUSH 0 (memory offset for return)
        self.push_const(U256::from(0));

        // PUSH size again
        self.push_const(U256::from(0x1000)); // Placeholder

        // RETURN
        self.asm.push(Asm::Op(Opcode::RETURN));
    }

    /// Embed data segments at the end of bytecode
    fn embed_data_segments(&mut self) {
        for (segment_id, _) in self.program.data_segments_start.iter_enumerated() {
            // Place mark for this segment
            if let Some(&mark) = self.data_marks.get(&segment_id) {
                self.asm.push(Asm::Mark(mark));
            }

            // Get segment bytes and embed as raw data
            let range = self.program.get_segment_range(segment_id);
            let mut bytes = Vec::new();
            for i in range.start.get()..range.end.get() {
                bytes.push(self.program.data_bytes[eth_ir_data::DataOffset::new(i)]);
            }

            if !bytes.is_empty() {
                self.asm.push(Asm::Data(bytes));
            }
        }
    }

    /// Emit initialization code for the EVM
    fn emit_initialization(&mut self) {
        use evm_glue::opcodes::Opcode;

        // Set up the free memory pointer at 0x40
        // The free memory pointer points to the start of free memory
        // We set it to point just after our locals area

        // Calculate where free memory starts (after all locals)
        // For now, use a conservative estimate
        let free_mem_start = 0x1000; // 4KB should be enough for locals

        // PUSH free_mem_start
        self.push_const(U256::from(free_mem_start));

        // PUSH 0x40 (free memory pointer location)
        self.push_const(U256::from(memory::constants::FREE_MEM_PTR));

        // MSTORE
        self.asm.push(Asm::Op(Opcode::MSTORE));
    }

    /// Pre-allocate memory for all locals in the program
    fn allocate_all_locals(&mut self) {
        // The program.locals is an arena/pool of LocalId references
        // Multiple entries might reference the same LocalId
        // We need to allocate memory for each UNIQUE LocalId

        // Collect unique LocalIds
        let mut seen = std::collections::HashSet::new();
        for local_id in self.program.locals.iter() {
            if seen.insert(*local_id) {
                // First time seeing this LocalId, allocate memory for it
                self.memory.allocate_local(*local_id);
            }
        }
    }

    /// Get the generated assembly
    pub fn into_asm(self) -> Vec<Asm> {
        self.asm
    }

    /// Translate a basic block
    fn translate_block(&mut self, block_id: BasicBlockId) {
        // Check if we've already translated this block
        if !self.translated_blocks.insert(block_id) {
            // Already translated, nothing to do
            return;
        }

        // Clone what we need from the block to avoid borrowing issues
        let operations_range = self.program.basic_blocks[block_id].operations.clone();
        let control = self.program.basic_blocks[block_id].control.clone();

        // Emit the block's mark (label)
        let block_mark = self.marks.get_block_mark(block_id);
        self.emit_mark(block_mark);

        // Handle block inputs
        // In our memory-backed approach, inputs are already in memory
        // from the caller, so we don't need to do anything special

        // Translate all operations in the block
        let operations: Vec<_> =
            self.program.operations[operations_range].iter().cloned().collect();
        for op in operations {
            self.translate_operation(&op);
        }

        // Handle block outputs
        // In our memory-backed approach, outputs are already in memory
        // after operations execute, so nothing special needed

        // Translate control flow
        self.translate_control(&control);

        // Recursively translate reachable blocks
        match &control {
            Control::ContinuesTo(next) => {
                self.translate_block(*next);
            }
            Control::Branches(branch) => {
                self.translate_block(branch.zero_target);
                self.translate_block(branch.non_zero_target);
            }
            Control::Switch(switch) => {
                // Translate all case targets
                let case_targets: Vec<_> =
                    self.program.cases[switch.cases].cases.iter().map(|c| c.target).collect();
                for target in case_targets {
                    self.translate_block(target);
                }
                if let Some(fallback) = switch.fallback {
                    self.translate_block(fallback);
                }
            }
            Control::LastOpTerminates | Control::InternalReturn => {
                // These don't continue to other blocks
            }
        }
    }

    /// Translate a single operation
    fn translate_operation(&mut self, op: &eth_ir_data::Operation) {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            // Arithmetic operations
            Operation::Add(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::ADD));
                self.store_local(two_in_one.result);
            }

            Operation::Sub(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::SUB));
                self.store_local(two_in_one.result);
            }

            Operation::Mul(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::MUL));
                self.store_local(two_in_one.result);
            }

            Operation::Div(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::DIV));
                self.store_local(two_in_one.result);
            }

            Operation::SDiv(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::SDIV));
                self.store_local(two_in_one.result);
            }

            Operation::Mod(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::MOD));
                self.store_local(two_in_one.result);
            }

            Operation::SMod(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::SMOD));
                self.store_local(two_in_one.result);
            }

            Operation::Exp(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::EXP));
                self.store_local(two_in_one.result);
            }

            Operation::SignExtend(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::SIGNEXTEND));
                self.store_local(two_in_one.result);
            }

            Operation::AddMod(large_in_one) => {
                // AddMod takes 3 args: (a + b) % n
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0]); // a
                self.load_local(args[1]); // b  
                self.load_local(args[2]); // n (modulus)
                self.asm.push(Asm::Op(Opcode::ADDMOD));
                self.store_local(large_in_one.result);
            }

            Operation::MulMod(large_in_one) => {
                // MulMod takes 3 args: (a * b) % n
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0]); // a
                self.load_local(args[1]); // b
                self.load_local(args[2]); // n (modulus)
                self.asm.push(Asm::Op(Opcode::MULMOD));
                self.store_local(large_in_one.result);
            }

            // Bitwise operations
            Operation::And(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::AND));
                self.store_local(two_in_one.result);
            }

            Operation::Or(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::OR));
                self.store_local(two_in_one.result);
            }

            Operation::Xor(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::XOR));
                self.store_local(two_in_one.result);
            }

            Operation::Not(one_in_one) => {
                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::NOT));
                self.store_local(one_in_one.result);
            }

            Operation::Byte(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::BYTE));
                self.store_local(two_in_one.result);
            }

            Operation::Shl(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::SHL));
                self.store_local(two_in_one.result);
            }

            Operation::Shr(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::SHR));
                self.store_local(two_in_one.result);
            }

            Operation::Sar(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::SAR));
                self.store_local(two_in_one.result);
            }

            // Hash operations
            Operation::Keccak256(two_in_one) => {
                // Keccak256 hash of memory region
                // arg1 = offset, arg2 = size, result = hash
                self.load_local(two_in_one.arg1); // memory offset
                self.load_local(two_in_one.arg2); // size
                // Note: EVM opcode is called SHA3 for historical reasons, but it's actually
                // Keccak256
                self.asm.push(Asm::Op(Opcode::SHA3));
                self.store_local(two_in_one.result);
            }

            // Comparison operations
            Operation::Lt(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::LT));
                self.store_local(two_in_one.result);
            }

            Operation::Gt(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::GT));
                self.store_local(two_in_one.result);
            }

            Operation::SLt(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::SLT));
                self.store_local(two_in_one.result);
            }

            Operation::SGt(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::SGT));
                self.store_local(two_in_one.result);
            }

            Operation::Eq(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::EQ));
                self.store_local(two_in_one.result);
            }

            Operation::IsZero(one_in_one) => {
                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::ISZERO));
                self.store_local(one_in_one.result);
            }

            // Local assignment
            Operation::LocalSet(one_in_one) => {
                self.load_local(one_in_one.arg1);
                self.store_local(one_in_one.result);
            }

            // Set local to small constant
            Operation::LocalSetSmallConst(set_const) => {
                self.push_const(U256::from(set_const.value));
                self.store_local(set_const.local);
            }

            // Set local to large constant from the constants array
            Operation::LocalSetLargeConst(set_large) => {
                // Get the constant from the large_consts array
                let value = self.program.large_consts[set_large.cid];
                self.push_const(value);
                self.store_local(set_large.local);
            }

            // External call operations
            Operation::Call(large_in_one) => {
                // CALL takes 7 args: gas, address, value, argsOffset, argsSize, retOffset, retSize
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0]); // gas
                self.load_local(args[1]); // address
                self.load_local(args[2]); // value
                self.load_local(args[3]); // argsOffset
                self.load_local(args[4]); // argsSize
                self.load_local(args[5]); // retOffset
                self.load_local(args[6]); // retSize
                self.asm.push(Asm::Op(Opcode::CALL));
                self.store_local(large_in_one.result); // Store success (0 or 1)
            }

            Operation::CallCode(large_in_one) => {
                // CALLCODE takes 7 args: gas, address, value, argsOffset, argsSize, retOffset,
                // retSize
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0]); // gas
                self.load_local(args[1]); // address
                self.load_local(args[2]); // value
                self.load_local(args[3]); // argsOffset
                self.load_local(args[4]); // argsSize
                self.load_local(args[5]); // retOffset
                self.load_local(args[6]); // retSize
                self.asm.push(Asm::Op(Opcode::CALLCODE));
                self.store_local(large_in_one.result); // Store success (0 or 1)
            }

            Operation::DelegateCall(large_in_one) => {
                // DELEGATECALL takes 6 args: gas, address, argsOffset, argsSize, retOffset, retSize
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0]); // gas
                self.load_local(args[1]); // address
                self.load_local(args[2]); // argsOffset
                self.load_local(args[3]); // argsSize
                self.load_local(args[4]); // retOffset
                self.load_local(args[5]); // retSize
                self.asm.push(Asm::Op(Opcode::DELEGATECALL));
                self.store_local(large_in_one.result); // Store success (0 or 1)
            }

            Operation::StaticCall(large_in_one) => {
                // STATICCALL takes 6 args: gas, address, argsOffset, argsSize, retOffset, retSize
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0]); // gas
                self.load_local(args[1]); // address
                self.load_local(args[2]); // argsOffset
                self.load_local(args[3]); // argsSize
                self.load_local(args[4]); // retOffset
                self.load_local(args[5]); // retSize
                self.asm.push(Asm::Op(Opcode::STATICCALL));
                self.store_local(large_in_one.result); // Store success (0 or 1)
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
                self.push_const(U256::from(memory::constants::ZERO_SLOT));
                self.asm.push(Asm::Op(Opcode::MSTORE));

                // Arguments are already in memory at args_start
                // Future: Will be in stack window, copied/spilled as needed

                // Jump to the function
                let func_mark = self.marks.get_function_mark(call.function);
                self.emit_jump(func_mark);

                // Emit the return point mark
                self.emit_mark(return_mark);

                // After return, outputs will be in memory at outputs_start
                // Future: Will be in stack window slots
            }

            // Return operation
            Operation::Return(two_in_zero) => {
                // RETURN takes offset and size from memory
                self.load_local(two_in_zero.arg1); // offset
                self.load_local(two_in_zero.arg2); // size
                self.asm.push(Asm::Op(Opcode::RETURN));
            }

            // Environmental information operations
            Operation::Address(zero_in_one) => {
                // Get address of currently executing contract
                self.asm.push(Asm::Op(Opcode::ADDRESS));
                self.store_local(zero_in_one.result);
            }

            Operation::Caller(zero_in_one) => {
                // Get caller address (msg.sender)
                self.asm.push(Asm::Op(Opcode::CALLER));
                self.store_local(zero_in_one.result);
            }

            Operation::Origin(zero_in_one) => {
                // Get transaction origin (tx.origin)
                self.asm.push(Asm::Op(Opcode::ORIGIN));
                self.store_local(zero_in_one.result);
            }

            Operation::CallValue(zero_in_one) => {
                // Get msg.value (wei sent with call)
                self.asm.push(Asm::Op(Opcode::CALLVALUE));
                self.store_local(zero_in_one.result);
            }

            Operation::CallDataSize(zero_in_one) => {
                // Get size of calldata
                self.asm.push(Asm::Op(Opcode::CALLDATASIZE));
                self.store_local(zero_in_one.result);
            }

            Operation::GasPrice(zero_in_one) => {
                // Get gas price of transaction
                self.asm.push(Asm::Op(Opcode::GASPRICE));
                self.store_local(zero_in_one.result);
            }

            Operation::Gas(zero_in_one) => {
                // Get remaining gas
                self.asm.push(Asm::Op(Opcode::GAS));
                self.store_local(zero_in_one.result);
            }

            Operation::Balance(one_in_one) => {
                // Get balance of address
                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::BALANCE));
                self.store_local(one_in_one.result);
            }

            Operation::CallDataLoad(one_in_one) => {
                // Load word from calldata at offset
                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::CALLDATALOAD));
                self.store_local(one_in_one.result);
            }

            Operation::ExtCodeSize(one_in_one) => {
                // Get code size of external contract
                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::EXTCODESIZE));
                self.store_local(one_in_one.result);
            }

            Operation::ExtCodeHash(one_in_one) => {
                // Get code hash of external contract
                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::EXTCODEHASH));
                self.store_local(one_in_one.result);
            }

            // Block information operations
            Operation::Coinbase(zero_in_one) => {
                // Get block coinbase (miner) address
                self.asm.push(Asm::Op(Opcode::COINBASE));
                self.store_local(zero_in_one.result);
            }

            Operation::Timestamp(zero_in_one) => {
                // Get block timestamp
                self.asm.push(Asm::Op(Opcode::TIMESTAMP));
                self.store_local(zero_in_one.result);
            }

            Operation::Number(zero_in_one) => {
                // Get block number
                self.asm.push(Asm::Op(Opcode::NUMBER));
                self.store_local(zero_in_one.result);
            }

            Operation::Difficulty(zero_in_one) => {
                // Get block difficulty (prevrandao after merge)
                self.asm.push(Asm::Op(Opcode::PREVRANDAO));
                self.store_local(zero_in_one.result);
            }

            Operation::GasLimit(zero_in_one) => {
                // Get block gas limit
                self.asm.push(Asm::Op(Opcode::GASLIMIT));
                self.store_local(zero_in_one.result);
            }

            Operation::ChainId(zero_in_one) => {
                // Get chain ID
                self.asm.push(Asm::Op(Opcode::CHAINID));
                self.store_local(zero_in_one.result);
            }

            Operation::SelfBalance(zero_in_one) => {
                // Get balance of current contract
                self.asm.push(Asm::Op(Opcode::SELFBALANCE));
                self.store_local(zero_in_one.result);
            }

            Operation::BaseFee(zero_in_one) => {
                // Get base fee
                self.asm.push(Asm::Op(Opcode::BASEFEE));
                self.store_local(zero_in_one.result);
            }

            Operation::BlobBaseFee(zero_in_one) => {
                // Get blob base fee
                self.asm.push(Asm::Op(Opcode::BLOBBASEFEE));
                self.store_local(zero_in_one.result);
            }

            Operation::BlockHash(one_in_one) => {
                // Get block hash for given block number
                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::BLOCKHASH));
                self.store_local(one_in_one.result);
            }

            Operation::BlobHash(one_in_one) => {
                // Get blob hash at index
                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::BLOBHASH));
                self.store_local(one_in_one.result);
            }

            // Storage operations
            Operation::SLoad(one_in_one) => {
                // Load value from storage
                self.load_local(one_in_one.arg1); // storage key
                self.asm.push(Asm::Op(Opcode::SLOAD));
                self.store_local(one_in_one.result);
            }

            Operation::SStore(two_in_zero) => {
                // Store value to storage
                self.load_local(two_in_zero.arg1); // storage key
                self.load_local(two_in_zero.arg2); // value
                self.asm.push(Asm::Op(Opcode::SSTORE));
            }

            Operation::TLoad(one_in_one) => {
                // Load value from transient storage
                self.load_local(one_in_one.arg1); // storage key
                self.asm.push(Asm::Op(Opcode::TLOAD));
                self.store_local(one_in_one.result);
            }

            Operation::TStore(two_in_zero) => {
                // Store value to transient storage
                self.load_local(two_in_zero.arg1); // storage key
                self.load_local(two_in_zero.arg2); // value
                self.asm.push(Asm::Op(Opcode::TSTORE));
            }

            // Logging operations
            Operation::Log0(two_in_zero) => {
                // LOG0: offset, size
                self.load_local(two_in_zero.arg1); // memory offset
                self.load_local(two_in_zero.arg2); // size
                self.asm.push(Asm::Op(Opcode::LOG0));
            }

            Operation::Log1(three_in_zero) => {
                // LOG1: offset, size, topic1
                self.load_local(three_in_zero.arg1); // memory offset
                self.load_local(three_in_zero.arg2); // size
                self.load_local(three_in_zero.arg3); // topic1
                self.asm.push(Asm::Op(Opcode::LOG1));
            }

            Operation::Log2(large_in_zero) => {
                // LOG2: offset, size, topic1, topic2
                let args = large_in_zero.get_args(&self.program.locals);
                self.load_local(args[0]); // memory offset
                self.load_local(args[1]); // size
                self.load_local(args[2]); // topic1
                self.load_local(args[3]); // topic2
                self.asm.push(Asm::Op(Opcode::LOG2));
            }

            Operation::Log3(large_in_zero) => {
                // LOG3: offset, size, topic1, topic2, topic3
                let args = large_in_zero.get_args(&self.program.locals);
                self.load_local(args[0]); // memory offset
                self.load_local(args[1]); // size
                self.load_local(args[2]); // topic1
                self.load_local(args[3]); // topic2
                self.load_local(args[4]); // topic3
                self.asm.push(Asm::Op(Opcode::LOG3));
            }

            Operation::Log4(large_in_zero) => {
                // LOG4: offset, size, topic1, topic2, topic3, topic4
                let args = large_in_zero.get_args(&self.program.locals);
                self.load_local(args[0]); // memory offset
                self.load_local(args[1]); // size
                self.load_local(args[2]); // topic1
                self.load_local(args[3]); // topic2
                self.load_local(args[4]); // topic3
                self.load_local(args[5]); // topic4
                self.asm.push(Asm::Op(Opcode::LOG4));
            }

            // Error handling
            Operation::Revert(two_in_zero) => {
                // REVERT: offset, size (like RETURN but reverts)
                self.load_local(two_in_zero.arg1); // memory offset
                self.load_local(two_in_zero.arg2); // size
                self.asm.push(Asm::Op(Opcode::REVERT));
            }

            // Contract creation and destruction
            Operation::Create(large_in_one) => {
                // CREATE: value, offset, size
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0]); // value to send
                self.load_local(args[1]); // memory offset of init code
                self.load_local(args[2]); // size of init code
                self.asm.push(Asm::Op(Opcode::CREATE));
                self.store_local(large_in_one.result); // new contract address (or 0 on failure)
            }

            Operation::Create2(large_in_one) => {
                // CREATE2: value, offset, size, salt
                let args = large_in_one.get_args(&self.program.locals);
                self.load_local(args[0]); // value to send
                self.load_local(args[1]); // memory offset of init code
                self.load_local(args[2]); // size of init code
                self.load_local(args[3]); // salt
                self.asm.push(Asm::Op(Opcode::CREATE2));
                self.store_local(large_in_one.result); // new contract address (or 0 on failure)
            }

            Operation::SelfDestruct(one_in_zero) => {
                // SELFDESTRUCT: beneficiary address
                self.load_local(one_in_zero.arg1); // beneficiary
                self.asm.push(Asm::Op(Opcode::SELFDESTRUCT));
            }

            // Simple operations
            Operation::CodeSize(zero_in_one) => {
                // Get size of current contract's code
                self.asm.push(Asm::Op(Opcode::CODESIZE));
                self.store_local(zero_in_one.result);
            }

            Operation::ReturnDataSize(zero_in_one) => {
                // Get size of return data from last call
                self.asm.push(Asm::Op(Opcode::RETURNDATASIZE));
                self.store_local(zero_in_one.result);
            }

            // Copy operations
            Operation::CallDataCopy(three_in_zero) => {
                // Copy calldata to memory: destOffset, dataOffset, size
                self.load_local(three_in_zero.arg1); // memory destination offset
                self.load_local(three_in_zero.arg2); // calldata source offset
                self.load_local(three_in_zero.arg3); // size
                self.asm.push(Asm::Op(Opcode::CALLDATACOPY));
            }

            Operation::CodeCopy(three_in_zero) => {
                // Copy code to memory: destOffset, codeOffset, size
                self.load_local(three_in_zero.arg1); // memory destination offset
                self.load_local(three_in_zero.arg2); // code source offset
                self.load_local(three_in_zero.arg3); // size
                self.asm.push(Asm::Op(Opcode::CODECOPY));
            }

            Operation::ReturnDataCopy(three_in_zero) => {
                // Copy return data to memory: destOffset, dataOffset, size
                self.load_local(three_in_zero.arg1); // memory destination offset
                self.load_local(three_in_zero.arg2); // return data source offset
                self.load_local(three_in_zero.arg3); // size
                self.asm.push(Asm::Op(Opcode::RETURNDATACOPY));
            }

            Operation::ExtCodeCopy(large_in_zero) => {
                // Copy external contract code to memory: address, destOffset, codeOffset, size
                let args = large_in_zero.get_args(&self.program.locals);
                self.load_local(args[0]); // external contract address
                self.load_local(args[1]); // memory destination offset
                self.load_local(args[2]); // code source offset
                self.load_local(args[3]); // size
                self.asm.push(Asm::Op(Opcode::EXTCODECOPY));
            }

            Operation::MCopy(three_in_zero) => {
                // Memory to memory copy: destOffset, srcOffset, size
                self.load_local(three_in_zero.arg1); // destination offset
                self.load_local(three_in_zero.arg2); // source offset
                self.load_local(three_in_zero.arg3); // size
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
                self.store_local(zero_in_one.result);
            }

            Operation::InitEndOffset(zero_in_one) => {
                // Push byte offset where init code ends
                self.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(self.init_end_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result);
            }

            Operation::RuntimeLength(zero_in_one) => {
                // Push length of runtime code (not including data)
                // This is a Delta reference: runtime_end - runtime_start
                self.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Delta(self.runtime_end_mark, self.runtime_start_mark),
                    is_pushed: true,
                    set_size: None,
                }));
                self.store_local(zero_in_one.result);
            }

            // Data segment reference
            Operation::LocalSetDataOffset(set) => {
                // Push the byte offset of this data segment
                let mark = self
                    .data_marks
                    .get(&set.segment_id)
                    .copied()
                    .unwrap_or_else(|| panic!("Data segment {:?} not found", set.segment_id));

                self.asm.push(Asm::Ref(MarkRef {
                    ref_type: RefType::Direct(mark),
                    is_pushed: true,
                    set_size: None,
                }));

                self.store_local(set.local);
            }

            // Memory management
            Operation::AcquireFreePointer(zero_in_one) => {
                // Load free memory pointer from 0x40
                self.push_const(U256::from(memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.store_local(zero_in_one.result);
            }

            Operation::DynamicAllocZeroed(one_in_one) => {
                // Allocate memory and zero it
                // Input: size, Output: pointer to allocated memory

                // Load current free memory pointer
                self.push_const(U256::from(memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::DUP1)); // Duplicate for later use
                self.asm.push(Asm::Op(Opcode::MLOAD)); // Load current free pointer
                self.asm.push(Asm::Op(Opcode::DUP1)); // This will be our return value

                // Calculate new free pointer (current + size)
                self.load_local(one_in_one.arg1); // Load size
                self.asm.push(Asm::Op(Opcode::ADD));

                // Store new free pointer
                self.asm.push(Asm::Op(Opcode::SWAP1)); // Swap with 0x40 address
                self.asm.push(Asm::Op(Opcode::MSTORE)); // Store new free pointer

                // Store the allocated pointer in result
                self.store_local(one_in_one.result);

                // TODO: Zero out the allocated memory (expensive)
            }

            Operation::DynamicAllocAnyBytes(one_in_one) => {
                // Allocate memory without zeroing
                // Same as DynamicAllocZeroed but without zeroing

                // Load current free memory pointer
                self.push_const(U256::from(memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::DUP1));
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.asm.push(Asm::Op(Opcode::DUP1));

                // Calculate new free pointer
                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::ADD));

                // Store new free pointer
                self.asm.push(Asm::Op(Opcode::SWAP1));
                self.asm.push(Asm::Op(Opcode::MSTORE));

                // Store result
                self.store_local(one_in_one.result);
            }

            Operation::LocalAllocZeroed(one_in_one) => {
                // For now, treat the same as DynamicAllocZeroed
                // Could use a different memory region in the future

                self.push_const(U256::from(memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::DUP1));
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.asm.push(Asm::Op(Opcode::DUP1));

                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::ADD));

                self.asm.push(Asm::Op(Opcode::SWAP1));
                self.asm.push(Asm::Op(Opcode::MSTORE));

                self.store_local(one_in_one.result);
            }

            Operation::LocalAllocAnyBytes(one_in_one) => {
                // For now, treat the same as DynamicAllocAnyBytes

                self.push_const(U256::from(memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::DUP1));
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.asm.push(Asm::Op(Opcode::DUP1));

                self.load_local(one_in_one.arg1);
                self.asm.push(Asm::Op(Opcode::ADD));

                self.asm.push(Asm::Op(Opcode::SWAP1));
                self.asm.push(Asm::Op(Opcode::MSTORE));

                self.store_local(one_in_one.result);
            }

            Operation::DynamicAllocUsingFreePointer(two_in_zero) => {
                // Takes: current free pointer and size
                // Updates free pointer to current + size

                // Load current free pointer value
                self.load_local(two_in_zero.arg1);

                // Add size to get new free pointer
                self.load_local(two_in_zero.arg2);
                self.asm.push(Asm::Op(Opcode::ADD));

                // Store new free pointer
                self.push_const(U256::from(memory::constants::FREE_MEM_PTR));
                self.asm.push(Asm::Op(Opcode::MSTORE));
            }

            // Memory operations
            Operation::MemoryLoad(load) => {
                // Load from memory with variable byte size
                self.load_local(load.address);

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

                self.store_local(load.result);
            }

            Operation::MemoryStore(store) => {
                // Store to memory with variable byte size

                if store.byte_size == 32 {
                    // Full word store
                    self.load_local(store.value);
                    self.load_local(store.address);
                    self.asm.push(Asm::Op(Opcode::MSTORE));
                } else if store.byte_size == 1 {
                    // Single byte store
                    self.load_local(store.value);
                    self.load_local(store.address);
                    self.asm.push(Asm::Op(Opcode::MSTORE8));
                } else {
                    // Partial store - need to preserve other bytes
                    // This is complex: load existing, mask, merge, store
                    // For now, simplified version that may overwrite adjacent bytes

                    // Shift value left to align with memory position
                    self.load_local(store.value);
                    let shift_bits = (32 - store.byte_size as u32) * 8;
                    if shift_bits > 0 {
                        self.push_const(U256::from(shift_bits));
                        self.asm.push(Asm::Op(Opcode::SHL));
                    }

                    self.load_local(store.address);
                    self.asm.push(Asm::Op(Opcode::MSTORE));
                }
            }
        }
    }

    /// Translate control flow at the end of a basic block
    fn translate_control(&mut self, control: &Control) {
        match control {
            Control::LastOpTerminates => {
                // The last operation (like STOP or RETURN) handles termination
                // Nothing to do here
            }

            Control::ContinuesTo(next_block) => {
                // Unconditional jump to next block
                let next_mark = self.marks.get_block_mark(*next_block);
                self.emit_jump(next_mark);
            }

            Control::Branches(branch) => {
                // Conditional branch
                self.load_local(branch.condition);

                // EVM's JUMPI jumps if condition is non-zero
                let non_zero_mark = self.marks.get_block_mark(branch.non_zero_target);
                self.emit_jumpi(non_zero_mark);

                // If we didn't jump, continue to zero target
                let zero_mark = self.marks.get_block_mark(branch.zero_target);
                self.emit_jump(zero_mark);
            }

            Control::InternalReturn => {
                // TODO: Update for stack window approach
                // Load return address from memory and jump back
                // Future: Return address will be on stack
                self.push_const(U256::from(memory::constants::ZERO_SLOT));
                use evm_glue::opcodes::Opcode;
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.asm.push(Asm::Op(Opcode::JUMP));
            }

            Control::Switch(_switch) => {
                // TODO: Implement switch statements
                // This is more complex and needs jumptable generation
            }
        }
    }

    // Helper methods for assembly generation

    /// Push a constant value onto the stack
    fn push_const(&mut self, value: U256) {
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

    /// Load a local from memory onto the stack
    fn load_local(&mut self, local: LocalId) {
        use evm_glue::opcodes::Opcode;

        if let Some(addr) = self.memory.get_local_address(local) {
            // Push memory address
            self.push_const(U256::from(addr));
            // Load from memory
            self.asm.push(Asm::Op(Opcode::MLOAD));
        }
        // TODO: Handle error when local not found
    }

    /// Store a value from stack to a local in memory
    fn store_local(&mut self, local: LocalId) {
        use evm_glue::opcodes::Opcode;

        if let Some(addr) = self.memory.get_local_address(local) {
            // Stack: [value]
            // Push memory address
            self.push_const(U256::from(addr));
            // Stack: [value, addr]
            // Store to memory
            self.asm.push(Asm::Op(Opcode::MSTORE));
            // Stack: []
        }
        // TODO: Handle error when local not found
    }

    /// Emit a mark (jump label) at the current position
    fn emit_mark(&mut self, mark_id: MarkId) {
        use evm_glue::opcodes::Opcode;

        // Emit the mark for evm-glue to track
        self.asm.push(Asm::Mark(mark_id));

        // Emit JUMPDEST opcode - required by EVM at jump targets
        self.asm.push(Asm::Op(Opcode::JUMPDEST));
    }

    /// Emit a jump to a mark
    fn emit_jump(&mut self, mark_id: MarkId) {
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

    /// Emit a conditional jump to a mark
    fn emit_jumpi(&mut self, mark_id: MarkId) {
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
}

/// High-level function to translate an IR program to EVM assembly
pub fn translate_program(program: EthIRProgram) -> Vec<Asm> {
    let mut translator = IrToEvm::new(program);
    translator.translate();
    translator.into_asm()
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy_primitives::U256;
    use eth_ir_data::{Branch, DataId, DataOffset, operation::*, *};

    #[test]
    fn test_simple_add_program() {
        // Create a simple program that adds two numbers and stops
        // Similar to: a = 5; b = 10; c = a + b; STOP

        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(4),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // a = 5
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 5 }),
                // b = 10
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 10 }),
                // c = a + b
                Operation::Add(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // STOP
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0), // a
                LocalId::new(1), // b
                LocalId::new(2), // c
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Print the assembly for debugging
        println!("Generated assembly:");
        for (i, instruction) in asm.iter().enumerate() {
            println!("{:3}: {:?}", i, instruction);
        }

        // Verify key instructions are present
        use evm_glue::opcodes::Opcode;

        // Should have initialization (free memory pointer setup)
        assert!(matches!(asm[0], Asm::Op(Opcode::PUSH2(_))));
        assert!(matches!(asm[1], Asm::Op(Opcode::PUSH1(_))));
        assert!(matches!(asm[2], Asm::Op(Opcode::MSTORE)));

        // Should have marks and JUMPDEST for function and block
        assert!(matches!(asm[3], Asm::Mark(_))); // Function mark
        assert!(matches!(asm[4], Asm::Op(Opcode::JUMPDEST)));
        assert!(matches!(asm[5], Asm::Mark(_))); // Block mark
        assert!(matches!(asm[6], Asm::Op(Opcode::JUMPDEST)));

        // Should set local 0 = 5
        assert!(matches!(asm[7], Asm::Op(Opcode::PUSH1([5]))));
        assert!(matches!(asm[8], Asm::Op(Opcode::PUSH1([128])))); // address 0x80
        assert!(matches!(asm[9], Asm::Op(Opcode::MSTORE)));

        // Should set local 1 = 10
        assert!(matches!(asm[10], Asm::Op(Opcode::PUSH1([10]))));
        assert!(matches!(asm[11], Asm::Op(Opcode::PUSH1([160])))); // address 0xA0
        assert!(matches!(asm[12], Asm::Op(Opcode::MSTORE)));

        // Should load locals, add, and store result
        assert!(matches!(asm[13], Asm::Op(Opcode::PUSH1([128])))); // load local 0
        assert!(matches!(asm[14], Asm::Op(Opcode::MLOAD)));
        assert!(matches!(asm[15], Asm::Op(Opcode::PUSH1([160])))); // load local 1
        assert!(matches!(asm[16], Asm::Op(Opcode::MLOAD)));
        assert!(matches!(asm[17], Asm::Op(Opcode::ADD)));
        assert!(matches!(asm[18], Asm::Op(Opcode::PUSH1([192])))); // store to local 2 at 0xC0
        assert!(matches!(asm[19], Asm::Op(Opcode::MSTORE)));

        // Should end with STOP
        assert!(matches!(asm[20], Asm::Op(Opcode::STOP)));
    }

    #[test]
    fn test_large_constants() {
        // Test with large constants (e.g., addresses, max uint256)
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(3),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // Set local 0 to an Ethereum address
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(0),
                    cid: LargeConstId::new(0),
                }),
                // Set local 1 to max uint256
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(1),
                    cid: LargeConstId::new(1),
                }),
                Operation::Stop,
            ],
            locals: index_vec![LocalId::new(0), LocalId::new(1),],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![
                // Ethereum address (160 bits, but stored as U256)
                U256::from_be_bytes([
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, // 12 leading zeros for 160-bit address
                    0xdE, 0xaD, 0xbE, 0xeF, 0xbA, 0xbE, 0xca, 0xfe, 0x12, 0x34, 0x56, 0x78, 0x90,
                    0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde
                ]),
                // Max uint256
                U256::MAX,
            ],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Print the assembly for debugging
        println!("Generated assembly for large constants:");
        for (i, instruction) in asm.iter().enumerate() {
            println!("{:3}: {:?}", i, instruction);
        }

        // Verify key instructions
        use evm_glue::opcodes::Opcode;

        // Should push the Ethereum address (using PUSH32 for large constant)
        assert!(matches!(asm[7], Asm::Op(Opcode::PUSH32(_))));
        // Check it contains our address bytes
        if let Asm::Op(Opcode::PUSH32(bytes)) = &asm[7] {
            // First 12 bytes should be 0 (padding)
            assert_eq!(&bytes[0..12], &[0u8; 12]);
            // Next bytes should be our address
            assert_eq!(bytes[12], 0xdE);
            assert_eq!(bytes[13], 0xaD);
            assert_eq!(bytes[14], 0xbE);
            assert_eq!(bytes[15], 0xeF);
        }

        // Should push MAX U256
        assert!(matches!(asm[10], Asm::Op(Opcode::PUSH32(_))));
        if let Asm::Op(Opcode::PUSH32(bytes)) = &asm[10] {
            // All bytes should be 0xFF
            assert_eq!(bytes, &[0xFF; 32]);
        }

        // Should end with STOP
        assert!(matches!(asm[13], Asm::Op(Opcode::STOP)));
    }

    #[test]
    fn test_arithmetic_operations() {
        // Test various arithmetic operations
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(9),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // a = 20
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 20 }),
                // b = 5
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 5 }),
                // exp = 3 (for exponent)
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(7), value: 3 }),
                // c = a - b (15)
                Operation::Sub(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // d = a * b (100)
                Operation::Mul(TwoInOneOut {
                    result: LocalId::new(3),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // e = a / b (4)
                Operation::Div(TwoInOneOut {
                    result: LocalId::new(4),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // f = a % b (0)
                Operation::Mod(TwoInOneOut {
                    result: LocalId::new(5),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // g = b ** 3 (125)
                Operation::Exp(TwoInOneOut {
                    result: LocalId::new(6),
                    arg1: LocalId::new(1),
                    arg2: LocalId::new(7), // exponent = 3
                }),
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0), // a = 20
                LocalId::new(1), // b = 5
                LocalId::new(2), // c = a - b
                LocalId::new(3), // d = a * b
                LocalId::new(4), // e = a / b
                LocalId::new(5), // f = a % b
                LocalId::new(6), // g = b ** 3
                LocalId::new(7), // constant 3 for exp
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Print the assembly for debugging
        println!("Generated assembly for arithmetic operations:");
        for (i, instruction) in asm.iter().enumerate() {
            println!("{:3}: {:?}", i, instruction);
        }

        // Verify key instructions
        use evm_glue::opcodes::Opcode;

        // Should have all arithmetic operations
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SUB))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MUL))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::DIV))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MOD))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::EXP))));

        // Should end with STOP
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::STOP))));
    }

    #[test]
    fn test_bitwise_operations() {
        // Test bitwise operations
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(11),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // a = 0b1010 (10)
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: 0b1010
                }),
                // b = 0b1100 (12)
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value: 0b1100
                }),
                // Set shift amounts
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(7), value: 2 }), /* shift amount 2 */
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(9), value: 1 }), /* shift amount 1 */
                // c = a & b (0b1000 = 8)
                Operation::And(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // d = a | b (0b1110 = 14)
                Operation::Or(TwoInOneOut {
                    result: LocalId::new(3),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // e = a ^ b (0b0110 = 6)
                Operation::Xor(TwoInOneOut {
                    result: LocalId::new(4),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // f = ~a (bitwise NOT)
                Operation::Not(OneInOneOut { result: LocalId::new(5), arg1: LocalId::new(0) }),
                // g = a << 2 (shift left by 2: 0b101000 = 40)
                Operation::Shl(TwoInOneOut {
                    result: LocalId::new(6),
                    arg1: LocalId::new(7), // shift amount (2)
                    arg2: LocalId::new(0), // value to shift
                }),
                // h = b >> 1 (shift right by 1: 0b0110 = 6)
                Operation::Shr(TwoInOneOut {
                    result: LocalId::new(8),
                    arg1: LocalId::new(9), // shift amount (1)
                    arg2: LocalId::new(1), // value to shift
                }),
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0), // a = 10
                LocalId::new(1), // b = 12
                LocalId::new(2), // c = a & b
                LocalId::new(3), // d = a | b
                LocalId::new(4), // e = a ^ b
                LocalId::new(5), // f = ~a
                LocalId::new(6), // g = a << 2
                LocalId::new(7), // shift amount 2
                LocalId::new(8), // h = b >> 1
                LocalId::new(9), // shift amount 1
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Print the assembly for debugging
        println!("Generated assembly for bitwise operations:");
        for (i, instruction) in asm.iter().enumerate() {
            println!("{:3}: {:?}", i, instruction);
        }

        // Verify key instructions
        use evm_glue::opcodes::Opcode;

        // Should have all bitwise operations
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::AND))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::OR))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::XOR))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::NOT))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SHL))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SHR))));

        // Should end with STOP
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::STOP))));
    }

    #[test]
    fn test_comparison_operations() {
        // Test comparison operations: a = 10, b = 5, c = (a > b), d = (a == b), e = IsZero(d)
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(6),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // a = 10
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 10 }),
                // b = 5
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 5 }),
                // c = (a > b) - should be 1 (true)
                Operation::Gt(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // d = (a == b) - should be 0 (false)
                Operation::Eq(TwoInOneOut {
                    result: LocalId::new(3),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1),
                }),
                // e = IsZero(d) - should be 1 (true, since d is 0)
                Operation::IsZero(OneInOneOut { result: LocalId::new(4), arg1: LocalId::new(3) }),
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0), // a
                LocalId::new(1), // b
                LocalId::new(2), // c (a > b)
                LocalId::new(3), // d (a == b)
                LocalId::new(4), // e (IsZero(d))
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Print the assembly for debugging
        println!("Generated assembly for comparison operations:");
        for (i, instruction) in asm.iter().enumerate() {
            println!("{:3}: {:?}", i, instruction);
        }

        // Verify key instructions
        use evm_glue::opcodes::Opcode;

        // After initialization and marks, verify the comparison operations
        // Find where operations start (after init, function mark, block mark)
        let ops_start = 7; // After init (3 ops) + func mark (2) + block mark (2)

        // Should have GT operation
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::GT))));

        // Should have EQ operation
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::EQ))));

        // Should have ISZERO operation
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::ISZERO))));

        // Should end with STOP
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::STOP))));
    }

    #[test]
    fn test_external_call_operations() {
        // Test external call operations (simplified - just checking opcodes are generated)
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(3),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // Set up minimal args for a STATICCALL (6 args needed)
                // In real usage, these would be actual memory addresses and sizes
                // Args are stored contiguously starting at LocalIndex 0
                Operation::StaticCall(LargeInOneOut::<6> {
                    args_start: LocalIndex::from_usize(0),
                    result: LocalId::new(10), // Store success result
                }),
                // RETURN with offset and size
                Operation::Return(TwoInZeroOut {
                    arg1: LocalId::new(11), // offset
                    arg2: LocalId::new(12), // size
                }),
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0),  // gas
                LocalId::new(1),  // address
                LocalId::new(2),  // argsOffset
                LocalId::new(3),  // argsSize
                LocalId::new(4),  // retOffset
                LocalId::new(5),  // retSize
                LocalId::new(10), // result
                LocalId::new(11), // return offset
                LocalId::new(12), // return size
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Print the assembly for debugging
        println!("Generated assembly for external call operations:");
        for (i, instruction) in asm.iter().enumerate() {
            println!("{:3}: {:?}", i, instruction);
        }

        // Verify key instructions
        use evm_glue::opcodes::Opcode;

        // Should have STATICCALL operation
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::STATICCALL))));

        // Should have RETURN operation
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::RETURN))));

        // Should end with STOP
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::STOP))));
    }

    #[test]
    fn test_internal_call() {
        // Test internal function calls
        // Note: This is a simplified test - real internal calls need proper argument passing
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![
                Function { entry: BasicBlockId::new(0), outputs: 0 }, // main function
                Function { entry: BasicBlockId::new(1), outputs: 1 }, // called function
            ],
            basic_blocks: index_vec![
                // Block 0: main function that calls function 1
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(2),
                    control: Control::LastOpTerminates,
                },
                // Block 1: called function
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    operations: OperationIndex::from_usize(2)..OperationIndex::from_usize(3),
                    control: Control::InternalReturn,
                },
            ],
            operations: index_vec![
                // Main function: call internal function
                Operation::InternalCall(InternalCall {
                    function: FunctionId::new(1),
                    args_start: LocalIndex::from_usize(0),
                    outputs_start: LocalIndex::from_usize(1),
                }),
                Operation::Stop,
                // Called function: set a value and return
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(5), value: 42 }),
            ],
            locals: index_vec![
                LocalId::new(0), // argument
                LocalId::new(1), // output
                LocalId::new(5), // return value
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Print the assembly for debugging
        println!("Generated assembly for internal call:");
        for (i, instruction) in asm.iter().enumerate() {
            println!("{:3}: {:?}", i, instruction);
        }

        // Verify we have marks for both functions
        assert!(asm.iter().any(|op| matches!(op, Asm::Mark(0)))); // init function
        assert!(asm.iter().any(|op| matches!(op, Asm::Mark(1)))); // called function

        // Should have at least one JUMP operation for the call
        use evm_glue::opcodes::Opcode;
        assert!(asm.iter().filter(|op| matches!(op, Asm::Op(Opcode::JUMP))).count() >= 1);

        // Should have mark references for the call
        assert!(asm.iter().any(|op| matches!(op, Asm::Ref(_))));
    }

    #[test]
    fn test_environmental_operations() {
        // Test environmental info operations
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(5),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // Get contract address
                Operation::Address(ZeroInOneOut { result: LocalId::new(0) }),
                // Get caller
                Operation::Caller(ZeroInOneOut { result: LocalId::new(1) }),
                // Get call value
                Operation::CallValue(ZeroInOneOut { result: LocalId::new(2) }),
                // Get balance of caller
                Operation::Balance(OneInOneOut { result: LocalId::new(3), arg1: LocalId::new(1) }),
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0), // contract address
                LocalId::new(1), // caller
                LocalId::new(2), // call value
                LocalId::new(3), // balance
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Verify key instructions
        use evm_glue::opcodes::Opcode;
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::ADDRESS))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CALLER))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CALLVALUE))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::BALANCE))));
    }

    #[test]
    fn test_storage_operations() {
        // Test storage operations
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(5),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // Set storage key
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 42 }),
                // Set value
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(1), value: 100 }),
                // Store to storage
                Operation::SStore(TwoInZeroOut { arg1: LocalId::new(0), arg2: LocalId::new(1) }),
                // Load from storage
                Operation::SLoad(OneInOneOut { result: LocalId::new(2), arg1: LocalId::new(0) }),
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0), // storage key
                LocalId::new(1), // value to store
                LocalId::new(2), // loaded value
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Verify key instructions
        use evm_glue::opcodes::Opcode;
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SLOAD))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::SSTORE))));
    }

    #[test]
    fn test_data_section_operations() {
        // Test data section and bytecode introspection operations
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: Some(FunctionId::new(1)),
            functions: index_vec![
                Function { entry: BasicBlockId::new(0), outputs: 0 }, // init function
                Function { entry: BasicBlockId::new(1), outputs: 0 }, // main function
            ],
            basic_blocks: index_vec![
                // Init block
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(3),
                    control: Control::LastOpTerminates,
                },
                // Main block
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    operations: OperationIndex::from_usize(3)..OperationIndex::from_usize(7),
                    control: Control::LastOpTerminates,
                },
            ],
            operations: index_vec![
                // Init operations
                Operation::RuntimeStartOffset(ZeroInOneOut { result: LocalId::new(0) }),
                Operation::RuntimeLength(ZeroInOneOut { result: LocalId::new(1) }),
                Operation::Stop,
                // Main operations
                Operation::LocalSetDataOffset(SetDataOffset {
                    local: LocalId::new(2),
                    segment_id: DataId::new(0),
                }),
                Operation::LocalSetDataOffset(SetDataOffset {
                    local: LocalId::new(3),
                    segment_id: DataId::new(1),
                }),
                Operation::InitEndOffset(ZeroInOneOut { result: LocalId::new(4) }),
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0), // runtime start offset
                LocalId::new(1), // runtime length
                LocalId::new(2), // data segment 0 offset
                LocalId::new(3), // data segment 1 offset
                LocalId::new(4), // init end offset
            ],
            data_segments_start: index_vec![
                DataOffset::new(0), // Segment 0 starts at 0
                DataOffset::new(4), // Segment 1 starts at 4
            ],
            data_bytes: index_vec![0xde, 0xad, 0xbe, 0xef, 0xca, 0xfe, 0xba, 0xbe],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Print the assembly for debugging
        println!("Generated assembly for data section operations:");
        for (i, instruction) in asm.iter().enumerate() {
            println!("{:3}: {:?}", i, instruction);
        }

        // Verify we have marks for init end and runtime end
        assert!(asm.iter().any(|op| matches!(op, Asm::Mark(0)))); // init_end_mark
        assert!(asm.iter().any(|op| matches!(op, Asm::Mark(1)))); // runtime_end_mark

        // Verify we have data embedded
        assert!(asm.iter().any(|op| matches!(op, Asm::Data(_))));

        // Verify we have references to marks
        assert!(asm.iter().any(|op| matches!(op, Asm::Ref(_))));

        // Verify deployment return (CODECOPY and RETURN)
        use evm_glue::opcodes::Opcode;
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::CODECOPY))));
        assert!(asm.iter().any(|op| matches!(op, Asm::Op(Opcode::RETURN))));
    }

    #[test]
    fn test_acquire_free_pointer() {
        // Test AcquireFreePointer operation
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(2),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::AcquireFreePointer(ZeroInOneOut { result: LocalId::new(0) }),
                Operation::Stop,
            ],
            locals: index_vec![LocalId::new(0)], // free pointer result
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we load from 0x40
        use evm_glue::opcodes::Opcode;

        // Should have PUSH 0x40, MLOAD sequence
        let mut found_free_ptr_load = false;
        for i in 0..asm.len() - 1 {
            if let (Asm::Op(Opcode::PUSH1([0x40])), Asm::Op(Opcode::MLOAD)) = (&asm[i], &asm[i + 1])
            {
                found_free_ptr_load = true;
                break;
            }
        }
        assert!(found_free_ptr_load, "Should load free memory pointer from 0x40");
    }

    #[test]
    fn test_memory_allocation() {
        // Test memory allocation operations
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(4),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // Set size to 64 bytes
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 64 }),
                // Allocate 64 bytes
                Operation::DynamicAllocAnyBytes(OneInOneOut {
                    arg1: LocalId::new(0),
                    result: LocalId::new(1),
                }),
                // Store something to the allocated memory
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(2),
                    value: 0xdeadbeef,
                }),
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0), // size
                LocalId::new(1), // allocated pointer
                LocalId::new(2), // value to store
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we manipulate the free memory pointer
        use evm_glue::opcodes::Opcode;

        // Should load and update free memory pointer
        let has_free_ptr_ops = asm.iter().any(|op| matches!(op, Asm::Op(Opcode::PUSH1([0x40]))));
        assert!(has_free_ptr_ops, "Should use free memory pointer at 0x40");

        // Should have MLOAD and MSTORE for free pointer manipulation
        let has_mload = asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MLOAD)));
        let has_mstore = asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MSTORE)));
        assert!(has_mload && has_mstore, "Should load and store free memory pointer");
    }

    #[test]
    fn test_memory_operations() {
        // Test MemoryLoad and MemoryStore
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(4),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                // Store a value to memory
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(0),
                    value: 0x100, // address
                }),
                Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::new(1),
                    value: 0xdeadbeef, // value
                }),
                Operation::MemoryStore(MemoryStore {
                    address: LocalId::new(0),
                    value: LocalId::new(1),
                    byte_size: 32, // full word
                }),
                Operation::Stop,
            ],
            locals: index_vec![
                LocalId::new(0), // address
                LocalId::new(1), // value
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Should have MSTORE for the memory store operation
        use evm_glue::opcodes::Opcode;
        let has_mstore = asm.iter().any(|op| matches!(op, Asm::Op(Opcode::MSTORE)));
        assert!(has_mstore, "Should have MSTORE for memory store operation");
    }

    #[test]
    fn test_branching_program() {
        // Create a program with branching
        // if (a == 0) goto block1 else goto block2

        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 0 }],
            basic_blocks: index_vec![
                // Entry block: set a = 5, branch on a
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(1),
                    control: Control::Branches(Branch {
                        condition: LocalId::new(0),
                        zero_target: BasicBlockId::new(1),
                        non_zero_target: BasicBlockId::new(2),
                    }),
                },
                // Block 1: zero branch
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    operations: OperationIndex::from_usize(1)..OperationIndex::from_usize(2),
                    control: Control::LastOpTerminates,
                },
                // Block 2: non-zero branch
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    operations: OperationIndex::from_usize(2)..OperationIndex::from_usize(3),
                    control: Control::LastOpTerminates,
                },
            ],
            operations: index_vec![
                // Set a = 5
                Operation::LocalSetSmallConst(SetSmallConst { local: LocalId::new(0), value: 5 }),
                // STOP (for zero branch)
                Operation::Stop,
                // INVALID (for non-zero branch)
                Operation::Invalid,
            ],
            locals: index_vec![
                LocalId::new(0), // a
            ],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let asm = translate_program(program);

        // Check that we generated some assembly
        assert!(!asm.is_empty());

        // Print the assembly for debugging
        println!("Generated assembly for branching program:");
        for (i, instruction) in asm.iter().enumerate() {
            println!("{:3}: {:?}", i, instruction);
        }
    }
}
