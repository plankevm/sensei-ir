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
use eth_ir_data::{BasicBlockId, Control, EthIRProgram, LocalId};
use evm_glue::assembly::Asm;
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
}

impl IrToEvm {
    /// Create a new translator for the given IR program
    pub fn new(program: EthIRProgram) -> Self {
        Self {
            program,
            memory: MemoryLayout::new(),
            marks: MarkAllocator::new(),
            translated_blocks: HashSet::new(),
            asm: Vec::new(),
        }
    }

    /// Translate the IR program to EVM assembly
    pub fn translate(&mut self) {
        // Pre-allocate memory for all locals
        self.allocate_all_locals();

        // Initialize EVM state
        self.emit_initialization();

        // Start translation from the init_entry function
        let init_entry_block = self.program.functions[self.program.init_entry].entry;

        // Emit a mark for the init function
        let init_mark = self.marks.get_function_mark(self.program.init_entry);
        self.emit_mark(init_mark);

        // Translate the entry block of the init function
        self.translate_block(init_entry_block);

        // If there's a main entry, translate it too
        if let Some(main_entry) = self.program.main_entry {
            let main_entry_block = self.program.functions[main_entry].entry;

            // Emit a mark for the main function
            let main_mark = self.marks.get_function_mark(main_entry);
            self.emit_mark(main_mark);

            // Translate the entry block of the main function
            self.translate_block(main_entry_block);
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
            Control::LastOpTerminates | Control::InternalReturn(_) => {
                // These don't continue to other blocks
            }
        }
    }

    /// Translate a single operation
    fn translate_operation(&mut self, op: &eth_ir_data::Operation) {
        use eth_ir_data::Operation;
        use evm_glue::opcodes::Opcode;

        match op {
            // Simple arithmetic operations
            Operation::Add(two_in_one) => {
                self.load_local(two_in_one.arg1);
                self.load_local(two_in_one.arg2);
                self.asm.push(Asm::Op(Opcode::ADD));
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

            // Terminal operations
            Operation::Stop => {
                self.asm.push(Asm::Op(Opcode::STOP));
            }

            Operation::Invalid => {
                self.asm.push(Asm::Op(Opcode::INVALID));
            }

            // TODO: Implement other operations
            _ => {
                // For now, just add a comment
                // In real implementation, this should handle all operations
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

            Control::InternalReturn(value) => {
                // For now, just load the return value
                // Actual implementation would need to handle function returns
                self.load_local(*value);
                // TODO: Implement proper return handling
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
        use evm_glue::{
            assembly::{MarkRef, RefType},
            opcodes::Opcode,
        };

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
        use evm_glue::{
            assembly::{MarkRef, RefType},
            opcodes::Opcode,
        };

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
    use eth_ir_data::{operation::*, *};

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
        assert!(matches!(asm[3], Asm::Mark(0)));
        assert!(matches!(asm[4], Asm::Op(Opcode::JUMPDEST)));
        assert!(matches!(asm[5], Asm::Mark(1)));
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
