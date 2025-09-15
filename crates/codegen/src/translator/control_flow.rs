//! Control flow translation

use super::Translator;
use crate::error::{CodegenError, Result, runtime};
use alloy_primitives::U256;
use eth_ir_data::{BasicBlockId, Control, Idx};
use evm_glue::assembly::Asm;
use smallvec::SmallVec;

impl Translator {
    /// Translate a basic block
    pub(super) fn translate_block(&mut self, block_id: BasicBlockId) -> Result<()> {
        // Check if we've already translated this block
        if !self.translated_blocks.insert(block_id) {
            // Already translated, nothing to do
            return Ok(());
        }

        // Check if block exists
        if block_id.index() >= self.program.basic_blocks.len() {
            return Err(CodegenError::InvalidBlockReference { block: block_id });
        }

        // Extract block data to avoid borrowing issues
        let block = &self.program.basic_blocks[block_id];
        let operations_range = block.operations.clone();
        let control = block.control.clone();

        // Emit the block's mark (label)
        let block_mark = self.marks.get_block_mark(block_id);
        self.emit_mark(block_mark);

        // Handle block inputs
        // In our memory-backed approach, inputs are already in memory
        // from the caller, so we don't need to do anything special

        // Translate all operations in the block
        // Clone operations to avoid borrowing conflicts
        // Use SmallVec since most blocks have < 32 operations
        let ops_to_translate: SmallVec<[_; 32]> =
            self.program.operations[operations_range].iter().cloned().collect();
        for op in ops_to_translate {
            self.translate_operation(&op)?;
        }

        // Handle block outputs
        // In our memory-backed approach, outputs are already in memory
        // after operations execute, so nothing special needed

        // Translate control flow
        self.translate_control(&control)?;

        // Recursively translate reachable blocks
        match &control {
            Control::ContinuesTo(next) => {
                self.translate_block(*next)?;
            }
            Control::Branches(branch) => {
                self.translate_block(branch.zero_target)?;
                self.translate_block(branch.non_zero_target)?;
            }
            Control::Switch(switch) => {
                // Check if cases array exists
                if switch.cases.index() >= self.program.cases.len() {
                    return Err(CodegenError::InvalidCasesReference { cases: switch.cases });
                }
                // Translate all case targets
                // Pre-allocate with exact size since we know the count
                let cases = &self.program.cases[switch.cases].cases;
                let mut case_targets = Vec::with_capacity(cases.len());
                for case in cases {
                    case_targets.push(case.target);
                }
                for target in case_targets {
                    self.translate_block(target)?;
                }
                if let Some(fallback) = switch.fallback {
                    self.translate_block(fallback)?;
                }
            }
            Control::LastOpTerminates | Control::InternalReturn => {
                // These don't continue to other blocks
            }
        }

        Ok(())
    }

    /// Translate control flow at the end of a basic block
    ///
    /// Handles the various ways a basic block can transfer control:
    /// * `LastOpTerminates` - The last operation (STOP, RETURN, etc.) handles termination
    /// * `ContinuesTo` - Unconditional jump to another block
    /// * `Branches` - Conditional jump based on a boolean value
    /// * `InternalReturn` - Return from an internal function call
    /// * `Switch` - Multi-way branch based on comparing a value to multiple cases
    ///
    /// # Arguments
    /// * `control` - The control flow instruction to translate
    ///
    /// # Switch Behavior
    /// For switches without a fallback, emits a runtime error with code SWITCH_NO_MATCH
    /// if no case matches the condition value.
    pub(super) fn translate_control(&mut self, control: &Control) -> Result<()> {
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
                self.load_local(branch.condition)?;

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
                self.push_const(U256::from(super::memory::constants::ZERO_SLOT));
                use evm_glue::opcodes::Opcode;
                self.asm.push(Asm::Op(Opcode::MLOAD));
                self.asm.push(Asm::Op(Opcode::JUMP));
            }

            Control::Switch(switch) => {
                use evm_glue::opcodes::Opcode;

                // Check if cases array exists
                if switch.cases.index() >= self.program.cases.len() {
                    return Err(CodegenError::InvalidCasesReference { cases: switch.cases });
                }

                // Load the condition value
                self.load_local(switch.condition)?;

                // Get number of cases
                let num_cases = self.program.cases[switch.cases].cases.len();

                // For each case: duplicate condition, push case value, compare, and jump if equal
                for i in 0..num_cases {
                    let case = &self.program.cases[switch.cases].cases[i];
                    let case_value = case.value;
                    let case_target = case.target;

                    // Stack: [condition]
                    self.asm.push(Asm::Op(Opcode::DUP1)); // Duplicate condition
                    // Stack: [condition, condition]

                    self.push_const(case_value); // Push case value
                    // Stack: [condition, condition, case_value]

                    self.asm.push(Asm::Op(Opcode::EQ)); // Compare
                    // Stack: [condition, is_equal]

                    // If equal, jump to case target
                    let case_mark = self.marks.get_block_mark(case_target);
                    self.emit_jumpi(case_mark);
                    // Stack: [condition] (after jump or continue)
                }

                // Pop the condition value (no longer needed)
                self.asm.push(Asm::Op(Opcode::POP));
                // Stack: []

                // If no case matched, jump to fallback or error
                if let Some(fallback) = switch.fallback {
                    let fallback_mark = self.marks.get_block_mark(fallback);
                    self.emit_jump(fallback_mark);
                } else {
                    // Switch without fallback - emit error code and revert
                    self.emit_runtime_error(runtime::SWITCH_NO_MATCH);
                }
            }
        }

        Ok(())
    }
}
