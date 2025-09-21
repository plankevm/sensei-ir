//! Control flow translation

use super::Translator;
use crate::error::{CodegenError, Result, runtime};
use eth_ir_data::{BasicBlockId, Control, Idx};
use evm_glue::assembly::Asm;
use std::collections::VecDeque;

impl Translator {
    /// Translate a basic block and all reachable blocks iteratively
    pub(super) fn translate_block(&mut self, initial_block_id: BasicBlockId) -> Result<()> {
        // Use a work queue to avoid stack overflow from deep recursion
        let mut work_queue = VecDeque::new();
        work_queue.push_back(initial_block_id);

        while let Some(block_id) = work_queue.pop_front() {
            // Check if we've already translated this block
            if !self.state.translated_blocks.insert(block_id) {
                // Already translated, skip
                continue;
            }

            // Translate this block
            self.translate_single_block(block_id, &mut work_queue)?;
        }

        Ok(())
    }

    /// Translate a single basic block (without recursion)
    fn translate_single_block(
        &mut self,
        block_id: BasicBlockId,
        work_queue: &mut VecDeque<BasicBlockId>,
    ) -> Result<()> {
        // Check if block exists
        if block_id.index() >= self.program.program.basic_blocks.len() {
            return Err(CodegenError::InvalidBlockReference { block: block_id });
        }

        // Extract block data to avoid borrowing issues
        let block = &self.program.program.basic_blocks[block_id];
        let operations_range = block.operations.clone();
        let control = block.control.clone();

        // Emit the block's mark (label)
        let block_mark = self.state.marks.get_block_mark(block_id);
        self.emit_mark(block_mark);

        // Handle block inputs
        // The IR uses explicit inputs/outputs for value flow analysis, but our
        // code generation strategy keeps all locals accessible throughout the function.
        //
        // We assume the IR follows single-assignment semantics (each local is assigned
        // at most once), which should be guaranteed by the IR generator. This means:
        // - Locals retain their values across blocks once assigned
        // - We don't need to explicitly pass unchanged values between blocks
        // - The memory/stack location for each local remains valid throughout the function
        //
        // Note: We do NOT enforce single-assignment - we trust the IR is well-formed.

        // Translate all operations in the block
        let ops_start = operations_range.start.index();
        let ops_end = operations_range.end.index();
        for i in ops_start..ops_end {
            let op_idx = eth_ir_data::OperationIndex::from_usize(i);
            // Use a match to avoid cloning the entire operation
            self.translate_operation_by_index(op_idx)?;
        }

        // Handle block outputs
        // Since we maintain locals throughout the function, outputs are already
        // available to successor blocks through the locals storage.
        // The explicit outputs in the IR are for static analysis and verification,
        // not for runtime value passing.

        // Translate control flow
        self.translate_control(&control)?;

        // Add reachable blocks to work queue (instead of recursing)
        match &control {
            Control::ContinuesTo(next) => {
                work_queue.push_back(*next);
            }
            Control::Branches(branch) => {
                work_queue.push_back(branch.zero_target);
                work_queue.push_back(branch.non_zero_target);
            }
            Control::Switch(switch) => {
                // Check if cases array exists
                if switch.cases.index() >= self.program.program.cases.len() {
                    return Err(CodegenError::InvalidCasesReference { cases: switch.cases });
                }
                // Add all case targets to work queue
                let cases = &self.program.program.cases[switch.cases].cases;
                for case in cases {
                    work_queue.push_back(case.target);
                }
                if let Some(fallback) = switch.fallback {
                    work_queue.push_back(fallback);
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
                let next_mark = self.state.marks.get_block_mark(*next_block);
                self.emit_jump(next_mark);
            }

            Control::Branches(branch) => {
                // Conditional branch
                self.load_local(branch.condition)?;

                // EVM's JUMPI jumps if condition is non-zero
                let non_zero_mark = self.state.marks.get_block_mark(branch.non_zero_target);
                self.emit_jumpi(non_zero_mark);

                // If we didn't jump, continue to zero target
                let zero_mark = self.state.marks.get_block_mark(branch.zero_target);
                self.emit_jump(zero_mark);
            }

            Control::InternalReturn => {
                // Return from an internal function call
                // The return address is on the stack, jump to it
                use evm_glue::opcodes::Opcode;
                self.state.asm.push(Asm::Op(Opcode::JUMP));
            }

            Control::Switch(switch) => {
                use evm_glue::opcodes::Opcode;

                // Check if cases array exists
                if switch.cases.index() >= self.program.program.cases.len() {
                    return Err(CodegenError::InvalidCasesReference { cases: switch.cases });
                }

                // Clone cases to avoid borrow issues
                let cases = self.program.program.cases[switch.cases].cases.clone();

                // For large switch statements, we need to avoid excessive stack depth
                // EVM has a max stack depth of 1024, and DUP operations are limited to DUP16
                // We'll handle this by reloading the condition value when needed
                const MAX_CASES_PER_BATCH: usize = 14; // Leave room for other stack operations

                // Load the condition value initially
                self.load_local(switch.condition)?;

                // Process cases in batches to avoid stack overflow
                for (batch_idx, case_batch) in cases.chunks(MAX_CASES_PER_BATCH).enumerate() {
                    // For batches after the first, reload the condition
                    if batch_idx > 0 {
                        // Pop the old condition value
                        self.state.asm.push(Asm::Op(Opcode::POP));
                        // Load fresh condition value
                        self.load_local(switch.condition)?;
                    }

                    // Process cases in this batch
                    for case in case_batch {
                        let case_value = case.value;
                        let case_target = case.target;

                        // Stack: [condition]
                        self.state.asm.push(Asm::Op(Opcode::DUP1)); // Duplicate condition
                        // Stack: [condition, condition]

                        self.push_const(case_value); // Push case value
                        // Stack: [condition, condition, case_value]

                        self.state.asm.push(Asm::Op(Opcode::EQ)); // Compare
                        // Stack: [condition, is_equal]

                        // If equal, jump to case target
                        let case_mark = self.state.marks.get_block_mark(case_target);
                        self.emit_jumpi(case_mark);
                        // Stack: [condition] (after jump or continue)
                    }
                }

                // Pop the final condition value (no longer needed)
                self.state.asm.push(Asm::Op(Opcode::POP));
                // Stack: []

                // If no case matched, jump to fallback or error
                if let Some(fallback) = switch.fallback {
                    let fallback_mark = self.state.marks.get_block_mark(fallback);
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
