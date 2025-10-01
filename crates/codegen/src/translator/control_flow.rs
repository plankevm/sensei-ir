//! Control flow translation

use super::Translator;
use crate::error::{Result, runtime};
use eth_ir_data::{BasicBlockId, Control, Idx};
use evm_glue::assembly::Asm;

impl Translator {
    /// Translate a basic block and all reachable blocks iteratively
    ///
    /// Uses depth-first traversal with a work stack to avoid recursion.
    pub(super) fn translate_block(&mut self, initial_block_id: BasicBlockId) -> Result<()> {
        // Use a work stack to avoid stack overflow from deep recursion
        let mut work_queue = Vec::new();
        work_queue.push(initial_block_id);

        while let Some(block_id) = work_queue.pop() {
            // Check if we've already translated this block
            let idx = block_id.index();
            if self.state.translated_blocks[idx] {
                // Already translated, skip
                continue;
            }
            self.state.translated_blocks[idx] = true;

            // Translate this block
            self.translate_single_block(block_id, &mut work_queue)?;
        }

        Ok(())
    }

    /// Translate a single basic block (without recursion)
    fn translate_single_block(
        &mut self,
        block_id: BasicBlockId,
        work_queue: &mut Vec<BasicBlockId>,
    ) -> Result<()> {
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

        // Extract just what we need from the block to avoid borrow issues
        // Both Range and Control are small and cheap to clone
        let (operations_range, control) = {
            let block = &self.program.program.basic_blocks[block_id];
            (block.operations.clone(), block.control.clone())
        };

        // Translate all operations in the block
        let ops_start = operations_range.start.index();
        let ops_end = operations_range.end.index();
        for i in ops_start..ops_end {
            let op_idx = eth_ir_data::OperationIndex::from_usize(i);
            self.translate_operation_by_index(op_idx)?;
        }

        // Handle block outputs
        // Since we maintain locals throughout the function, outputs are already
        // available to successor blocks through the locals storage.
        // The explicit outputs in the IR are for static analysis and verification,
        // not for runtime value passing.

        // Translate control flow
        self.translate_control(&control)?;

        // Add reachable blocks to work stack (instead of recursing)
        match &control {
            Control::ContinuesTo(next) => {
                work_queue.push(*next);
            }
            Control::Branches(branch) => {
                work_queue.push(branch.zero_target);
                work_queue.push(branch.non_zero_target);
            }
            Control::Switch(switch) => {
                // Add all case targets to work stack
                let cases = &self.program.program.cases[switch.cases].cases;
                for case in cases {
                    work_queue.push(case.target);
                }
                if let Some(fallback) = switch.fallback {
                    work_queue.push(fallback);
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
                self.translate_switch(switch)?;
            }
        }

        Ok(())
    }

    /// Translate a switch statement with case batching to avoid stack depth issues
    ///
    /// ## EVM Stack Constraints
    ///
    /// The EVM has several constraints that affect how we compile switch statements:
    /// - **Stack limit**: 1024 items maximum
    /// - **DUP limit**: DUP1-DUP16 only (can only duplicate top 16 items)
    /// - **SWAP limit**: SWAP1-SWAP16 only (can only swap with top 16 items)
    ///
    /// ## Batching Strategy
    ///
    /// For large switches (>14 cases), we batch them to avoid hitting the DUP16 limit.
    /// Each batch:
    /// 1. Loads the condition value onto the stack
    /// 2. Compares against each case in the batch (using DUP1, PUSH, EQ, JUMPI)
    /// 3. If no match, pops the condition and loads it again for the next batch
    ///
    /// This keeps stack depth manageable while still allowing arbitrarily large switches.
    ///
    /// ## Stack Behavior
    ///
    /// ```text
    /// Initial:  []
    /// After:    [] (jumps to matched case or fallback)
    /// ```
    fn translate_switch(&mut self, switch: &eth_ir_data::Switch) -> Result<()> {
        use evm_glue::opcodes::Opcode;

        // Load the condition value initially
        self.load_local(switch.condition)?;

        // Process cases in batches to respect EVM stack constraints
        // We pass the CasesId and look it up in each iteration to avoid cloning
        self.emit_switch_case_batches(switch.cases, switch.condition)?;

        // Pop the final condition value (no longer needed)
        self.state.asm.push(Asm::Op(Opcode::POP));

        // Handle fallback or error
        self.emit_switch_fallback(switch.fallback)?;

        Ok(())
    }

    /// Emit batched case comparisons for a switch statement
    ///
    /// Processes cases in batches of MAX_SWITCH_CASES_PER_BATCH to avoid exceeding
    /// the EVM's DUP16 limitation. Reloads the condition between batches.
    fn emit_switch_case_batches(
        &mut self,
        cases_id: eth_ir_data::CasesId,
        condition: eth_ir_data::LocalId,
    ) -> Result<()> {
        use super::constants::MAX_SWITCH_CASES_PER_BATCH;
        use evm_glue::opcodes::Opcode;

        let cases = &self.program.program.cases[cases_id].cases;
        let case_count = cases.len();

        for batch_idx in 0..case_count.div_ceil(MAX_SWITCH_CASES_PER_BATCH) {
            // For batches after the first, reload the condition
            if batch_idx > 0 {
                self.state.asm.push(Asm::Op(Opcode::POP)); // Pop old condition
                self.load_local(condition)?; // Load fresh condition
            }

            // Process each case in this batch
            let start_idx = batch_idx * MAX_SWITCH_CASES_PER_BATCH;
            let end_idx = ((batch_idx + 1) * MAX_SWITCH_CASES_PER_BATCH).min(case_count);

            for case_idx in start_idx..end_idx {
                // Extract just the fields we need to avoid borrowing the whole program
                let (case_value, case_target) = {
                    let case = &self.program.program.cases[cases_id].cases[case_idx];
                    (case.value, case.target)
                };
                self.emit_switch_case_comparison_inline(case_value, case_target)?;
            }
        }

        Ok(())
    }

    /// Emit a single case comparison in a switch statement (inline version)
    ///
    /// Stack transition: [condition] → [condition]
    /// Side effect: JUMPI to case target if condition == case value
    fn emit_switch_case_comparison_inline(
        &mut self,
        case_value: alloy_primitives::U256,
        case_target: eth_ir_data::BasicBlockId,
    ) -> Result<()> {
        use evm_glue::opcodes::Opcode;

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
        // Stack: [condition] (JUMPI consumed is_equal)

        Ok(())
    }

    /// Emit the fallback behavior for a switch statement
    ///
    /// Either jumps to the fallback block or emits a runtime error if exhaustive.
    fn emit_switch_fallback(&mut self, fallback: Option<eth_ir_data::BasicBlockId>) -> Result<()> {
        if let Some(fallback_block) = fallback {
            let fallback_mark = self.state.marks.get_block_mark(fallback_block);
            self.emit_jump(fallback_mark);
        } else {
            // Exhaustive switch without fallback - emit error if no case matched
            self.emit_runtime_error(runtime::SWITCH_NO_MATCH);
        }
        Ok(())
    }
}
