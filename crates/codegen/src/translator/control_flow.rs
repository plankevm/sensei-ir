//! Control flow translation

use super::Translator;

use eth_ir_data::{BasicBlockId, Control, Idx, RangeExt};
use evm_glue::assembly::Asm;

impl Translator {
    fn needs_locals_copy(
        &self,
        outputs_range: &std::ops::Range<eth_ir_data::LocalIndex>,
        target_block: BasicBlockId,
    ) -> bool {
        let target_inputs_range = &self.program.basic_blocks[target_block].inputs;

        if outputs_range.is_empty() || target_inputs_range.is_empty() {
            return false;
        }

        let output_locals = &self.program.locals[outputs_range.clone()];
        let input_locals = &self.program.locals[target_inputs_range.clone()];

        output_locals != input_locals
    }

    fn emit_locals_copy(
        &mut self,
        outputs_range: &std::ops::Range<eth_ir_data::LocalIndex>,
        target_block: BasicBlockId,
    ) {
        let target_inputs_range = self.program.basic_blocks[target_block].inputs.clone();

        let copy_pairs: Vec<_> = {
            let output_locals = &self.program.locals[outputs_range.clone()];
            let input_locals = &self.program.locals[target_inputs_range];

            output_locals
                .iter()
                .zip(input_locals.iter())
                .filter(|(out, inp)| out != inp)
                .map(|(out, inp)| (*out, *inp))
                .collect()
        };

        for (output_local, input_local) in copy_pairs {
            self.load_local(output_local);
            self.store_local(input_local);
        }
    }

    /// Depth-first traversal using work stack to avoid recursion
    pub(super) fn translate_block(&mut self, initial_block_id: BasicBlockId) {
        let mut work_queue = Vec::new();
        work_queue.push(initial_block_id);

        while let Some(block_id) = work_queue.pop() {
            let idx = block_id.index();
            if self.state.translated_blocks[idx] {
                continue;
            }
            self.state.translated_blocks[idx] = true;

            self.translate_single_block(block_id, &mut work_queue);
        }
    }

    fn translate_single_block(
        &mut self,
        block_id: BasicBlockId,
        work_queue: &mut Vec<BasicBlockId>,
    ) {
        let block_mark = self.state.marks.get_block_mark(block_id);
        self.emit_mark(block_mark);

        let block = &self.program.basic_blocks[block_id];
        let operations_range = block.operations.clone();
        let control = block.control.clone();
        let outputs_range = block.outputs.clone();

        for op_idx in operations_range.iter() {
            self.translate_operation_by_index(op_idx, work_queue);
        }

        self.translate_control_with_outputs(&control, &outputs_range);

        match &control {
            Control::ContinuesTo(next) => {
                work_queue.push(*next);
            }
            Control::Branches(branch) => {
                work_queue.push(branch.zero_target);
                work_queue.push(branch.non_zero_target);
            }
            Control::Switch(switch) => {
                let cases = &self.program.cases[switch.cases].cases;
                for case in cases {
                    work_queue.push(case.target);
                }
                if let Some(fallback) = switch.fallback {
                    work_queue.push(fallback);
                }
            }
            Control::LastOpTerminates | Control::InternalReturn => {}
        }
    }

    fn translate_control_with_outputs(
        &mut self,
        control: &Control,
        outputs_range: &std::ops::Range<eth_ir_data::LocalIndex>,
    ) {
        match control {
            Control::LastOpTerminates => {}

            Control::ContinuesTo(next_block) => {
                if self.needs_locals_copy(outputs_range, *next_block) {
                    self.emit_locals_copy(outputs_range, *next_block);
                }
                let next_mark = self.state.marks.get_block_mark(*next_block);
                self.emit_jump(next_mark);
            }

            Control::Branches(branch) => {
                let needs_copy_zero = self.needs_locals_copy(outputs_range, branch.zero_target);
                let needs_copy_non_zero =
                    self.needs_locals_copy(outputs_range, branch.non_zero_target);

                self.load_local(branch.condition);

                if needs_copy_zero || needs_copy_non_zero {
                    let non_zero_copy_mark = self.state.marks.allocate_mark();
                    let zero_copy_mark = self.state.marks.allocate_mark();

                    self.emit_jumpi(non_zero_copy_mark);

                    self.emit_mark(zero_copy_mark);
                    if needs_copy_zero {
                        self.emit_locals_copy(outputs_range, branch.zero_target);
                    }
                    let zero_mark = self.state.marks.get_block_mark(branch.zero_target);
                    self.emit_jump(zero_mark);

                    self.emit_mark(non_zero_copy_mark);
                    if needs_copy_non_zero {
                        self.emit_locals_copy(outputs_range, branch.non_zero_target);
                    }
                    let non_zero_mark = self.state.marks.get_block_mark(branch.non_zero_target);
                    self.emit_jump(non_zero_mark);
                } else {
                    let non_zero_mark = self.state.marks.get_block_mark(branch.non_zero_target);
                    self.emit_jumpi(non_zero_mark);

                    let zero_mark = self.state.marks.get_block_mark(branch.zero_target);
                    self.emit_jump(zero_mark);
                }
            }

            Control::InternalReturn => {
                use evm_glue::opcodes::Opcode;
                self.state.asm.push(Asm::Op(Opcode::JUMP));
            }

            Control::Switch(switch) => {
                self.translate_switch_with_outputs(switch, outputs_range);
            }
        }
    }

    fn translate_switch_with_outputs(
        &mut self,
        switch: &eth_ir_data::Switch,
        outputs_range: &std::ops::Range<eth_ir_data::LocalIndex>,
    ) {
        use alloy_primitives::U256;
        use evm_glue::opcodes::Opcode;

        const SWITCH_TEMP_LOC: u32 = 0x00;

        self.load_local(switch.condition);
        self.push_const(U256::from(SWITCH_TEMP_LOC));
        self.state.asm.push(Asm::Op(Opcode::MSTORE));

        let case_info: Vec<_> = {
            let cases = &self.program.cases[switch.cases].cases;
            cases.iter().map(|case| (case.value, case.target)).collect()
        };

        let cases_data: Vec<_> = case_info
            .iter()
            .map(|&(value, target)| {
                let needs_copy = self.needs_locals_copy(outputs_range, target);
                let target_mark = if needs_copy {
                    self.state.marks.allocate_mark()
                } else {
                    self.state.marks.get_block_mark(target)
                };
                (value, target, target_mark, needs_copy)
            })
            .collect();

        for &(case_value, _target, target_mark, _needs_copy) in &cases_data {
            self.push_const(U256::from(SWITCH_TEMP_LOC));
            self.state.asm.push(Asm::Op(Opcode::MLOAD));
            self.push_const(case_value);
            self.state.asm.push(Asm::Op(Opcode::EQ));
            self.emit_jumpi(target_mark);
        }

        if let Some(fallback_block) = switch.fallback {
            if self.needs_locals_copy(outputs_range, fallback_block) {
                self.emit_locals_copy(outputs_range, fallback_block);
            }
            let fallback_mark = self.state.marks.get_block_mark(fallback_block);
            self.emit_jump(fallback_mark);
        } else {
            self.emit_runtime_error(crate::runtime_errors::SWITCH_NO_MATCH);
        }

        for &(_, target, copy_mark, needs_copy) in &cases_data {
            if needs_copy {
                self.emit_mark(copy_mark);
                self.emit_locals_copy(outputs_range, target);
                let case_mark = self.state.marks.get_block_mark(target);
                self.emit_jump(case_mark);
            }
        }
    }
}
