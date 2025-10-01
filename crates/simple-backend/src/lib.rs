use std::ops::Range;

use eth_ir_data::{BasicBlockId, EthIRProgram, FunctionId};
use eth_ir_data::{Function, IndexVec};
use evm_glue::{assembly::Asm, utils::MarkTracker};

pub fn emit_assembly(entry: FunctionId, ir: &EthIRProgram) -> Vec<Asm> {
    let mut evm_mem_layout = EvmLayoutBuilder::new();
    let mut code_layout = CodeLayoutBuider::new();

    for (id, function) in ir.functions.iter_enumerated() {
        evm_mem_layout.add_function(id, function.get_inputs(ir), function.outputs);
        code_layout.add_fn(id);
    }

    for (id, _) in ir.basic_blocks.iter_enumerated() {
        code_layout.add_bb(id);
    }

    todo!()
}

/*
 * We need to allocate
 * - free memory pointer for dynamic allocations
 * - functions (including entry point)
 *      - all unique locals
 *      - (additionally) basic block inputs (inputs get copied into locals at BB entry)
 *      - function outputs
 *      - function inputs
 */

type MemorySlot = u32;

struct EvmLayoutBuilder {
    next_free_offset: MemorySlot,
    functions: IndexVec<FunctionId, FunctionMemLayout>,
}

struct CodeLayoutBuider {
    marks: MarkTracker,
    // TODO: Optimize these this as the mappings should be contiguous anyway.
    fn_marks: IndexVec<FunctionId, usize>,
    bb_marks: IndexVec<BasicBlockId, usize>,
    data_obj_marks: IndexVec<BasicBlockId, usize>,
}

const FUNCTIONS_DEFAULT_CAPACITY: usize = 512;
const BASIC_BLOCKS_DEFAULT_CAPACITY: usize = 4096;
const DATA_OBJECTS_DEFAULT_CAPACITY: usize = 16;

impl CodeLayoutBuider {
    fn new() -> Self {
        Self {
            marks: MarkTracker::default(),
            fn_marks: IndexVec::with_capacity(FUNCTIONS_DEFAULT_CAPACITY),
            bb_marks: IndexVec::with_capacity(BASIC_BLOCKS_DEFAULT_CAPACITY),
            data_obj_marks: IndexVec::with_capacity(DATA_OBJECTS_DEFAULT_CAPACITY),
        }
    }

    fn add_fn(&mut self, id: FunctionId) {
        let next_mark = self.marks.next_mark();
        assert!(self.fn_marks.last().is_none_or(|m| m + 1 == next_mark), "marks not contiguous");
        let new_id = self.fn_marks.push(next_mark);
        assert_eq!(new_id, id, "IDs not contiguous");
    }

    fn add_bb(&mut self, id: BasicBlockId) {
        let next_mark = self.marks.next_mark();
        assert!(self.bb_marks.last().is_none_or(|m| m + 1 == next_mark), "marks not contiguous");
        let new_id = self.bb_marks.push(next_mark);
        assert_eq!(new_id, id, "IDs not contiguous");
    }
}

struct FunctionMemLayout {
    inputs_start: MemorySlot,
    outputs_start: MemorySlot,
    outputs_end: MemorySlot,
}

impl FunctionMemLayout {
    fn inputs(&self) -> Range<MemorySlot> {
        self.inputs_start..self.outputs_start
    }

    fn outputs(&self) -> Range<MemorySlot> {
        self.outputs_start..self.outputs_end
    }
}

impl EvmLayoutBuilder {
    fn new() -> Self {
        Self { next_free_offset: 0, functions: IndexVec::with_capacity(FUNCTIONS_DEFAULT_CAPACITY) }
    }

    fn inner_get_memory(&mut self, slots: u32) -> Range<MemorySlot> {
        let offset = self.next_free_offset;
        self.next_free_offset += slots;
        offset..self.next_free_offset
    }

    fn add_function(&mut self, id: FunctionId, inputs: u32, outputs: u32) {
        let inputs = self.inner_get_memory(inputs);
        let outputs = self.inner_get_memory(outputs);
        assert_eq!(inputs.end, outputs.start);
        let pushed_idx = self.functions.push(FunctionMemLayout {
            inputs_start: inputs.start,
            outputs_start: outputs.start,
            outputs_end: outputs.end,
        });
        assert_eq!(pushed_idx, id, "setting unexpected");
    }
}
