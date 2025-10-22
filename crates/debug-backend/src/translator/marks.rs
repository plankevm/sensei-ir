//! Mark (jump label) management for basic blocks and functions

use sir_data::{BasicBlockId, FunctionId};
use std::collections::HashMap;

pub type MarkId = usize;

pub struct MarkAllocator {
    block_marks: HashMap<BasicBlockId, MarkId>,
    function_marks: HashMap<FunctionId, MarkId>,
    next_mark: MarkId,
}

impl MarkAllocator {
    pub fn new() -> Self {
        Self { block_marks: HashMap::new(), function_marks: HashMap::new(), next_mark: 0 }
    }

    pub fn allocate_mark(&mut self) -> MarkId {
        let mark = self.next_mark;
        self.next_mark += 1;
        mark
    }

    pub fn get_block_mark(&mut self, block_id: BasicBlockId) -> MarkId {
        if let Some(&mark) = self.block_marks.get(&block_id) {
            mark
        } else {
            let mark = self.allocate_mark();
            self.block_marks.insert(block_id, mark);
            mark
        }
    }

    pub fn get_function_mark(&mut self, func_id: FunctionId) -> MarkId {
        if let Some(&mark) = self.function_marks.get(&func_id) {
            mark
        } else {
            let mark = self.allocate_mark();
            self.function_marks.insert(func_id, mark);
            mark
        }
    }
}
