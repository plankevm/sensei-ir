//! Mark (jump label) management for basic blocks and functions
//!
//! evm-glue uses Mark for jump destinations. We need to allocate
//! unique marks for each BasicBlockId and FunctionId.

use eth_ir_data::{BasicBlockId, FunctionId};
use std::collections::HashMap;

/// Type alias for mark IDs used in evm-glue assembly
pub type MarkId = usize;

/// Manages allocation of marks (jump labels)
pub struct MarkAllocator {
    /// Maps BasicBlockId to its mark ID
    block_marks: HashMap<BasicBlockId, MarkId>,
    /// Maps FunctionId to its mark ID
    function_marks: HashMap<FunctionId, MarkId>,
    /// Next available mark ID
    next_mark: MarkId,
}

impl MarkAllocator {
    pub fn new() -> Self {
        Self { block_marks: HashMap::new(), function_marks: HashMap::new(), next_mark: 0 }
    }

    /// Allocate a new mark ID
    pub fn allocate_mark(&mut self) -> MarkId {
        let mark = self.next_mark;
        self.next_mark += 1;
        mark
    }

    /// Get or allocate a mark for a basic block
    pub fn get_block_mark(&mut self, block_id: BasicBlockId) -> MarkId {
        if let Some(&mark) = self.block_marks.get(&block_id) {
            mark
        } else {
            let mark = self.allocate_mark();
            self.block_marks.insert(block_id, mark);
            mark
        }
    }

    /// Get or allocate a mark for a function
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
