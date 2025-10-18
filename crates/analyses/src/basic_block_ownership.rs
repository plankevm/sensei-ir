use sir_data::{BasicBlockId, EthIRProgram, FunctionId, IndexVec, index_vec};

#[derive(Debug, Clone)]
pub struct BasicBlockOwnershipAndReachability {
    /// Maps each basic block to its owning function.
    /// None means the block is unreachable from any function.
    ownership: IndexVec<BasicBlockId, Option<FunctionId>>,
}

impl BasicBlockOwnershipAndReachability {
    /// Analyze the program to determine which function owns each basic block.
    pub fn analyze(program: &EthIRProgram) -> Self {
        // Initialize ownership vector with None for each basic block
        let mut ownership = index_vec![None; program.basic_blocks.len()];

        // Process each function
        for (func_id, func) in program.functions.iter_enumerated() {
            // Start DFS from the function's entry block
            Self::mark_reachable_blocks(&mut ownership, program, func.entry(), func_id);
        }

        Self { ownership }
    }

    /// Depth-first traversal to mark all blocks reachable from the given entry
    fn mark_reachable_blocks(
        ownership: &mut IndexVec<BasicBlockId, Option<FunctionId>>,
        program: &EthIRProgram,
        current: BasicBlockId,
        owner: FunctionId,
    ) {
        // Check if already visited
        if ownership[current].is_some() {
            return;
        }

        ownership[current] = Some(owner);
        let bb = &program.basic_blocks[current];

        // Use the new iter_outgoing API to get all successor basic blocks
        for successor in bb.control.iter_outgoing(program) {
            Self::mark_reachable_blocks(ownership, program, successor, owner);
        }
    }

    /// Get the function that owns a basic block, if any
    pub fn get_owner(&self, block: BasicBlockId) -> Option<FunctionId> {
        self.ownership[block]
    }

    /// Check if a basic block is reachable from any function
    pub fn is_reachable(&self, block: BasicBlockId) -> bool {
        self.ownership[block].is_some()
    }

    /// Get all basic blocks owned by a specific function
    pub fn blocks_owned_by(&self, func: FunctionId) -> impl Iterator<Item = BasicBlockId> + '_ {
        self.ownership
            .iter_enumerated()
            .filter_map(move |(bb_id, owner)| if *owner == Some(func) { Some(bb_id) } else { None })
    }

    /// Get all unreachable basic blocks
    pub fn unreachable_blocks(&self) -> impl Iterator<Item = BasicBlockId> + '_ {
        self.ownership
            .iter_enumerated()
            .filter_map(move |(bb_id, owner)| if owner.is_none() { Some(bb_id) } else { None })
    }

    /// Display IR with basic blocks grouped by function
    pub fn display_ir_with_function_grouping(&self, program: &EthIRProgram) -> String {
        use std::fmt::Write;
        let mut output = String::new();

        // Display functions with their owned basic blocks
        for (func_id, _func) in program.functions.iter_enumerated() {
            writeln!(&mut output, "fn @{}:", func_id).unwrap();

            // Display all basic blocks owned by this function
            for bb_id in self.blocks_owned_by(func_id) {
                let bb = &program.basic_blocks[bb_id];
                bb.fmt_display(&mut output, bb_id, program).unwrap();
                writeln!(&mut output).unwrap();
            }
        }

        // Display unreachable basic blocks
        let mut unreachable = self.unreachable_blocks().peekable();
        if unreachable.peek().is_some() {
            writeln!(&mut output, "// Unreachable basic blocks").unwrap();
            for bb_id in unreachable {
                let bb = &program.basic_blocks[bb_id];
                bb.fmt_display(&mut output, bb_id, program).unwrap();
                writeln!(&mut output).unwrap();
            }
        }

        // Display data segments
        if !program.data_segments_start.is_empty() {
            writeln!(&mut output).unwrap();

            for (segment_id, _) in program.data_segments_start.iter_enumerated() {
                write!(&mut output, "data .{segment_id} ").unwrap();

                // Display hex bytes for the segment
                let range = program.get_segment_range(segment_id);
                write!(&mut output, "0x").unwrap();
                for i in range.start.get()..range.end.get() {
                    write!(&mut output, "{:02x}", program.data_bytes[sir_data::DataOffset::new(i)])
                        .unwrap();
                }
                writeln!(&mut output).unwrap();
            }
        }

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sir_data::{
        BasicBlock, Branch, Control, Function, LocalId, LocalIndex, OperationIndex, index_vec,
        operation::*,
    };

    #[test]
    fn test_simple_ownership() {
        // Create a simple program with one function and two basic blocks
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function::new(BasicBlockId::new(0), 0)],
            basic_blocks: index_vec![
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(0)..OperationIndex::new(1),
                    control: Control::ContinuesTo(BasicBlockId::new(1)),
                },
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(1)..OperationIndex::new(2),
                    control: Control::LastOpTerminates,
                },
            ],
            operations: index_vec![
                Operation::Noop(Default::default()),
                Operation::Stop(Default::default()),
            ],
            locals: index_vec![],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
            cases_bb_ids: index_vec![],
        };

        let analysis = BasicBlockOwnershipAndReachability::analyze(&program);

        // Both blocks should be owned by function 0
        assert_eq!(analysis.get_owner(BasicBlockId::new(0)), Some(FunctionId::new(0)));
        assert_eq!(analysis.get_owner(BasicBlockId::new(1)), Some(FunctionId::new(0)));

        // Both blocks should be reachable
        assert!(analysis.is_reachable(BasicBlockId::new(0)));
        assert!(analysis.is_reachable(BasicBlockId::new(1)));

        // No unreachable blocks
        assert!(analysis.unreachable_blocks().next().is_none());
    }

    #[test]
    fn test_unreachable_blocks() {
        // Create a program with unreachable blocks
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function::new(BasicBlockId::new(0), 0)],
            basic_blocks: index_vec![
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(0)..OperationIndex::new(1),
                    control: Control::LastOpTerminates,
                },
                // This block is unreachable
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(1)..OperationIndex::new(2),
                    control: Control::LastOpTerminates,
                },
            ],
            operations: index_vec![
                Operation::Stop(Default::default()),
                Operation::Stop(Default::default()),
            ],
            locals: index_vec![],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
            cases_bb_ids: index_vec![],
        };

        let analysis = BasicBlockOwnershipAndReachability::analyze(&program);

        // Block 0 is reachable, block 1 is not
        assert!(analysis.is_reachable(BasicBlockId::new(0)));
        assert!(!analysis.is_reachable(BasicBlockId::new(1)));

        // Block 1 should be in unreachable list
        assert_eq!(analysis.unreachable_blocks().collect::<Vec<_>>(), vec![BasicBlockId::new(1)]);
    }

    #[test]
    fn test_multiple_functions() {
        // Create a program with two functions
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![
                Function::new(BasicBlockId::new(0), 0),
                Function::new(BasicBlockId::new(2), 1),
            ],
            basic_blocks: index_vec![
                // Function 0 blocks
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(0)..OperationIndex::new(1),
                    control: Control::ContinuesTo(BasicBlockId::new(1)),
                },
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(1)..OperationIndex::new(2),
                    control: Control::LastOpTerminates,
                },
                // Function 1 blocks
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(2)..OperationIndex::new(3),
                    control: Control::InternalReturn,
                },
            ],
            operations: index_vec![
                Operation::Noop(Default::default()),
                Operation::Stop(Default::default()),
                Operation::Noop(Default::default()),
            ],
            locals: index_vec![],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
            cases_bb_ids: index_vec![],
        };

        let analysis = BasicBlockOwnershipAndReachability::analyze(&program);

        // Check ownership
        assert_eq!(analysis.get_owner(BasicBlockId::new(0)), Some(FunctionId::new(0)));
        assert_eq!(analysis.get_owner(BasicBlockId::new(1)), Some(FunctionId::new(0)));
        assert_eq!(analysis.get_owner(BasicBlockId::new(2)), Some(FunctionId::new(1)));

        // Check blocks owned by each function
        assert_eq!(
            analysis.blocks_owned_by(FunctionId::new(0)).collect::<Vec<_>>(),
            vec![BasicBlockId::new(0), BasicBlockId::new(1)]
        );
        assert_eq!(
            analysis.blocks_owned_by(FunctionId::new(1)).collect::<Vec<_>>(),
            vec![BasicBlockId::new(2)]
        );
    }

    #[test]
    fn test_branching_control_flow() {
        // Create a program with branching
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function::new(BasicBlockId::new(0), 0)],
            basic_blocks: index_vec![
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(0)..OperationIndex::new(1),
                    control: Control::Branches(Branch {
                        condition: LocalId::new(0),
                        zero_target: BasicBlockId::new(1),
                        non_zero_target: BasicBlockId::new(2),
                    }),
                },
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(1)..OperationIndex::new(2),
                    control: Control::LastOpTerminates,
                },
                BasicBlock {
                    inputs: LocalIndex::new(0)..LocalIndex::new(0),
                    outputs: LocalIndex::new(0)..LocalIndex::new(0),
                    operations: OperationIndex::new(2)..OperationIndex::new(3),
                    control: Control::LastOpTerminates,
                },
            ],
            operations: index_vec![
                Operation::Noop(Default::default()),
                Operation::Stop(Default::default()),
                Operation::Stop(Default::default()),
            ],
            locals: index_vec![LocalId::new(0)],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
            cases_bb_ids: index_vec![],
        };

        let analysis = BasicBlockOwnershipAndReachability::analyze(&program);

        // All blocks should be owned by function 0
        assert_eq!(analysis.get_owner(BasicBlockId::new(0)), Some(FunctionId::new(0)));
        assert_eq!(analysis.get_owner(BasicBlockId::new(1)), Some(FunctionId::new(0)));
        assert_eq!(analysis.get_owner(BasicBlockId::new(2)), Some(FunctionId::new(0)));

        // All blocks should be reachable
        assert!(analysis.is_reachable(BasicBlockId::new(0)));
        assert!(analysis.is_reachable(BasicBlockId::new(1)));
        assert!(analysis.is_reachable(BasicBlockId::new(2)));
    }
}
