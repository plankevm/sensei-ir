pub mod analysis;
pub mod index;
pub mod operation;

pub use crate::{
    analysis::BasicBlockOwnershipAndReachability,
    index::*,
    operation::{InternalCall, Operation},
};
use alloy_primitives::U256;
use std::{fmt, ops::Range};

/// Implemented in a data oriented way. Instead of each basic block and function holding its own
/// vector of items they're all stored contiguously in the top level program
#[derive(Debug, Clone)]
pub struct EthIRProgram {
    // Entry Points
    pub init_entry: FunctionId,
    pub main_entry: Option<FunctionId>,
    // IR Statements
    pub functions: IndexVec<FunctionId, Function>,
    pub basic_blocks: IndexVec<BasicBlockId, BasicBlock>,
    pub operations: IndexVec<OperationIndex, Operation>,
    pub data_segments_start: IndexVec<DataId, DataOffset>,
    // IR Data
    pub locals: IndexVec<LocalIndex, LocalId>,
    pub data_bytes: IndexVec<DataOffset, u8>,
    pub large_consts: IndexVec<LargeConstId, U256>,
    pub cases: IndexVec<CasesId, Cases>,
}

impl EthIRProgram {
    /// Get the byte range for a data segment
    pub fn get_segment_range(&self, segment_id: DataId) -> Range<DataOffset> {
        let start = self.data_segments_start[segment_id];
        let next_segment = segment_id + 1;
        let end = self
            .data_segments_start
            .get(next_segment)
            .copied()
            .unwrap_or_else(|| self.data_bytes.len_idx());
        start..end
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub entry: BasicBlockId,
    pub outputs: u32,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// Input locals.
    pub inputs: Range<LocalIndex>,
    pub outputs: Range<LocalIndex>,
    pub operations: Range<OperationIndex>,
    pub control: Control,
}

impl BasicBlock {
    pub fn fmt_display(
        &self,
        f: &mut fmt::Formatter<'_>,
        bb_id: BasicBlockId,
        ir: &EthIRProgram,
    ) -> fmt::Result {
        write!(f, "    @{bb_id}")?;

        // Display inputs
        if !self.inputs.is_empty() {
            for local in &ir.locals[self.inputs.clone()] {
                write!(f, " ${local}")?;
            }
        }

        // Display outputs
        if !self.outputs.is_empty() {
            write!(f, " ->")?;
            for local in &ir.locals[self.outputs.clone()] {
                write!(f, " ${local}")?;
            }
        }

        writeln!(f, " {{")?;

        // Display operations
        for op in &ir.operations[self.operations.clone()] {
            write!(f, "        ")?;
            match op {
                Operation::InternalCall(call) => {
                    // Format internal call with function information
                    let num_outputs = ir.functions[call.function].outputs;
                    if num_outputs > 0 {
                        for i in 0..num_outputs {
                            if i > 0 {
                                write!(f, " ")?;
                            }
                            let idx = LocalIndex::new(call.outputs_start.get() + i);
                            write!(f, "${}", ir.locals[idx])?;
                        }
                        write!(f, " = ")?;
                    }
                    write!(f, "icall @{}", call.function)?;

                    // Display arguments
                    let num_args = call.outputs_start.get().saturating_sub(call.args_start.get());
                    for i in 0..num_args {
                        let idx = LocalIndex::new(call.args_start.get() + i);
                        write!(f, " ${}", ir.locals[idx])?;
                    }
                }
                _ => op.fmt_display(f, &ir.locals, &ir.large_consts)?,
            }
            writeln!(f)?;
        }

        // Display control flow
        match &self.control {
            Control::LastOpTerminates => {}
            _ => {
                write!(f, "        ")?;
                self.control.fmt_display(f, &ir.cases)?;
                writeln!(f)?;
            }
        }

        writeln!(f, "    }}")
    }
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub condition: LocalId,
    pub non_zero_target: BasicBlockId,
    pub zero_target: BasicBlockId,
}

// Kept small to ensure that `Control` is no larger because of it. This is because I expect `Switch`
// to not be that common so I don't want to optimize for it.
#[derive(Debug, Clone)]
pub struct Switch {
    pub condition: LocalId,
    pub fallback: Option<BasicBlockId>,
    pub cases: CasesId,
}

#[derive(Debug, Clone)]
pub struct Case {
    pub value: U256,
    pub target: BasicBlockId,
}

// TODO: Optimized memory layout.
#[derive(Debug, Clone)]
pub struct Cases {
    pub cases: Vec<Case>,
}

#[derive(Debug, Clone)]
pub enum Control {
    LastOpTerminates,
    InternalReturn,
    ContinuesTo(BasicBlockId),
    Branches(Branch),
    Switch(Switch),
}

impl Control {
    pub fn fmt_display(
        &self,
        f: &mut fmt::Formatter<'_>,
        cases: &IndexSlice<CasesId, [Cases]>,
    ) -> fmt::Result {
        use Control as C;
        match self {
            C::LastOpTerminates => Ok(()),
            C::InternalReturn => write!(f, "iret"),
            C::ContinuesTo(bb) => write!(f, "=> @{bb}"),
            C::Branches(branch) => write!(
                f,
                "=> ${} ? @{} : @{}",
                branch.condition, branch.non_zero_target, branch.zero_target
            ),
            C::Switch(switch) => {
                writeln!(f, "switch ${} {{", switch.condition)?;
                let switch_cases = &cases[switch.cases];
                for case in switch_cases.cases.iter() {
                    writeln!(f, "{:x} => @{},", case.value, case.target)?;
                }
                if let Some(fallback) = switch.fallback {
                    writeln!(f, "_ => @{fallback}}}")
                } else {
                    writeln!(f, "}}")
                }
            }
        }
    }
}

impl fmt::Display for EthIRProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Analyze basic block ownership
        let ownership_analysis = BasicBlockOwnershipAndReachability::analyze(self);

        // Display functions with their owned basic blocks
        for (func_id, func) in self.functions.iter_enumerated() {
            writeln!(f, "fn @{} {}:", func_id, func.outputs)?;

            // Display all basic blocks owned by this function
            for bb_id in ownership_analysis.blocks_owned_by(func_id) {
                let bb = &self.basic_blocks[bb_id];
                bb.fmt_display(f, bb_id, self)?;
                writeln!(f)?;
            }
        }

        // Display unreachable basic blocks at the end
        let unreachable_blocks: Vec<_> = ownership_analysis.unreachable_blocks().collect();
        if !unreachable_blocks.is_empty() {
            writeln!(f, "// Unreachable basic blocks")?;
            for bb_id in unreachable_blocks {
                let bb = &self.basic_blocks[bb_id];
                bb.fmt_display(f, bb_id, self)?;
                writeln!(f)?;
            }
        }

        // Display data segments
        if !self.data_segments_start.is_empty() {
            writeln!(f)?;

            for (segment_id, _) in self.data_segments_start.iter_enumerated() {
                write!(f, "data .{segment_id} ")?;

                // Display hex bytes for the segment
                let range = self.get_segment_range(segment_id);
                write!(f, "0x")?;
                for i in range.start.get()..range.end.get() {
                    write!(f, "{:02x}", self.data_bytes[DataOffset::new(i)])?;
                }
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_ir_display(program: &EthIRProgram, expected: &str) {
        let actual = format!("{}", program);
        test_utils::assert_strings_with_diff(&actual, expected, "IR display", None);
    }

    #[test]
    fn control_memory_layout() {
        assert_eq!(std::mem::size_of::<Control>(), 16, "changed desired control size");
        assert_eq!(std::mem::align_of::<Control>(), 4, "changed desired control alignment");
    }

    #[test]
    fn test_display() {
        use crate::{index::*, operation::*};

        // Create a simple program
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 1 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(2),
                outputs: LocalIndex::from_usize(2)..LocalIndex::from_usize(3),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(2),
                control: Control::InternalReturn,
            }],
            operations: index_vec![
                Operation::Add(TwoInOneOut {
                    result: LocalId::new(2),
                    arg1: LocalId::new(0),
                    arg2: LocalId::new(1)
                }),
                Operation::Stop,
            ],
            locals: index_vec![LocalId::new(0), LocalId::new(1), LocalId::new(2),],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let expected = r#"
fn @0 1:
    @0 $0 $1 -> $2 {
        $2 = add $0 $1
        stop
        iret
    }
"#;

        assert_ir_display(&program, expected);
    }

    #[test]
    fn test_display_with_unreachable_blocks() {
        use crate::{index::*, operation::*};

        // Create a program with unreachable blocks
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![
                Function { entry: BasicBlockId::new(0), outputs: 0 },
                Function { entry: BasicBlockId::new(2), outputs: 1 }
            ],
            basic_blocks: index_vec![
                // Function 0 block
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(1),
                    control: Control::LastOpTerminates,
                },
                // Unreachable block
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                    operations: OperationIndex::from_usize(1)..OperationIndex::from_usize(2),
                    control: Control::LastOpTerminates,
                },
                // Function 1 block
                BasicBlock {
                    inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(1),
                    outputs: LocalIndex::from_usize(1)..LocalIndex::from_usize(2),
                    operations: OperationIndex::from_usize(2)..OperationIndex::from_usize(3),
                    control: Control::InternalReturn,
                },
                // Another unreachable block
                BasicBlock {
                    inputs: LocalIndex::from_usize(2)..LocalIndex::from_usize(2),
                    outputs: LocalIndex::from_usize(2)..LocalIndex::from_usize(2),
                    operations: OperationIndex::from_usize(3)..OperationIndex::from_usize(4),
                    control: Control::LastOpTerminates,
                },
            ],
            operations: index_vec![
                Operation::Stop,
                Operation::Invalid,
                Operation::LocalSet(OneInOneOut { result: LocalId::new(1), arg1: LocalId::new(0) }),
                Operation::Stop,
            ],
            locals: index_vec![LocalId::new(0), LocalId::new(1),],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let expected = r#"
fn @0 0:
    @0 {
        stop
    }

fn @1 1:
    @2 $0 -> $1 {
        $1 = $0
        iret
    }

// Unreachable basic blocks
    @1 {
        invalid
    }

    @3 {
        stop
    }
"#;

        assert_ir_display(&program, expected);
    }

    #[test]
    fn test_display_with_data() {
        use crate::{index::*, operation::*};

        // Create a program with data segments and large constants
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
                Operation::LocalSetLargeConst(SetLargeConst {
                    local: LocalId::new(0),
                    cid: LargeConstId::new(0),
                }),
                Operation::LocalSetDataOffset(SetDataOffset {
                    local: LocalId::new(1),
                    segment_id: DataId::new(1),
                }),
                Operation::Stop,
            ],
            locals: index_vec![LocalId::new(0), LocalId::new(1),],
            data_segments_start: index_vec![
                DataOffset::new(0),
                DataOffset::new(2),
                DataOffset::new(6)
            ],
            data_bytes: index_vec![0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0],
            large_consts: index_vec![U256::from(0xdeadbeef_u64)],
            cases: index_vec![],
        };

        let expected = r#"
fn @0 0:
    @0 {
        $0 = 0xdeadbeef
        $1 = .1
        stop
    }


data .0 0x1234
data .1 0x56789abc
data .2 0xdef0
"#;

        assert_ir_display(&program, expected);
    }
}
