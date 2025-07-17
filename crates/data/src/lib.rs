pub mod analysis;
pub mod index;
pub mod operation;

pub use crate::{analysis::DataSegmentAnalysis, index::*, operation::Operation};
use alloy_primitives::U256;
use std::{fmt, ops::Range};

/// Implemented in a data oriented way. Instead of each basic block and function holding its own
/// vector of items they're all stored contiguously in the top level program
#[derive(Debug, Clone)]
pub struct EthIRProgram {
    pub init_entry: FunctionId,
    pub main_entry: Option<FunctionId>,
    // Top Level IR Structure
    pub functions: IndexVec<FunctionId, Function>,
    pub basic_blocks: IndexVec<BasicBlockId, BasicBlock>,
    pub operations: IndexVec<OperationIndex, Operation>,
    // Small IR Pieces
    pub locals: IndexVec<LocalIndex, LocalId>,
    pub data_bytes: IndexVec<DataOffset, u8>,
    pub large_consts: IndexVec<LargeConstId, U256>,
    // Control Flow
    pub cases: IndexVec<CasesId, Cases>,
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
    InternalReturn(LocalId),
    ContinuesTo(BasicBlockId),
    Branches(Branch),
    Switch(Switch),
}

impl fmt::Display for EthIRProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Analyze data segments
        let data_analysis = DataSegmentAnalysis::analyze(self);

        // Display functions
        for (id, func) in self.functions.iter_enumerated() {
            write!(f, "fn @{} {}:", id.get(), func.outputs)?;

            // Display basic blocks for this function
            for (bb_id, bb) in self.basic_blocks.iter_enumerated() {
                // Only display blocks that belong to this function
                // (This is a simplified approach - in a real implementation we'd track which blocks belong to which function)
                writeln!(f)?;
                self.fmt_basic_block(f, bb_id, bb, &data_analysis)?;
            }
            writeln!(f)?;
        }

        // Display data segments
        if !self.data_bytes.is_empty() {
            writeln!(f)?;

            for (&start, &(end, id)) in &data_analysis.segments {
                // Only display referenced segments
                if data_analysis.referenced_segments.contains(&id) {
                    write!(f, "data .{id} ")?;

                    // Display hex bytes for the segment
                    write!(f, "0x")?;
                    for i in start..end {
                        write!(f, "{:02x}", self.data_bytes[DataOffset::new(i)])?;
                    }
                    writeln!(f)?;
                }
            }
        }

        Ok(())
    }
}

impl EthIRProgram {
    fn fmt_basic_block(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: BasicBlockId,
        bb: &BasicBlock,
        data_analysis: &DataSegmentAnalysis,
    ) -> fmt::Result {
        write!(f, "    @{}", id.get())?;

        // Display inputs
        if !bb.inputs.is_empty() {
            for i in Idx::index(bb.inputs.start)..Idx::index(bb.inputs.end) {
                let local_id = self.locals[LocalIndex::from_usize(i)];
                write!(f, " ${}", local_id.get())?;
            }
        }

        // Display outputs
        if !bb.outputs.is_empty() {
            write!(f, " ->")?;
            for i in Idx::index(bb.outputs.start)..Idx::index(bb.outputs.end) {
                let local_id = self.locals[LocalIndex::from_usize(i)];
                write!(f, " ${}", local_id.get())?;
            }
        }

        writeln!(f, " {{")?;

        // Display operations
        for i in Idx::index(bb.operations.start)..Idx::index(bb.operations.end) {
            write!(f, "        ")?;
            self.fmt_operation(f, &self.operations[OperationIndex::from_usize(i)], data_analysis)?;
            writeln!(f)?;
        }

        // Display control flow
        write!(f, "        ")?;
        self.fmt_control(f, &bb.control)?;
        writeln!(f)?;

        write!(f, "    }}")
    }

    fn fmt_operation(
        &self,
        f: &mut fmt::Formatter<'_>,
        op: &Operation,
        data_analysis: &DataSegmentAnalysis,
    ) -> fmt::Result {
        match op {
            Operation::InternalCall(call) => {
                // Special handling for internal call
                let num_outputs = self.functions[call.function].outputs;
                if num_outputs > 0 {
                    for i in 0..num_outputs {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        let idx = LocalIndex::new(call.outputs_start.get() + i);
                        write!(f, "${}", self.locals[idx].get())?;
                    }
                    write!(f, " = ")?;
                }
                write!(f, "icall @{}", call.function.get())?;

                // Display arguments
                let num_args = call.outputs_start.get().saturating_sub(call.args_start.get());
                for i in 0..num_args {
                    let idx = LocalIndex::new(call.args_start.get() + i);
                    write!(f, " ${}", self.locals[idx].get())?;
                }
                Ok(())
            }
            _ => op.fmt_display(f, &self.locals, &self.large_consts, data_analysis),
        }
    }

    fn fmt_control(&self, f: &mut fmt::Formatter<'_>, control: &Control) -> fmt::Result {
        use Control::*;
        match control {
            LastOpTerminates => Ok(()),
            InternalReturn(local) => write!(f, "iret ${}", local.get()),
            ContinuesTo(bb) => write!(f, "=> @{}", bb.get()),
            Branches(branch) => write!(
                f,
                "=> ${} ? @{} : @{}",
                branch.condition.get(),
                branch.non_zero_target.get(),
                branch.zero_target.get()
            ),
            Switch(switch) => {
                write!(f, "switch ${} {{", switch.condition.get())?;
                let cases = &self.cases[switch.cases];
                for (i, case) in cases.cases.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} => @{}", case.value, case.target.get())?;
                }
                if let Some(fallback) = switch.fallback {
                    write!(f, ", _ => @{}}}", fallback.get())
                } else {
                    write!(f, "}}")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn control_memory_layout() {
        assert_eq!(std::mem::size_of::<Control>(), 16, "changed desired control size");
        assert_eq!(std::mem::align_of::<Control>(), 4, "changed desired control alignment");
    }

    #[test]
    fn test_display() {
        use crate::index::*;
        use crate::operation::*;

        // Create a simple program
        let program = EthIRProgram {
            entry: FunctionId::new(0),
            functions: index_vec![Function { entry: BasicBlockId::new(0), outputs: 1 }],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(2),
                outputs: LocalIndex::from_usize(2)..LocalIndex::from_usize(3),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(2),
                control: Control::InternalReturn(LocalId::new(2)),
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
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
        };

        let display = format!("{}", program);
        println!("{}", display);
        assert!(display.contains("fn @0 1:"));
        assert!(display.contains("@0 $0 $1 -> $2"));
        assert!(display.contains("$2 = add $0 $1"));
        assert!(display.contains("iret $2"));
    }

    #[test]
    fn test_display_with_data() {
        use crate::index::*;
        use crate::operation::*;

        // Create a program with data segments and large constants
        let program = EthIRProgram {
            entry: FunctionId::new(0),
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
                    value: DataOffset::new(2)..DataOffset::new(6),
                }),
                Operation::Stop,
            ],
            locals: index_vec![LocalId::new(0), LocalId::new(1),],
            data_bytes: index_vec![0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0],
            large_consts: index_vec![U256::from(0xdeadbeef_u64)],
            cases: index_vec![],
        };

        let display = format!("{}", program);
        println!("{}", display);

        // Check data segment display
        assert!(display.contains("data .1 0x56789abc"));

        // Check large constant displayed inline
        assert!(display.contains("$0 = 0xdeadbeef"));

        // Check data offset uses segment ID
        assert!(display.contains("$1 = .1"));
    }
}
