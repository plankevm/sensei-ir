pub mod builder;
pub mod index;
pub mod operation;

pub use crate::{index::*, operation::Operation};
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
    pub cases_bb_ids: IndexVec<CasesBasicBlocksIndex, BasicBlockId>,
}

impl EthIRProgram {
    /// Get the byte range for a data segment
    pub fn get_segment_range(&self, id: DataId) -> Range<DataOffset> {
        let start = self.data_segments_start[id];
        match self.data_segments_start.get(id + 1) {
            Some(&end) => start..end,
            None => start..self.data_bytes.len_idx(),
        }
    }
}

/// Simple display of IR program - shows all elements independently without grouping
pub fn display_program(ir: &EthIRProgram) -> String {
    use fmt::Write;
    let mut output = String::new();

    if !ir.functions.is_empty() {
        writeln!(&mut output, "Functions:").unwrap();
    }

    // Display functions
    for (fn_id, func) in ir.functions.iter_enumerated() {
        writeln!(
            &mut output,
            "    fn @{} -> entry @{}  (outputs: {})",
            fn_id,
            func.entry(),
            func.get_outputs()
        )
        .unwrap();
    }

    if !ir.functions.is_empty() {
        writeln!(&mut output).unwrap();
    }

    writeln!(&mut output, "Basic Blocks:").unwrap();

    // Display all basic blocks
    for (bb_id, bb) in ir.basic_blocks.iter_enumerated() {
        use std::fmt::Write as _;
        bb.fmt_display(&mut output, bb_id, ir).unwrap();
        writeln!(&mut output).unwrap();
    }

    // Display data segments
    if !ir.data_segments_start.is_empty() {
        writeln!(&mut output).unwrap();

        for (segment_id, _) in ir.data_segments_start.iter_enumerated() {
            write!(&mut output, "data .{segment_id} ").unwrap();

            let range = ir.get_segment_range(segment_id);
            write!(&mut output, "0x").unwrap();
            for i in range.start.get()..range.end.get() {
                write!(&mut output, "{:02x}", ir.data_bytes[DataOffset::new(i)]).unwrap();
            }
            writeln!(&mut output).unwrap();
        }
    }

    output
}

#[derive(Debug, Clone, Copy)]
pub struct Function {
    entry_bb_id: BasicBlockId,
    outputs: u32,
}

impl Function {
    pub fn new(entry_bb_id: BasicBlockId, outputs: u32) -> Self {
        Self { entry_bb_id, outputs }
    }

    pub fn entry(&self) -> BasicBlockId {
        self.entry_bb_id
    }

    pub fn get_inputs(&self, basic_blocks: &IndexVec<BasicBlockId, BasicBlock>) -> u32 {
        let inputs = basic_blocks[self.entry()].inputs.clone();
        inputs.end - inputs.start
    }

    pub fn get_outputs(&self) -> u32 {
        self.outputs
    }
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
    pub fn implied_fn_out(&self) -> Option<u32> {
        match self.control {
            Control::InternalReturn => Some(self.outputs.end - self.outputs.start),
            _ => None,
        }
    }

    pub fn fmt_display(
        &self,
        f: &mut impl fmt::Write,
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
            op.op_fmt(f, ir)?;
            writeln!(f)?;
        }

        // Display control flow
        match &self.control {
            Control::LastOpTerminates => {}
            _ => {
                write!(f, "        ")?;
                self.control.fmt_display(f, ir)?;
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

/// Values stored at `values_start_id..values_start_id + cases_count`, target basic block IDs stored
/// at `targets_start_id..targets_start_id + cases_count`.
#[derive(Debug, Clone)]
pub struct Cases {
    pub values_start_id: LargeConstId,
    pub targets_start_id: CasesBasicBlocksIndex,
    pub cases_count: u32,
}

impl Cases {
    pub fn get_values<'ir>(
        &'ir self,
        ir: &'ir EthIRProgram,
    ) -> &'ir IndexSlice<LargeConstId, [U256]> {
        &ir.large_consts[self.values_start_id..self.values_start_id + self.cases_count]
    }

    pub fn get_bb_ids<'ir>(
        &'ir self,
        ir: &'ir EthIRProgram,
    ) -> &'ir IndexSlice<CasesBasicBlocksIndex, [BasicBlockId]> {
        &ir.cases_bb_ids[self.targets_start_id..self.targets_start_id + self.cases_count]
    }
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
    pub fn iter_outgoing<'ir>(&'ir self, ir: &'ir EthIRProgram) -> OutgoingConnectionsIter<'ir> {
        use core::slice::from_ref;
        match self {
            Control::InternalReturn | Control::LastOpTerminates => OutgoingConnectionsIter::empty(),
            Control::ContinuesTo(bb_id) => OutgoingConnectionsIter::from_list(from_ref(bb_id)),
            Control::Branches(branch) => OutgoingConnectionsIter::new(
                from_ref(&branch.zero_target),
                Some(branch.non_zero_target),
            ),
            Control::Switch(switch) => OutgoingConnectionsIter::new(
                &ir.cases[switch.cases].get_bb_ids(ir).raw,
                switch.fallback,
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct OutgoingConnectionsIter<'ir> {
    extra_connection: Option<BasicBlockId>,
    connections_list: &'ir [BasicBlockId],
}

impl<'ir> OutgoingConnectionsIter<'ir> {
    fn empty() -> Self {
        Self { extra_connection: None, connections_list: &[] }
    }

    fn from_list(connections_list: &'ir [BasicBlockId]) -> Self {
        Self::new(connections_list, None)
    }

    fn new(connections_list: &'ir [BasicBlockId], extra_connection: Option<BasicBlockId>) -> Self {
        Self { extra_connection, connections_list }
    }
}

impl<'ir> Iterator for OutgoingConnectionsIter<'ir> {
    type Item = BasicBlockId;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(bb_id) = self.connections_list.split_off_first() {
            return Some(*bb_id);
        }

        self.extra_connection.take()
    }
}

pub trait IterIdx {
    type I: GudIndex;
    fn iter_idx(&self) -> IndexIter<Self::I>;
}

pub struct IndexIter<I: GudIndex> {
    current: I,
    end: I,
}

impl<I: GudIndex> IterIdx for Range<I> {
    type I = I;

    fn iter_idx(&self) -> IndexIter<Self::I> {
        IndexIter { current: self.start, end: self.end }
    }
}

impl<I: GudIndex> Iterator for IndexIter<I> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        (self.current < self.end).then(|| self.current.get_and_inc())
    }
}

impl Control {
    pub fn fmt_display(&self, f: &mut impl fmt::Write, ir: &EthIRProgram) -> fmt::Result {
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
                let cases = &ir.cases[switch.cases];
                for (value, target) in cases.get_values(ir).iter().zip(cases.get_bb_ids(ir)) {
                    writeln!(f, "{:x} => @{},", value, target)?;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_ir_display(program: &EthIRProgram, expected: &str) {
        let actual = display_program(program);
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

        // Create a simple program directly (old test)
        let program = EthIRProgram {
            init_entry: FunctionId::new(0),
            main_entry: None,
            functions: index_vec![Function::new(BasicBlockId::new(0), 1)],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(2),
                outputs: LocalIndex::from_usize(2)..LocalIndex::from_usize(3),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(2),
                control: Control::InternalReturn,
            }],
            operations: index_vec![
                Operation::Add(InlineOperands {
                    ins: [LocalId::new(0), LocalId::new(1)],
                    outs: [LocalId::new(2)],
                }),
                Operation::Stop(Default::default()),
            ],
            locals: index_vec![LocalId::new(0), LocalId::new(1), LocalId::new(2),],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
            cases_bb_ids: index_vec![],
        };

        let expected = r#"
Functions:
    fn @0 -> entry @0  (outputs: 1)

Basic Blocks:
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
                Function::new(BasicBlockId::new(0), 0),
                Function::new(BasicBlockId::new(2), 1)
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
                Operation::Stop(Default::default()),
                Operation::Invalid(Default::default()),
                Operation::SetCopy(InlineOperands {
                    ins: [LocalId::new(0)],
                    outs: [LocalId::new(1)]
                }),
                Operation::Stop(Default::default()),
            ],
            locals: index_vec![LocalId::new(0), LocalId::new(1),],
            data_segments_start: index_vec![],
            data_bytes: index_vec![],
            large_consts: index_vec![],
            cases: index_vec![],
            cases_bb_ids: index_vec![],
        };

        let expected = r#"
Functions:
    fn @0 -> entry @0  (outputs: 0)
    fn @1 -> entry @2  (outputs: 1)

Basic Blocks:
    @0 {
        stop
    }

    @1 {
        invalid
    }

    @2 $0 -> $1 {
        $1 = copy $0
        iret
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
            functions: index_vec![Function::new(BasicBlockId::new(0), 0)],
            basic_blocks: index_vec![BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(3),
                control: Control::LastOpTerminates,
            }],
            operations: index_vec![
                Operation::SetLargeConst(SetLargeConstData {
                    sets: LocalId::new(0),
                    value: LargeConstId::new(0),
                }),
                Operation::SetDataOffset(SetDataOffsetData {
                    sets: LocalId::new(1),
                    segment_id: DataId::new(1),
                }),
                Operation::Stop(Default::default()),
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
            cases_bb_ids: index_vec![],
        };

        let expected = r#"
Functions:
    fn @0 -> entry @0  (outputs: 0)

Basic Blocks:
    @0 {
        $0 = large_const 0xdeadbeef
        $1 = data_offset .1
        stop
    }


data .0 0x1234
data .1 0x56789abc
data .2 0xdef0
"#;

        assert_ir_display(&program, expected);
    }
}
