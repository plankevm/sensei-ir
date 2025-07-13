pub mod index;
pub mod operation;

pub use crate::{index::*, operation::Operation};
use alloy_primitives::U256;
use std::ops::Range;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn control_memory_layout() {
        assert_eq!(std::mem::size_of::<Control>(), 16, "changed desired control size");
        assert_eq!(std::mem::align_of::<Control>(), 4, "changed desired control alignment");
    }
}
