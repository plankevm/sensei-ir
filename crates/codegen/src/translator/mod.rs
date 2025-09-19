//! Translator from eth-ir to EVM assembly

pub mod marks;
pub mod memory;

mod control_flow;
mod helpers;
mod initialization;
mod operations;

/// Common constants used throughout the translator
mod constants {
    /// Size threshold for using SmallVec vs Vec for data segments
    pub const SMALL_DATA_SEGMENT_SIZE: usize = 256;

    /// Capacity hint for SmallVec when handling data segments
    pub const SMALL_DATA_SEGMENT_CAPACITY: usize = 256;

    /// EVM word size for memory operations (32 bytes)
    pub const EVM_WORD_SIZE: usize = 32;

    /// Initial capacity estimate multiplier for assembly instructions
    /// Most operations translate to approximately this many assembly instructions
    pub const ASM_INSTRUCTIONS_PER_OPERATION: usize = 4;

    /// Additional assembly instructions for initialization and control flow
    pub const ASM_INITIALIZATION_OVERHEAD: usize = 100;
}

use crate::error::Result;
use eth_ir_data::{BasicBlockId, DataId, EthIRProgram, Idx};
use evm_glue::assembly::Asm;
use marks::{MarkAllocator, MarkId};
use memory::MemoryLayout;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

/// Immutable program data that can be safely shared
pub(crate) struct ProgramData {
    /// The IR program being translated
    pub(crate) program: EthIRProgram,
}

/// Mutable translation state
pub(crate) struct TranslationState {
    /// Memory layout for locals
    pub(crate) memory: MemoryLayout,

    /// Mark allocator for jump labels
    pub(crate) marks: MarkAllocator,

    /// Tracks which blocks have been translated
    pub(crate) translated_blocks: HashSet<BasicBlockId>,

    /// Generated assembly instructions
    pub(crate) asm: Vec<Asm>,

    /// Section marks for bytecode layout
    pub(crate) init_end_mark: MarkId,
    pub(crate) runtime_start_mark: MarkId,
    pub(crate) runtime_end_mark: MarkId,
    pub(crate) data_end_mark: MarkId,

    /// Marks for data segments
    pub(crate) data_marks: HashMap<DataId, MarkId>,

    /// Track if we're currently translating init code
    pub(crate) is_translating_init: bool,

    /// Track if we've emitted a RETURN during init code generation
    pub(crate) init_has_return: bool,
}

/// Main translator from IR to EVM assembly
pub struct Translator {
    /// Immutable program data (cheap to clone with Arc)
    pub(crate) program: Arc<ProgramData>,

    /// Mutable translation state
    pub(crate) state: TranslationState,
}

impl Translator {
    /// Create a new translator for the given IR program
    pub fn new(program: EthIRProgram) -> Self {
        let mut marks = MarkAllocator::new();

        // Pre-allocate section marks
        let init_end_mark = marks.allocate_mark();
        let runtime_start_mark = init_end_mark; // Runtime immediately follows init
        let runtime_end_mark = marks.allocate_mark();
        let data_end_mark = marks.allocate_mark();

        // Estimate initial capacity for assembly based on program size
        let estimated_asm_size = program.operations.len()
            * constants::ASM_INSTRUCTIONS_PER_OPERATION
            + constants::ASM_INITIALIZATION_OVERHEAD;
        let blocks_count = program.basic_blocks.len();
        let data_segments_count = program.data_segments_start.len();

        let program_data = Arc::new(ProgramData { program });

        let state = TranslationState {
            memory: MemoryLayout::new(),
            marks,
            // Pre-allocate based on basic blocks count since we track which blocks are translated
            translated_blocks: HashSet::with_capacity(blocks_count),
            asm: Vec::with_capacity(estimated_asm_size),
            init_end_mark,
            runtime_start_mark,
            runtime_end_mark,
            data_end_mark,
            // Pre-allocate based on data segments count
            data_marks: HashMap::with_capacity(data_segments_count),
            is_translating_init: false,
            init_has_return: false,
        };

        Self { program: program_data, state }
    }

    /// Translate the IR program to EVM assembly
    pub fn translate(&mut self) -> Result<()> {
        // Pre-allocate memory for all locals
        self.allocate_all_locals();

        // Pre-allocate marks for data segments
        for (segment_id, _) in self.program.program.data_segments_start.iter_enumerated() {
            let mark = self.state.marks.allocate_mark();
            self.state.data_marks.insert(segment_id, mark);
        }

        // Generate init code first
        self.generate_init_code()?;

        // Mark where init ends and runtime begins
        self.state.asm.push(Asm::Mark(self.state.init_end_mark));

        // Generate runtime code
        self.generate_runtime_code()?;

        // Mark where runtime ends
        self.state.asm.push(Asm::Mark(self.state.runtime_end_mark));

        // Translate any remaining untranslated blocks (e.g., from internal calls)
        for block_idx in 0..self.program.program.basic_blocks.len() {
            let block_id = BasicBlockId::from_usize(block_idx);
            if !self.state.translated_blocks.contains(&block_id) {
                self.translate_block(block_id)?;
            }
        }

        // Embed data segments
        self.embed_data_segments();

        // Mark the end of all data
        self.state.asm.push(Asm::Mark(self.state.data_end_mark));

        Ok(())
    }

    /// Pre-allocate memory for all locals in the program
    fn allocate_all_locals(&mut self) {
        // The program.locals is an arena/pool of LocalId references
        // Multiple entries might reference the same LocalId
        // We need to allocate memory for each UNIQUE LocalId

        // Collect unique LocalIds
        // Pre-allocate based on locals count (worst case: all are unique)
        let mut seen = HashSet::with_capacity(self.program.program.locals.len());
        for local_id in self.program.program.locals.iter() {
            if seen.insert(*local_id) {
                // First time seeing this LocalId, allocate memory for it
                self.state.memory.allocate_local(*local_id);
            }
        }
    }

    /// Get the generated assembly
    pub fn into_asm(self) -> Vec<Asm> {
        self.state.asm
    }

    /// Get immutable access to the program
    pub(crate) fn program(&self) -> &EthIRProgram {
        &self.program.program
    }
}

/// High-level function to translate an IR program to EVM assembly
pub fn translate_program(program: EthIRProgram) -> Result<Vec<Asm>> {
    let mut translator = Translator::new(program);
    translator.translate()?;
    Ok(translator.into_asm())
}
