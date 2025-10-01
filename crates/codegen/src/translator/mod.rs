//! Translator from eth-ir to EVM assembly

mod locals;
mod marks;

mod control_flow;
mod helpers;
mod initialization;
mod operations;

/// Common constants used throughout the translator
mod constants {

    /// Initial capacity estimate multiplier for assembly instructions
    /// Most operations translate to approximately this many assembly instructions
    pub const ASM_INSTRUCTIONS_PER_OPERATION: usize = 4;

    /// Additional assembly instructions for initialization and control flow
    pub const ASM_INITIALIZATION_OVERHEAD: usize = 100;

    /// EVM word size in bytes
    pub const EVM_WORD_SIZE: usize = 32;

    /// EVM scratch space end address
    /// Memory from 0x00 to 0x7F is reserved for EVM operations
    pub const EVM_SCRATCH_SPACE_END: u32 = 0x80;

    /// Maximum number of switch cases to process per batch
    /// Limited by EVM's DUP16 constraint - we DUP1 for each case comparison
    /// Set to 14 to leave room for other stack operations
    pub const MAX_SWITCH_CASES_PER_BATCH: usize = 14;

    /// Out-of-bounds offset for CALLDATACOPY to write zeros
    /// This value (4GB) is impossible to reach as calldata due to gas limits.
    /// When CALLDATACOPY's dataOffset >= CALLDATASIZE, it writes zeros (EVM spec).
    pub const CALLDATACOPY_ZERO_OFFSET: u32 = 0xFFFFFFFF;
}

use crate::error::Result;
use eth_ir_data::{BasicBlockId, DataId, EthIRProgram, Idx};
use evm_glue::assembly::Asm;
use locals::LocalStorage;
use marks::{MarkAllocator, MarkId};
use std::{collections::HashMap, sync::Arc};

/// Immutable program data that can be safely shared
pub(crate) struct ProgramData {
    /// The IR program being translated
    pub(crate) program: EthIRProgram,
}

/// Mutable translation state
pub(crate) struct TranslationState {
    /// Storage strategy for locals (stack vs memory)
    pub(crate) locals: LocalStorage,

    /// Mark allocator for jump labels
    pub(crate) marks: MarkAllocator,

    /// Tracks which blocks have been translated (indexed by BasicBlockId)
    pub(crate) translated_blocks: Vec<bool>,

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

        // Find the maximum LocalId to pre-allocate the locals vector
        let max_local_id =
            program.locals.iter().map(|local_id| local_id.index()).max().unwrap_or(0);

        let program_data = Arc::new(ProgramData { program });

        // Pre-allocate locals storage with the right capacity
        let locals = if max_local_id > 0 {
            LocalStorage::with_capacity(max_local_id + 1)
        } else {
            LocalStorage::new()
        };

        let state = TranslationState {
            locals,
            marks,
            // Pre-allocate vec of bools for tracking translated blocks (dense indexing)
            translated_blocks: vec![false; blocks_count],
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
            if !self.state.translated_blocks[block_idx] {
                let block_id = BasicBlockId::from_usize(block_idx);
                self.translate_block(block_id)?;
            }
        }

        // Embed data segments
        self.embed_data_segments();

        // Mark the end of all data
        self.state.asm.push(Asm::Mark(self.state.data_end_mark));

        Ok(())
    }

    /// Pre-allocate storage for all locals in the program
    fn allocate_all_locals(&mut self) {
        // The program.locals is an arena/pool of LocalId references
        // Multiple entries might reference the same LocalId
        // We need to allocate storage for each UNIQUE LocalId
        // LocalStorage::allocate() handles duplicates internally by checking
        // if a local is already allocated before creating new storage
        for local_id in self.program.program.locals.iter() {
            self.state.locals.allocate(*local_id);
        }
    }

    /// Get the generated assembly
    pub fn into_asm(self) -> Vec<Asm> {
        self.state.asm
    }
}

/// High-level function to translate an IR program to EVM assembly
pub fn translate_program(program: EthIRProgram) -> Result<Vec<Asm>> {
    let mut translator = Translator::new(program);
    translator.translate()?;
    Ok(translator.into_asm())
}
