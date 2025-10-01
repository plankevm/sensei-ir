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

    /// Maximum safe memory size (to prevent overflow)
    /// Set to 2^32 - 1 to fit in u32 addresses
    pub const MAX_MEMORY_SIZE: usize = 0xFFFFFFFF;

    /// Maximum reasonable allocation size (1MB)
    /// Prevents excessive memory usage and potential DOS
    pub const MAX_ALLOCATION_SIZE: u32 = 0x100000;

    /// EVM scratch space end address
    /// Memory from 0x00 to 0x7F is reserved for EVM operations
    pub const EVM_SCRATCH_SPACE_END: u32 = 0x80;

    /// Maximum number of switch cases to process per batch
    /// Limited by EVM's DUP16 constraint - we DUP1 for each case comparison
    /// Set to 14 to leave room for other stack operations
    pub const MAX_SWITCH_CASES_PER_BATCH: usize = 14;
}

use crate::error::Result;
use eth_ir_data::{BasicBlockId, DataId, EthIRProgram, Idx};
use evm_glue::assembly::Asm;
use locals::LocalStorage;
use marks::{MarkAllocator, MarkId};
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
    /// Storage strategy for locals (stack vs memory)
    pub(crate) locals: LocalStorage,

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

    /// Track whether we should emit bounds checking (can be disabled for optimization)
    pub(crate) enable_bounds_checking: bool,

    /// Track whether we should emit debug assertions
    pub(crate) enable_debug_assertions: bool,

    /// Track the last loaded local to avoid redundant loads
    /// Maps LocalId to the position where it's on the stack
    pub(crate) last_loaded: Option<eth_ir_data::LocalId>,
}

/// Configuration for the translator
pub struct TranslatorConfig {
    /// Enable bounds checking for memory operations
    pub enable_bounds_checking: bool,
    /// Enable debug assertions
    pub enable_debug_assertions: bool,
}

impl Default for TranslatorConfig {
    fn default() -> Self {
        Self { enable_bounds_checking: false, enable_debug_assertions: cfg!(debug_assertions) }
    }
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
        Self::with_config(program, TranslatorConfig::default())
    }

    /// Create a new translator with custom options (legacy interface)
    pub fn with_options(
        program: EthIRProgram,
        enable_bounds_checking: bool,
        enable_debug_assertions: bool,
    ) -> Self {
        let config = TranslatorConfig { enable_bounds_checking, enable_debug_assertions };
        Self::with_config(program, config)
    }

    /// Create a new translator with full configuration
    pub fn with_config(program: EthIRProgram, config: TranslatorConfig) -> Self {
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
            enable_bounds_checking: config.enable_bounds_checking,
            enable_debug_assertions: config.enable_debug_assertions,
            last_loaded: None,
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

    /// Pre-allocate storage for all locals in the program
    fn allocate_all_locals(&mut self) {
        // The program.locals is an arena/pool of LocalId references
        // Multiple entries might reference the same LocalId
        // We need to allocate storage for each UNIQUE LocalId

        // Collect unique LocalIds
        // Pre-allocate based on locals count (worst case: all are unique)
        let mut seen = HashSet::with_capacity(self.program.program.locals.len());
        for local_id in self.program.program.locals.iter() {
            if seen.insert(*local_id) {
                // First time seeing this LocalId, allocate storage for it
                // The LocalStorage module decides whether to use stack or memory
                self.state.locals.allocate(*local_id);
            }
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
