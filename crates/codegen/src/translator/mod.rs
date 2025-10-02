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

    pub const CALL_ARG_COUNT: usize = 7; // gas, address, value, argsOffset, argsSize, retOffset, retSize
    pub const CALLCODE_ARG_COUNT: usize = 7; // gas, address, value, argsOffset, argsSize, retOffset, retSize
    pub const DELEGATECALL_ARG_COUNT: usize = 6; // gas, address, argsOffset, argsSize, retOffset, retSize
    pub const STATICCALL_ARG_COUNT: usize = 6; // gas, address, argsOffset, argsSize, retOffset, retSize
    pub const CREATE_ARG_COUNT: usize = 3; // value, offset, size
    pub const CREATE2_ARG_COUNT: usize = 4; // value, offset, size, salt
}

use eth_ir_data::{BasicBlockId, DataId, EthIRProgram, Idx, IndexVec, LocalId, index_vec};
use evm_glue::assembly::Asm;
use locals::LocalStorage;
use marks::{MarkAllocator, MarkId};

/// Mutable translation state
pub(crate) struct TranslationState {
    pub(crate) locals: LocalStorage,
    pub(crate) marks: MarkAllocator,
    pub(crate) translated_blocks: IndexVec<BasicBlockId, bool>,
    pub(crate) asm: Vec<Asm>,
    pub(crate) init_end_mark: MarkId,
    pub(crate) runtime_start_mark: MarkId,
    pub(crate) runtime_end_mark: MarkId,
    pub(crate) data_end_mark: MarkId,
    pub(crate) data_marks: IndexVec<DataId, MarkId>,
    pub(crate) is_translating_init: bool,
}

/// Main translator from IR to EVM assembly
pub struct Translator {
    pub(crate) program: EthIRProgram,
    pub(crate) state: TranslationState,
    pub(crate) config: crate::Config,
}

impl Translator {
    /// Create a new translator for the given IR program with default configuration
    pub fn new(program: EthIRProgram) -> Self {
        Self::with_config(program, crate::Config::default())
    }

    /// Create a new translator for the given IR program with custom configuration
    pub fn with_config(program: EthIRProgram, config: crate::Config) -> Self {
        let mut marks = MarkAllocator::new();

        let init_end_mark = marks.allocate_mark();
        let runtime_start_mark = init_end_mark; // Runtime immediately follows init
        let runtime_end_mark = marks.allocate_mark();
        let data_end_mark = marks.allocate_mark();

        // Estimate initial capacity for assembly based on program size
        let estimated_asm_size = program.operations.len()
            * constants::ASM_INSTRUCTIONS_PER_OPERATION
            + constants::ASM_INITIALIZATION_OVERHEAD;
        let blocks_count = program.basic_blocks.len();

        let starting_locals_capacity = program.next_free_local_id.index();

        let locals = LocalStorage::with_capacity(starting_locals_capacity);

        let state = TranslationState {
            locals,
            marks,
            translated_blocks: index_vec![false; blocks_count],
            asm: Vec::with_capacity(estimated_asm_size),
            init_end_mark,
            runtime_start_mark,
            runtime_end_mark,
            data_end_mark,
            data_marks: IndexVec::new(),
            is_translating_init: false,
        };

        Self { program, state, config }
    }

    /// Translate the IR program to EVM assembly
    pub fn translate(&mut self) {
        self.allocate_all_locals();
        // Finalize local allocations to fix free memory pointer location
        self.state.locals.finalize_allocations();
        self.allocate_data_marks();
        self.generate_init_code();
        self.state.asm.push(Asm::Mark(self.state.init_end_mark));
        self.generate_runtime_code();
        self.state.asm.push(Asm::Mark(self.state.runtime_end_mark));

        if self.config.panic_on_untranslated_blocks {
            let all_translated = self.state.translated_blocks.iter().all(|b| *b);
            assert!(all_translated, "Untranslated blocks found (unreachable/dead code)");
        }

        // Embed data segments
        self.embed_data_segments();
        self.state.asm.push(Asm::Mark(self.state.data_end_mark));
    }

    fn allocate_data_marks(&mut self) {
        for _ in 0..self.program.data_segments_start.len() {
            let mark = self.state.marks.allocate_mark();
            self.state.data_marks.push(mark);
        }
    }

    fn allocate_all_locals(&mut self) {
        use eth_ir_data::RangeExt;
        for local_id in (LocalId::new(0)..self.program.next_free_local_id).iter() {
            self.state.locals.allocate(local_id);
        }
    }

    /// Get the generated assembly
    pub fn into_asm(self) -> Vec<Asm> {
        self.state.asm
    }
}

/// High-level function to translate an IR program to EVM assembly
pub fn translate_program(program: EthIRProgram) -> Vec<Asm> {
    let mut translator = Translator::new(program);
    translator.translate();
    translator.into_asm()
}
