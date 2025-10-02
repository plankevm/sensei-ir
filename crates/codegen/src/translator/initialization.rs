//! Initialization and deployment code generation

use super::Translator;

use alloy_primitives::U256;
use evm_glue::assembly::Asm;

impl Translator {
    /// Generate init code
    pub(super) fn generate_init_code(&mut self) {
        self.emit_initialization();

        self.state.is_translating_init = true;

        let init_entry_block = self.program.functions[self.program.init_entry].entry;
        let init_func_mark = self.state.marks.get_function_mark(self.program.init_entry);
        self.emit_mark(init_func_mark);
        self.translate_block(init_entry_block);

        self.state.is_translating_init = false;
    }

    /// Generate runtime code
    pub(super) fn generate_runtime_code(&mut self) {
        if let Some(main_entry) = self.program.main_entry {
            let main_entry_block = self.program.functions[main_entry].entry;
            let main_mark = self.state.marks.get_function_mark(main_entry);
            self.emit_mark(main_mark);
            self.translate_block(main_entry_block);
        }
    }

    /// Embed data segments at the end of bytecode
    pub(super) fn embed_data_segments(&mut self) {
        for (segment_id, _) in self.program.data_segments_start.iter_enumerated() {
            if let Some(&mark) = self.state.data_marks.get(segment_id) {
                self.state.asm.push(Asm::Mark(mark));
            }

            let range = self.program.get_segment_range(segment_id);
            let segment_size = (range.end.get() - range.start.get()) as usize;

            if segment_size > 0 {
                let mut bytes = Vec::with_capacity(segment_size);
                for i in range.start.get()..range.end.get() {
                    bytes.push(self.program.data_bytes[eth_ir_data::DataOffset::new(i)]);
                }
                self.state.asm.push(Asm::Data(bytes));
            }
        }
    }

    /// Emit initialization code for the EVM
    pub(super) fn emit_initialization(&mut self) {
        use evm_glue::opcodes::Opcode;

        let free_mem_ptr_loc = self.state.locals.get_free_memory_pointer_location();
        let initial_free_mem = self.state.locals.get_initial_free_memory_value();

        self.push_const(U256::from(initial_free_mem));
        self.push_const(U256::from(free_mem_ptr_loc));
        self.state.asm.push(Asm::Op(Opcode::MSTORE));
    }
}
