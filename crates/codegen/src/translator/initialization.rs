//! Initialization and deployment code generation

use super::Translator;
use crate::error::{CodegenError, Result};
use alloy_primitives::U256;
use eth_ir_data::Idx;
use evm_glue::assembly::{Asm, MarkRef, RefType};

impl Translator {
    /// Generate init code
    pub(super) fn generate_init_code(&mut self) -> Result<()> {
        // Initialize EVM state
        self.emit_initialization();

        // Set flag to track we're translating init code
        self.state.is_translating_init = true;

        // Translate init_entry function
        // Validate init entry function exists
        if self.program.program.init_entry.index() >= self.program.program.functions.len() {
            return Err(CodegenError::InvalidFunctionReference {
                function: self.program.program.init_entry,
            });
        }
        let init_entry_block =
            self.program.program.functions[self.program.program.init_entry].entry;
        let init_func_mark = self.state.marks.get_function_mark(self.program.program.init_entry);
        self.emit_mark(init_func_mark);
        self.translate_block(init_entry_block)?;

        // Clear the init translation flag
        self.state.is_translating_init = false;

        // Always add deployment return if init code didn't have its own RETURN
        // This ensures the runtime code is deployed properly
        if !self.state.init_has_return {
            self.emit_deployment_return();
        }

        Ok(())
    }

    /// Generate runtime code
    pub(super) fn generate_runtime_code(&mut self) -> Result<()> {
        // If there's a main entry, translate it
        if let Some(main_entry) = self.program.program.main_entry {
            // Validate main entry function exists
            if main_entry.index() >= self.program.program.functions.len() {
                return Err(CodegenError::InvalidFunctionReference { function: main_entry });
            }
            let main_entry_block = self.program.program.functions[main_entry].entry;
            let main_mark = self.state.marks.get_function_mark(main_entry);
            self.emit_mark(main_mark);
            self.translate_block(main_entry_block)?;
        }
        Ok(())
    }

    /// Emit deployment return (copies runtime+data and returns it)
    pub(super) fn emit_deployment_return(&mut self) {
        use evm_glue::opcodes::Opcode;

        // PUSH 0 (memory destination)
        self.push_const(U256::from(0));

        // PUSH runtime_start (source in code)
        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(self.state.runtime_start_mark),
            is_pushed: true,
            set_size: None,
        }));

        // PUSH size (runtime + data length)
        // Calculate the actual size from runtime_start to data_end
        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Delta(self.state.data_end_mark, self.state.runtime_start_mark),
            is_pushed: true,
            set_size: None,
        }));

        // CODECOPY
        self.state.asm.push(Asm::Op(Opcode::CODECOPY));

        // PUSH 0 (memory offset for return)
        self.push_const(U256::from(0));

        // PUSH size again for RETURN
        self.state.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Delta(self.state.data_end_mark, self.state.runtime_start_mark),
            is_pushed: true,
            set_size: None,
        }));

        // RETURN
        self.state.asm.push(Asm::Op(Opcode::RETURN));
    }

    /// Embed data segments at the end of bytecode
    pub(super) fn embed_data_segments(&mut self) {
        for (segment_id, _) in self.program.program.data_segments_start.iter_enumerated() {
            // Place mark for this segment
            if let Some(&mark) = self.state.data_marks.get(&segment_id) {
                self.state.asm.push(Asm::Mark(mark));
            }

            // Get segment bytes and embed as raw data
            let range = self.program.program.get_segment_range(segment_id);
            let segment_size = (range.end.get() - range.start.get()) as usize;

            if segment_size > 0 {
                // Collect bytes from the segment
                let mut bytes = Vec::with_capacity(segment_size);
                for i in range.start.get()..range.end.get() {
                    bytes.push(self.program.program.data_bytes[eth_ir_data::DataOffset::new(i)]);
                }
                self.state.asm.push(Asm::Data(bytes));
            }
        }
    }

    /// Emit initialization code for the EVM
    pub(super) fn emit_initialization(&mut self) {
        use evm_glue::opcodes::Opcode;

        // Set up the free memory pointer
        if let Some(free_mem_ptr_loc) = self.state.locals.get_free_memory_pointer_location() {
            // Get the initial value for dynamic memory allocations
            let initial_free_mem = self.state.locals.get_initial_free_memory_value();

            // Store initial_free_mem at free_mem_ptr_loc
            self.push_const(U256::from(initial_free_mem));
            self.push_const(U256::from(free_mem_ptr_loc));
            self.state.asm.push(Asm::Op(Opcode::MSTORE));
        }
    }
}
