//! Initialization and deployment code generation

use super::Translator;
use crate::error::Result;
use alloy_primitives::U256;
use eth_ir_data::Idx;
use evm_glue::assembly::{Asm, MarkRef, RefType};
use smallvec::SmallVec;

impl Translator {
    /// Generate init code
    pub(super) fn generate_init_code(&mut self) -> Result<()> {
        // Initialize EVM state
        self.emit_initialization();

        // Set flag to track we're translating init code
        self.is_translating_init = true;

        // Translate init_entry function
        debug_assert!(
            self.program.init_entry.index() < self.program.functions.len(),
            "Invalid init entry function reference: {:?}",
            self.program.init_entry
        );
        let init_entry_block = self.program.functions[self.program.init_entry].entry;
        let init_func_mark = self.marks.get_function_mark(self.program.init_entry);
        self.emit_mark(init_func_mark);
        self.translate_block(init_entry_block)?;

        // Clear the init translation flag
        self.is_translating_init = false;

        // Always add deployment return if init code didn't have its own RETURN
        // This ensures the runtime code is deployed properly
        if !self.init_has_return {
            self.emit_deployment_return();
        }

        Ok(())
    }

    /// Generate runtime code
    pub(super) fn generate_runtime_code(&mut self) -> Result<()> {
        // If there's a main entry, translate it
        if let Some(main_entry) = self.program.main_entry {
            debug_assert!(
                main_entry.index() < self.program.functions.len(),
                "Invalid main entry function reference: {:?}",
                main_entry
            );
            let main_entry_block = self.program.functions[main_entry].entry;
            let main_mark = self.marks.get_function_mark(main_entry);
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
        self.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Direct(self.runtime_start_mark),
            is_pushed: true,
            set_size: None,
        }));

        // PUSH size (runtime + data length)
        // Calculate the actual size from runtime_start to data_end
        self.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Delta(self.data_end_mark, self.runtime_start_mark),
            is_pushed: true,
            set_size: None,
        }));

        // CODECOPY
        self.asm.push(Asm::Op(Opcode::CODECOPY));

        // PUSH 0 (memory offset for return)
        self.push_const(U256::from(0));

        // PUSH size again for RETURN
        self.asm.push(Asm::Ref(MarkRef {
            ref_type: RefType::Delta(self.data_end_mark, self.runtime_start_mark),
            is_pushed: true,
            set_size: None,
        }));

        // RETURN
        self.asm.push(Asm::Op(Opcode::RETURN));
    }

    /// Embed data segments at the end of bytecode
    pub(super) fn embed_data_segments(&mut self) {
        for (segment_id, _) in self.program.data_segments_start.iter_enumerated() {
            // Place mark for this segment
            if let Some(&mark) = self.data_marks.get(&segment_id) {
                self.asm.push(Asm::Mark(mark));
            }

            // Get segment bytes and embed as raw data
            let range = self.program.get_segment_range(segment_id);
            let segment_size = (range.end.get() - range.start.get()) as usize;

            if segment_size > 0 {
                // Use SmallVec for small segments (< 256 bytes), Vec for larger
                if segment_size <= 256 {
                    let mut bytes: SmallVec<[u8; 256]> = SmallVec::with_capacity(segment_size);
                    for i in range.start.get()..range.end.get() {
                        bytes.push(self.program.data_bytes[eth_ir_data::DataOffset::new(i)]);
                    }
                    self.asm.push(Asm::Data(bytes.to_vec()));
                } else {
                    let mut bytes = Vec::with_capacity(segment_size);
                    for i in range.start.get()..range.end.get() {
                        bytes.push(self.program.data_bytes[eth_ir_data::DataOffset::new(i)]);
                    }
                    self.asm.push(Asm::Data(bytes));
                }
            }
        }
    }

    /// Emit initialization code for the EVM
    pub(super) fn emit_initialization(&mut self) {
        use evm_glue::opcodes::Opcode;

        // Set up the free memory pointer at 0x40
        // The free memory pointer points to the start of free memory
        // We set it to point just after our locals area

        // Get the actual calculated free memory start address
        let free_mem_start = self.memory.get_free_memory_start();

        // PUSH free_mem_start
        self.push_const(U256::from(free_mem_start));

        // PUSH 0x40 (free memory pointer location)
        self.push_const(U256::from(super::memory::constants::FREE_MEM_PTR));

        // MSTORE
        self.asm.push(Asm::Op(Opcode::MSTORE));
    }
}
