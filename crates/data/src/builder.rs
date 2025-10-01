use crate::{
    BasicBlock, BasicBlockId, Cases, CasesBasicBlocksIndex, CasesId, Control, DataId, DataOffset,
    EthIRProgram, Function, FunctionId, InternalCall, LargeConstId, LocalId, LocalIndex, Operation,
    OperationIndex,
};
use alloy_primitives::U256;
use index_vec::IndexVec;
use std::ops::Range;

#[derive(Debug)]
pub enum BuildError {
    InvalidCasesCount { expected: usize, got: usize },
    InvalidDataSegment { id: DataId, range: Range<DataOffset> },
    InvalidInternalCall { function: FunctionId, args: Vec<LocalId>, outputs: Vec<LocalId> },
    NestedFunctionBuilder,
    NestedBasicBlockBuilder,
}

pub struct EthIRBuilder {
    // IR Statements
    pub(crate) functions: IndexVec<FunctionId, Function>,
    pub(crate) basic_blocks: IndexVec<BasicBlockId, BasicBlock>,
    pub(crate) operations: IndexVec<OperationIndex, Operation>,
    pub(crate) data_segments_start: IndexVec<DataId, DataOffset>,

    // IR Data
    pub(crate) locals: IndexVec<LocalIndex, LocalId>,
    pub(crate) data_bytes: IndexVec<DataOffset, u8>,
    pub(crate) large_consts: IndexVec<LargeConstId, U256>,
    pub(crate) cases: IndexVec<CasesId, Cases>,
    pub(crate) cases_bb_ids: IndexVec<CasesBasicBlocksIndex, BasicBlockId>,
}

#[must_use]
pub struct FunctionBuilder<'a> {
    builder: &'a mut EthIRBuilder,
    function_id: FunctionId,
    first_block: BasicBlockId,
    last_block: Option<BasicBlockId>,
}

#[must_use]
pub struct BasicBlockBuilder<'func, 'builder: 'func> {
    fn_builder: &'func mut FunctionBuilder<'builder>,
    block_id: BasicBlockId,
    first_operation: OperationIndex,
    last_operation: Option<OperationIndex>,
    pub inputs: Range<LocalIndex>,
    pub outputs: Range<LocalIndex>,
}

impl EthIRBuilder {
    pub fn new() -> Self {
        Self {
            functions: IndexVec::new(),
            basic_blocks: IndexVec::new(),
            operations: IndexVec::new(),
            data_segments_start: IndexVec::new(),
            locals: IndexVec::new(),
            data_bytes: IndexVec::new(),
            large_consts: IndexVec::new(),
            cases: IndexVec::new(),
            cases_bb_ids: IndexVec::new(),
        }
    }

    pub fn build(
        self,
        init_entry: FunctionId,
        main_entry: Option<FunctionId>,
    ) -> Result<EthIRProgram, BuildError> {
        Ok(EthIRProgram {
            init_entry,
            main_entry,
            functions: self.functions,
            basic_blocks: self.basic_blocks,
            operations: self.operations,
            data_segments_start: self.data_segments_start,
            locals: self.locals,
            data_bytes: self.data_bytes,
            large_consts: self.large_consts,
            cases: self.cases,
            cases_bb_ids: self.cases_bb_ids,
        })
    }

    // Low-level push methods
    pub fn push_local(&mut self, local: LocalId) -> Result<LocalIndex, BuildError> {
        let index = self.locals.push(local);
        Ok(index)
    }

    pub fn next_local_index(&self) -> LocalIndex {
        self.locals.next_idx()
    }

    pub fn next_basic_block_id(&self) -> BasicBlockId {
        self.basic_blocks.next_idx()
    }

    pub fn locals_mut(&mut self) -> &mut IndexVec<LocalIndex, LocalId> {
        &mut self.locals
    }

    pub fn large_consts_mut(&mut self) -> &mut IndexVec<LargeConstId, U256> {
        &mut self.large_consts
    }

    pub fn locals_and_consts_mut(
        &mut self,
    ) -> (&mut IndexVec<LocalIndex, LocalId>, &mut IndexVec<LargeConstId, U256>) {
        (&mut self.locals, &mut self.large_consts)
    }

    pub fn push_large_const(&mut self, value: U256) -> Result<LargeConstId, BuildError> {
        let id = self.large_consts.push(value);
        Ok(id)
    }

    pub fn push_data_bytes(&mut self, bytes: &[u8]) -> Result<DataId, BuildError> {
        let start_offset = self.data_bytes.next_idx();
        self.data_bytes.extend(bytes.iter().copied());
        let id = self.data_segments_start.push(start_offset);
        Ok(id)
    }

    pub fn push_operation(&mut self, op: Operation) -> Result<OperationIndex, BuildError> {
        let index = self.operations.push(op);
        Ok(index)
    }

    pub fn push_case(
        &mut self,
        values: &[U256],
        targets: &[BasicBlockId],
    ) -> Result<CasesId, BuildError> {
        if values.len() != targets.len() {
            return Err(BuildError::InvalidCasesCount {
                expected: values.len(),
                got: targets.len(),
            });
        }

        let values_start_id = self.large_consts.next_idx();
        for value in values {
            self.large_consts.push(*value);
        }

        let targets_start_id = self.cases_bb_ids.next_idx();
        for target in targets {
            self.cases_bb_ids.push(*target);
        }

        let case = Cases { values_start_id, targets_start_id, cases_count: values.len() as u32 };

        let id = self.cases.push(case);
        Ok(id)
    }

    // Function builder
    pub fn begin_function(&mut self) -> Result<FunctionBuilder<'_>, BuildError> {
        if self.current_function.is_some() {
            return Err(BuildError::NestedFunctionBuilder);
        }

        let first_block = self.basic_blocks.next_idx();

        let function_id = self.functions.push(Function {
            basic_blocks: first_block..first_block,
            outputs: 0, // Will be set when finish is called
        });

        self.current_function = Some(function_id);

        Ok(FunctionBuilder { builder: self, function_id, first_block, last_block: None })
    }

    // Basic block builder
    pub fn begin_basic_block(
        &mut self,
        inputs: Range<LocalIndex>,
        outputs: Range<LocalIndex>,
    ) -> Result<BasicBlockBuilder<'_>, BuildError> {
        if self.current_basic_block.is_some() {
            return Err(BuildError::NestedBasicBlockBuilder);
        }

        // Validate ranges
        if inputs.start > inputs.end || inputs.end > self.locals.next_idx() {
            return Err(BuildError::InvalidLocalRange { range: inputs.clone() });
        }
        if outputs.start > outputs.end || outputs.end > self.locals.next_idx() {
            return Err(BuildError::InvalidLocalRange { range: outputs.clone() });
        }

        let first_operation = self.operations.next_idx();

        let block_id = self.basic_blocks.push(BasicBlock {
            inputs: inputs.clone(),
            outputs: outputs.clone(),
            operations: first_operation..first_operation,
            control: Control::LastOpTerminates, // Placeholder, will be set when finish is called
        });

        self.current_basic_block = Some(block_id);

        Ok(BasicBlockBuilder {
            builder: self,
            block_id,
            first_operation,
            last_operation: None,
            inputs,
            outputs,
        })
    }

    // Convenience API
    pub fn append_internal_call(
        &mut self,
        function: FunctionId,
        args: &[LocalId],
        outputs: &[LocalId],
    ) -> Result<OperationIndex, BuildError> {
        // Validate function exists
        if self.functions.get(function).is_none() {
            return Err(BuildError::InvalidInternalCall {
                function,
                args: args.to_vec(),
                outputs: outputs.to_vec(),
            });
        }

        // Add arguments to locals
        let args_start = self.locals.next_idx();
        for arg in args {
            self.locals.push(*arg);
        }

        // Add outputs to locals
        let outputs_start = self.locals.next_idx();
        for output in outputs {
            self.locals.push(*output);
        }

        // Create the internal call operation
        let op = Operation::InternalCall(InternalCall { function, args_start, outputs_start });

        self.push_operation(op)
    }
}

impl<'a> FunctionBuilder<'a> {
    pub fn add_basic_block(&mut self) -> Result<BasicBlockBuilder<'_>, BuildError> {
        // Create a basic block through the main builder with empty ranges
        // The parser will set these appropriately
        let inputs = self.builder.locals.next_idx()..self.builder.locals.next_idx();
        let outputs = self.builder.locals.next_idx()..self.builder.locals.next_idx();

        let block_builder = self.builder.begin_basic_block(inputs, outputs)?;

        // Track the last block for this function
        self.last_block = Some(block_builder.block_id);

        Ok(block_builder)
    }

    pub fn finish(self, outputs: u32) -> Result<FunctionId, BuildError> {
        // Update the function with the final outputs and basic block range
        let end_block = if let Some(_last) = self.last_block {
            self.builder.basic_blocks.next_idx()
        } else {
            self.first_block
        };

        self.builder.functions[self.function_id] =
            Function { basic_blocks: self.first_block..end_block, outputs };

        // Clear the current function
        self.builder.current_function = None;

        Ok(self.function_id)
    }
}

impl<'a> BasicBlockBuilder<'a> {
    pub fn add_operation(&mut self, op: Operation) -> Result<OperationIndex, BuildError> {
        let index = self.builder.operations.push(op);
        self.last_operation = Some(index);
        Ok(index)
    }

    pub fn add_operations<I>(&mut self, ops: I) -> Result<Range<OperationIndex>, BuildError>
    where
        I: IntoIterator<Item = Operation>,
    {
        let start = self.builder.operations.next_idx();
        for op in ops {
            let index = self.builder.operations.push(op);
            self.last_operation = Some(index);
        }
        let end = self.builder.operations.next_idx();
        Ok(start..end)
    }

    pub fn finish(self, control: Control) -> Result<BasicBlockId, BuildError> {
        // Update the basic block with the final control and operation range
        let end_operation = if self.last_operation.is_some() {
            self.builder.operations.next_idx()
        } else {
            self.first_operation
        };

        self.builder.basic_blocks[self.block_id] = BasicBlock {
            inputs: self.inputs,
            outputs: self.outputs,
            operations: self.first_operation..end_operation,
            control,
        };

        // Clear the current basic block
        self.builder.current_basic_block = None;

        Ok(self.block_id)
    }
}

impl Default for EthIRBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::operation;

    #[test]
    fn test_simple_function() {
        let mut builder = EthIRBuilder::new();

        // Create some locals
        let _l0 = builder.push_local(LocalId::new(0)).unwrap();
        let _l1 = builder.push_local(LocalId::new(1)).unwrap();

        // Create a function
        let mut func = builder.begin_function().unwrap();

        // Add a basic block
        let mut bb = func.add_basic_block().unwrap();
        bb.add_operation(Operation::LocalSetSmallConst(operation::SetSmallConst {
            local: LocalId::new(0),
            value: 42,
        }))
        .unwrap();
        bb.finish(Control::InternalReturn).unwrap();

        // Finish the function
        let func_id = func.finish(1).unwrap();

        // Set as init entry
        builder.set_init_entry(func_id);

        // Build the program
        let program = builder.build().unwrap();

        assert_eq!(program.init_entry, func_id);
        assert_eq!(program.functions[func_id].outputs, 1);
        assert_eq!(program.operations.len(), 1);
    }

    #[test]
    fn test_validation_errors() {
        let builder = EthIRBuilder::new();

        // Try to build without init entry
        assert!(matches!(builder.build(), Err(BuildError::NoEntryFunction)));
    }

    #[test]
    fn test_nested_builder_error() {
        let mut builder = EthIRBuilder::new();

        let _func1 = builder.begin_function().unwrap();

        // Try to start another function without finishing the first
        let result = builder.begin_function();
        assert!(matches!(result, Err(BuildError::NestedFunctionBuilder)));
    }
}
