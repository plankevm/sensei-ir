use super::constants;
use crate::translate_program;
use alloy_primitives::U256;
use evm_glue::{assembler::assemble_minimized, assembly::Asm};
use revm::{
    Evm, InMemoryDB,
    primitives::{
        AccountInfo, Bytecode, ExecutionResult, Output, SuccessReason, TransactTo, address,
    },
};
use sir_data::{
    BasicBlock, BasicBlockId, Control, EthIRProgram, Function, FunctionId, Idx, LocalId,
    LocalIndex, Operation, OperationIndex, index_vec,
    operation::{OperationKind, *},
};

/// Builder for creating and testing sequences of IR operations
/// This builder simplifies the creation of test cases by:
/// - Managing local ID allocation automatically
/// - Providing fluent API for adding operations
/// - Handling compilation and execution
/// - Supporting both execution testing and opcode verification
pub struct OperationTestBuilder {
    operations: Vec<Operation>,
    locals_counter: u32,
}

impl OperationTestBuilder {
    pub fn new() -> Self {
        Self { operations: Vec::new(), locals_counter: 0 }
    }

    pub fn with_values(mut self, values: &[u32]) -> Self {
        for &value in values {
            self.operations.push(Operation::SetSmallConst(SetSmallConstData {
                sets: LocalId::new(self.locals_counter),
                value,
            }));
            self.locals_counter += 1;
        }
        self
    }

    pub fn add_operation(mut self, op: impl FnOnce(Vec<LocalId>) -> Operation) -> Self {
        let locals: Vec<LocalId> = (0..self.locals_counter).map(LocalId::new).collect();
        self.operations.push(op(locals));
        self
    }

    pub fn add_binary_op(
        mut self,
        create_op: impl FnOnce(LocalId, LocalId, LocalId) -> Operation,
    ) -> Self {
        let result = LocalId::new(self.locals_counter);
        self.locals_counter += 1;
        self.operations.push(create_op(result, LocalId::new(0), LocalId::new(1)));
        self
    }

    pub fn add_unary_op(mut self, create_op: impl FnOnce(LocalId, LocalId) -> Operation) -> Self {
        let result = LocalId::new(self.locals_counter);
        self.locals_counter += 1;
        self.operations.push(create_op(result, LocalId::new(0)));
        self
    }

    pub fn add_zero_input_op(mut self, create_op: impl FnOnce(LocalId) -> Operation) -> Self {
        let result = LocalId::new(self.locals_counter);
        self.locals_counter += 1;
        self.operations.push(create_op(result));
        self
    }

    pub fn with_stop(mut self) -> Self {
        self.operations.push(Operation::Stop(InlineOperands::default()));
        self
    }

    pub fn with_operations(mut self, operations: Vec<Operation>) -> Self {
        let max_local = find_max_local_id(&operations);
        self.locals_counter = self.locals_counter.max(max_local + 1);
        self.operations.extend(operations);
        self
    }

    pub fn build(self) -> Vec<Operation> {
        self.operations
    }

    pub fn build_and_translate(self) -> Vec<Asm> {
        self.build_program().translate()
    }

    pub fn build_and_execute(self, return_local: u32) -> Result<U256, String> {
        self.with_return(return_local).execute()
    }

    pub fn with_return(mut self, return_local: u32) -> Self {
        let offset_local = LocalId::new(self.locals_counter);
        let size_local = LocalId::new(self.locals_counter + 1);
        self.locals_counter += 2;

        self.operations.extend([
            Operation::SetSmallConst(SetSmallConstData { sets: offset_local, value: 0 }),
            Operation::SetSmallConst(SetSmallConstData { sets: size_local, value: 32 }),
            Operation::MemoryStore(MemoryStoreData {
                ptr: offset_local,
                value: LocalId::new(return_local),
                io_size: IRMemoryIOByteSize::B32,
            }),
            Operation::Return(InlineOperands { ins: [offset_local, size_local], outs: [] }),
        ]);
        self
    }

    pub fn build_program(self) -> TestProgram {
        TestProgram::from_operations(self.operations)
    }

    pub fn execute(self) -> Result<U256, String> {
        self.build_program().execute()
    }
}

/// Wrapper around an EthIRProgram that provides common test operations
pub struct TestProgram {
    program: EthIRProgram,
}

impl TestProgram {
    pub fn from_operations(operations: Vec<Operation>) -> Self {
        Self { program: create_simple_program(operations) }
    }

    pub fn from_program(program: EthIRProgram) -> Self {
        Self { program }
    }

    pub fn translate(self) -> Vec<Asm> {
        translate_program(self.program)
    }

    pub fn into_bytecode(self) -> Result<Vec<u8>, String> {
        let asm = self.translate();
        let (_, bytecode) =
            assemble_minimized(&asm, true).map_err(|e| format!("Assembly failed: {:?}", e))?;
        Ok(bytecode)
    }

    pub fn execute(self) -> Result<U256, String> {
        let bytecode = self.into_bytecode()?;
        execute_bytecode(bytecode)
    }
}

pub struct EvmBuilder {
    db: InMemoryDB,
    contract_address: revm::primitives::Address,
    caller_address: revm::primitives::Address,
    bytecode: Vec<u8>,
    calldata: Vec<u8>,
    gas_limit: u64,
}

impl EvmBuilder {
    pub fn new() -> Self {
        Self {
            db: InMemoryDB::default(),
            contract_address: address!("1000000000000000000000000000000000000000"),
            caller_address: address!("9000000000000000000000000000000000000000"),
            bytecode: Vec::new(),
            calldata: Vec::new(),
            gas_limit: 1_000_000,
        }
    }

    pub fn with_bytecode(mut self, bytecode: Vec<u8>) -> Self {
        self.bytecode = bytecode;
        self
    }

    pub fn with_calldata(mut self, calldata: Vec<u8>) -> Self {
        self.calldata = calldata;
        self
    }

    pub fn with_gas_limit(mut self, gas_limit: u64) -> Self {
        self.gas_limit = gas_limit;
        self
    }

    pub fn build(mut self) -> Evm<'static, (), InMemoryDB> {
        self.db.insert_account_info(
            self.contract_address,
            AccountInfo {
                balance: U256::ZERO,
                nonce: 0,
                code_hash: revm::primitives::keccak256(&self.bytecode),
                code: Some(Bytecode::new_raw(self.bytecode.clone().into())),
            },
        );

        self.db.insert_account_info(
            self.caller_address,
            AccountInfo {
                balance: U256::from(1_000_000_000_000_000_000u64),
                nonce: 0,
                code_hash: revm::primitives::KECCAK_EMPTY,
                code: None,
            },
        );

        Evm::builder()
            .with_db(self.db)
            .modify_tx_env(|tx| {
                tx.caller = self.caller_address;
                tx.transact_to = TransactTo::Call(self.contract_address);
                tx.data = self.calldata.into();
                tx.gas_limit = self.gas_limit;
                tx.gas_price = U256::from(1);
                tx.value = U256::ZERO;
            })
            .build()
    }
}

pub fn create_simple_program(mut operations: Vec<Operation>) -> EthIRProgram {
    // Init code must end with RETURN to deploy runtime code (when there's runtime code)
    // For test helpers, we add a minimal RETURN that deploys empty runtime code
    // BUT: if program explicitly uses Stop/Revert/Invalid, respect that choice
    // Check if last operation is a terminator by checking its kind
    let has_explicit_terminator = operations.last().map_or(false, |op| {
        matches!(
            op.kind(),
            OperationKind::Stop
                | OperationKind::Return
                | OperationKind::Revert
                | OperationKind::Invalid
                | OperationKind::SelfDestruct
        )
    });

    if !has_explicit_terminator {
        let max_local_id = find_max_local_id(&operations);

        // Add minimal deployment RETURN(0, 0) - returns empty runtime code
        let zero_local = LocalId::new(max_local_id + 1);
        operations.push(Operation::SetSmallConst(SetSmallConstData { sets: zero_local, value: 0 }));
        operations.push(Operation::Return(InlineOperands {
            ins: [zero_local, zero_local], // Return 0 bytes (empty deployment)
            outs: [],
        }));
    }

    let max_local_id = find_max_local_id(&operations);
    let locals = (0..=max_local_id).map(LocalId::new).collect();
    let ops_vec: sir_data::IndexVec<OperationIndex, Operation> = operations.into_iter().collect();
    let ops_range = OperationIndex::from_usize(0)..OperationIndex::from_usize(ops_vec.len());

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: index_vec![Function::new(BasicBlockId::new(0), 0)],
        basic_blocks: index_vec![BasicBlock {
            inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            operations: ops_range,
            control: Control::LastOpTerminates,
        }],
        operations: ops_vec,
        locals,
        data_segments_start: index_vec![],
        data_bytes: index_vec![],
        large_consts: index_vec![],
        cases: index_vec![],
        cases_bb_ids: index_vec![],
        next_free_local_id: LocalId::new(max_local_id + 1),
    }
}

fn find_max_local_id(operations: &[Operation]) -> u32 {
    // Manually find the maximum local ID referenced in any operation
    operations
        .iter()
        .flat_map(|op| {
            let mut locals = Vec::new();
            // Extract locals based on operation type - this is a simplified version
            // that works for most common test operations
            match op {
                Operation::SetSmallConst(data) => locals.push(data.sets),
                Operation::SetLargeConst(data) => locals.push(data.sets),
                Operation::Add(data)
                | Operation::Sub(data)
                | Operation::Mul(data)
                | Operation::Div(data)
                | Operation::Mod(data)
                | Operation::And(data)
                | Operation::Or(data)
                | Operation::Xor(data)
                | Operation::Lt(data)
                | Operation::Gt(data)
                | Operation::Eq(data)
                | Operation::SLt(data)
                | Operation::SGt(data)
                | Operation::Shl(data)
                | Operation::Shr(data)
                | Operation::Sar(data)
                | Operation::Byte(data) => {
                    locals.extend_from_slice(&data.ins);
                    locals.extend_from_slice(&data.outs);
                }
                Operation::Not(data) | Operation::IsZero(data) => {
                    locals.extend_from_slice(&data.ins);
                    locals.extend_from_slice(&data.outs);
                }
                Operation::Exp(data)
                | Operation::SignExtend(data)
                | Operation::SDiv(data)
                | Operation::SMod(data) => {
                    locals.extend_from_slice(&data.ins);
                    locals.extend_from_slice(&data.outs);
                }
                Operation::AddMod(data) | Operation::MulMod(data) => {
                    // AllocatedIns uses ins_start instead of ins array
                    let ins_start = data.ins_start.get();
                    for i in 0..3 {
                        locals.push(LocalId::new(ins_start + i));
                    }
                    locals.extend_from_slice(&data.outs);
                }
                Operation::SStore(data) | Operation::TStore(data) => {
                    locals.extend_from_slice(&data.ins);
                }
                Operation::SLoad(data)
                | Operation::TLoad(data)
                | Operation::DynamicAllocZeroed(data)
                | Operation::CallDataLoad(data)
                | Operation::ExtCodeSize(data)
                | Operation::ExtCodeHash(data)
                | Operation::BlockHash(data)
                | Operation::BlobHash(data)
                | Operation::Balance(data)
                | Operation::DynamicAllocAnyBytes(data)
                | Operation::SetCopy(data) => {
                    locals.extend_from_slice(&data.ins);
                    locals.extend_from_slice(&data.outs);
                }
                Operation::Keccak256(data) => {
                    locals.extend_from_slice(&data.ins);
                    locals.extend_from_slice(&data.outs);
                }
                // Zero-input single-output operations
                Operation::Address(data)
                | Operation::Origin(data)
                | Operation::Caller(data)
                | Operation::CallValue(data)
                | Operation::CallDataSize(data)
                | Operation::CodeSize(data)
                | Operation::GasPrice(data)
                | Operation::ReturnDataSize(data)
                | Operation::Coinbase(data)
                | Operation::Timestamp(data)
                | Operation::Number(data)
                | Operation::Difficulty(data)
                | Operation::GasLimit(data)
                | Operation::ChainId(data)
                | Operation::SelfBalance(data)
                | Operation::BaseFee(data)
                | Operation::BlobBaseFee(data)
                | Operation::Gas(data)
                | Operation::AcquireFreePointer(data)
                | Operation::RuntimeStartOffset(data)
                | Operation::InitEndOffset(data)
                | Operation::RuntimeLength(data) => {
                    locals.extend_from_slice(&data.outs);
                }
                Operation::MemoryStore(data) => {
                    locals.push(data.ptr);
                    locals.push(data.value);
                }
                Operation::MemoryLoad(data) => {
                    locals.push(data.ptr);
                    locals.push(data.out);
                }
                // Call operations with AllocatedIns
                Operation::Call(data) | Operation::CallCode(data) => {
                    // 7 inputs starting from ins_start
                    let ins_start = data.ins_start.get();
                    for i in 0..7 {
                        locals.push(LocalId::new(ins_start + i));
                    }
                    locals.extend_from_slice(&data.outs);
                }
                Operation::DelegateCall(data) | Operation::StaticCall(data) => {
                    // 6 inputs starting from ins_start
                    let ins_start = data.ins_start.get();
                    for i in 0..6 {
                        locals.push(LocalId::new(ins_start + i));
                    }
                    locals.extend_from_slice(&data.outs);
                }
                Operation::Create(data) => {
                    // 3 inputs starting from ins_start
                    let ins_start = data.ins_start.get();
                    for i in 0..3 {
                        locals.push(LocalId::new(ins_start + i));
                    }
                    locals.extend_from_slice(&data.outs);
                }
                Operation::Create2(data) => {
                    // 4 inputs starting from ins_start
                    let ins_start = data.ins_start.get();
                    for i in 0..4 {
                        locals.push(LocalId::new(ins_start + i));
                    }
                    locals.extend_from_slice(&data.outs);
                }
                Operation::ExtCodeCopy(data) => {
                    // 4 inputs starting from ins_start, no outputs
                    let ins_start = data.ins_start.get();
                    for i in 0..4 {
                        locals.push(LocalId::new(ins_start + i));
                    }
                }
                Operation::Log2(data) => {
                    // 4 inputs starting from ins_start, no outputs
                    let ins_start = data.ins_start.get();
                    for i in 0..4 {
                        locals.push(LocalId::new(ins_start + i));
                    }
                }
                Operation::Log3(data) => {
                    // 5 inputs starting from ins_start, no outputs
                    let ins_start = data.ins_start.get();
                    for i in 0..5 {
                        locals.push(LocalId::new(ins_start + i));
                    }
                }
                Operation::Log4(data) => {
                    // 6 inputs starting from ins_start, no outputs
                    let ins_start = data.ins_start.get();
                    for i in 0..6 {
                        locals.push(LocalId::new(ins_start + i));
                    }
                }
                _ => {}
            }
            locals
        })
        .map(|id| id.get())
        .max()
        .unwrap_or(0)
}

/// Parse human-readable IR string into an EthIRProgram, panicking on errors
///
/// This is a convenience function for tests that handles:
/// - Arena allocation
/// - Parsing to AST
/// - IR emission
/// - Error formatting with source context
///
/// # Example
/// ```ignore
/// let program = checked_parse(r#"
///     fn init:
///         entry {
///             value = const 42
///             offset = const 0
///             size = const 32
///             mstore256 offset value
///             return offset size
///         }
/// "#);
/// ```
///
/// # Panics
/// Panics with a detailed error message if parsing or IR emission fails
pub fn checked_parse(source: &str) -> EthIRProgram {
    use bumpalo::{Bump, collections::String as BString};
    use sir_parser::{
        emit::{self, EmitConfig},
        highlight_span, parser,
    };

    let arena = Bump::with_capacity(8_192);

    // Parse source to AST
    let ast = parser::parse(source, &arena).unwrap_or_else(|err| {
        let err = &err[0];
        let mut out = BString::with_capacity_in(200, &arena);
        highlight_span(&mut out, source, err.span().clone(), 2);
        panic!("Parse error:\n{}\n{:?}", out, err);
    });

    // Emit IR from AST (use new_without_run for single-function test programs)
    let config = EmitConfig::new_without_run();
    let ir = emit::emit_ir(&arena, &ast, config).unwrap_or_else(|err| {
        let mut out = BString::with_capacity_in(400, &arena);
        for span in err.spans.iter() {
            highlight_span(&mut out, source, span.clone(), 0);
        }
        panic!("IR emission error:\n{}{}", out, err.reason);
    });

    ir
}

pub fn create_branching_program(
    blocks: Vec<(Vec<Operation>, Control)>,
    start_block: usize,
) -> EthIRProgram {
    let (all_operations, basic_blocks) = build_blocks_from_ops(blocks);
    let max_local_id = find_max_local_id(&all_operations);
    let locals = (0..=max_local_id + 200).map(LocalId::new).collect();
    let ops_vec = all_operations.into_iter().collect();

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: index_vec![Function::new(BasicBlockId::new(start_block as u32), 0)],
        basic_blocks,
        operations: ops_vec,
        locals,
        data_segments_start: index_vec![],
        data_bytes: index_vec![],
        large_consts: index_vec![],
        cases: index_vec![],
        cases_bb_ids: index_vec![],
        next_free_local_id: LocalId::new(max_local_id + 201),
    }
}

fn build_blocks_from_ops(
    blocks: Vec<(Vec<Operation>, Control)>,
) -> (Vec<Operation>, sir_data::IndexVec<BasicBlockId, BasicBlock>) {
    let mut all_operations = Vec::new();
    let mut basic_blocks = index_vec![];

    for (block_ops, control) in blocks {
        let start_idx = all_operations.len();
        all_operations.extend(block_ops);
        let end_idx = all_operations.len();

        basic_blocks.push(BasicBlock {
            inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
            operations: OperationIndex::from_usize(start_idx)..OperationIndex::from_usize(end_idx),
            control,
        });
    }

    (all_operations, basic_blocks)
}

pub fn create_program_with_switch(
    _blocks: Vec<(Vec<Operation>, Control)>,
    _switch_cases: Vec<Vec<()>>,
) -> EthIRProgram {
    // TODO: Update this function to work with the new Cases API
    unimplemented!("create_program_with_switch needs to be updated for new Cases structure")
}

type FunctionBlocks = Vec<(Vec<Operation>, Control)>;

pub fn create_multi_function_program(functions: Vec<(FunctionBlocks, usize)>) -> EthIRProgram {
    let mut all_operations = Vec::new();
    let mut all_blocks = index_vec![];
    let mut all_functions = index_vec![];

    for (function_blocks, entry_block) in functions {
        let function_start_block = all_blocks.len();
        let (ops, blocks) = build_blocks_from_ops(function_blocks);
        all_operations.extend(ops);
        all_blocks.extend(blocks);

        all_functions
            .push(Function::new(BasicBlockId::new((function_start_block + entry_block) as u32), 0));
    }

    let max_local_id = find_max_local_id(&all_operations);
    let locals = (0..=max_local_id + 200).map(LocalId::new).collect();
    let ops_vec = all_operations.into_iter().collect();

    EthIRProgram {
        init_entry: FunctionId::new(0),
        main_entry: None,
        functions: all_functions,
        basic_blocks: all_blocks,
        operations: ops_vec,
        locals,
        data_segments_start: index_vec![],
        data_bytes: index_vec![],
        large_consts: index_vec![],
        cases: index_vec![],
        cases_bb_ids: index_vec![],
        next_free_local_id: LocalId::new(max_local_id + 201),
    }
}

pub fn create_ops_with_return(operations: Vec<Operation>, return_local: u32) -> Vec<Operation> {
    OperationTestBuilder::new().with_operations(operations).with_return(return_local).build()
}

pub fn create_return_for_local(
    local_id: u32,
    offset_local: u32,
    size_local: u32,
) -> Vec<Operation> {
    vec![
        Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(offset_local),
            value: (constants::LOCALS_START as u64 + local_id as u64 * constants::SLOT_SIZE as u64)
                as u32,
        }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(size_local), value: 32 }),
        Operation::Return(InlineOperands {
            ins: [LocalId::new(offset_local), LocalId::new(size_local)],
            outs: [],
        }),
    ]
}

pub fn build_binary_op_with_return<F>(a: u32, b: u32, create_op: F) -> Vec<Operation>
where
    F: FnOnce(LocalId, LocalId, LocalId) -> Operation,
{
    OperationTestBuilder::new().with_values(&[a, b]).add_binary_op(create_op).with_return(2).build()
}

pub fn local_memory_offset(local_id: u32) -> u64 {
    constants::LOCALS_START as u64 + (local_id as u64) * constants::SLOT_SIZE as u64
}

pub fn compile_to_bytecode(operations: Vec<Operation>) -> Vec<u8> {
    TestProgram::from_operations(operations)
        .into_bytecode()
        .expect("Failed to compile operations to bytecode")
}

pub fn execute_bytecode_raw(bytecode: Vec<u8>) -> ExecutionResult {
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();
    evm.transact_commit().expect("EVM transaction failed to commit")
}

pub fn execute_bytecode(bytecode: Vec<u8>) -> Result<U256, String> {
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();
    let result = evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;

    match result {
        ExecutionResult::Success { output, .. } => match output {
            Output::Call(bytes) => {
                if bytes.len() >= 32 {
                    Ok(U256::from_be_bytes::<32>(
                        bytes[0..32].try_into().expect("Failed to convert bytes to array"),
                    ))
                } else {
                    Err("Return data too small".to_string())
                }
            }
            _ => Err("Unexpected output type".to_string()),
        },
        ExecutionResult::Revert { output, .. } => Err(format!("Reverted: {:?}", output)),
        ExecutionResult::Halt { reason, .. } => Err(format!("Halted: {:?}", reason)),
    }
}

pub fn execute_and_get_result(bytecode: Vec<u8>) -> Result<U256, String> {
    execute_bytecode(bytecode)
}

pub fn execute_and_verify_revert(bytecode: Vec<u8>) -> Result<(), String> {
    let result = execute_bytecode_raw(bytecode);
    match result {
        ExecutionResult::Revert { .. } => Ok(()),
        _ => Err(format!("Expected revert, got: {:?}", result)),
    }
}

pub fn execute_bytecode_with_calldata(
    bytecode: Vec<u8>,
    calldata: Vec<u8>,
) -> Result<U256, String> {
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).with_calldata(calldata).build();
    let result = evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;

    match result {
        ExecutionResult::Success { output, .. } => match output {
            Output::Call(bytes) => {
                if bytes.len() >= 32 {
                    Ok(U256::from_be_bytes::<32>(
                        bytes[0..32].try_into().expect("Failed to convert bytes to array"),
                    ))
                } else {
                    Err("Return data too small".to_string())
                }
            }
            _ => Err("Unexpected output type".to_string()),
        },
        ExecutionResult::Revert { output, .. } => Err(format!("Reverted: {:?}", output)),
        ExecutionResult::Halt { reason, .. } => Err(format!("Halted: {:?}", reason)),
    }
}

pub fn execute_and_get_result_with_calldata(
    bytecode: Vec<u8>,
    calldata: Vec<u8>,
) -> Result<U256, String> {
    execute_bytecode_with_calldata(bytecode, calldata)
}

pub fn execute_and_verify_stop(bytecode: Vec<u8>) -> Result<(), String> {
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();
    let result = evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;

    match result {
        ExecutionResult::Success { reason, .. } => {
            if reason == SuccessReason::Stop {
                Ok(())
            } else {
                Err(format!("Expected STOP but got {:?}", reason))
            }
        }
        _ => Err(format!("Expected successful STOP but got {:?}", result)),
    }
}

pub fn execute_and_verify_halt(bytecode: Vec<u8>) -> Result<(), String> {
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();
    let result = evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;

    match result {
        ExecutionResult::Halt { .. } => Ok(()),
        _ => Err(format!("Expected HALT but got {:?}", result)),
    }
}

pub fn execute_and_verify_result(operations: Vec<Operation>, expected: U256) -> Result<(), String> {
    let result_local = find_result_local(&operations)
        .ok_or_else(|| "No result-producing operation found".to_string())?;

    let actual =
        TestProgram::from_operations(create_ops_with_return(operations, result_local)).execute()?;

    if actual == expected {
        Ok(())
    } else {
        Err(format!("Expected {} but got {}", expected, actual))
    }
}

pub fn execute_storage_operations(operations: Vec<Operation>) -> Result<(), String> {
    let bytecode = TestProgram::from_operations(operations).into_bytecode()?;
    let mut evm = EvmBuilder::new().with_bytecode(bytecode).build();
    evm.transact_commit().map_err(|e| format!("Execution error: {:?}", e))?;
    Ok(())
}

pub fn assert_opcode_counts(asm: &[Asm], expected: &[(&str, usize)]) {
    for &(opcode, expected_count) in expected {
        let actual_count = count_opcode(asm, opcode);
        assert_eq!(
            actual_count, expected_count,
            "Expected {} {} opcodes but found {}",
            expected_count, opcode, actual_count
        );
    }
}

pub fn count_opcode(asm: &[Asm], opcode: &str) -> usize {
    asm.iter()
        .filter(|&op| match op {
            Asm::Op(op_enum) => format!("{:?}", op_enum) == opcode,
            _ => false,
        })
        .count()
}

pub fn extract_opcode_sequence(asm: &[Asm]) -> Vec<String> {
    asm.iter()
        .filter_map(|op| match op {
            Asm::Op(opcode) => Some(format!("{:?}", opcode)),
            _ => None,
        })
        .collect()
}

/// Verifies opcodes appear in order, allowing other instructions between them
pub fn assert_opcode_sequence(asm: &[Asm], expected: &[&str]) {
    let opcodes = extract_opcode_sequence(asm);
    let mut expected_idx = 0;
    let mut found_indices = Vec::new();

    for (i, opcode) in opcodes.iter().enumerate() {
        if expected_idx < expected.len() && opcode == expected[expected_idx] {
            found_indices.push(i);
            expected_idx += 1;
        }
    }

    if expected_idx != expected.len() {
        let mut msg = String::from("Expected opcode sequence not found\n");
        msg.push_str("Expected sequence:\n");
        for (i, op) in expected.iter().enumerate() {
            let status = if i < expected_idx { "✓" } else { "✗" };
            msg.push_str(&format!("  {} [{}] {}\n", status, i, op));
        }
        msg.push_str("\nActual opcodes:\n");
        for (i, op) in opcodes.iter().take(30).enumerate() {
            let marker = if found_indices.contains(&i) { " <--" } else { "" };
            msg.push_str(&format!("  [{}] {}{}\n", i, op, marker));
        }
        if opcodes.len() > 30 {
            msg.push_str(&format!("  ... ({} more opcodes)\n", opcodes.len() - 30));
        }
        panic!("{}", msg);
    }
}

pub fn ir_to_bytecode(program: EthIRProgram) -> Vec<u8> {
    TestProgram::from_program(program)
        .into_bytecode()
        .expect("Failed to compile IR program to bytecode")
}

/// Execute blocks and extract result
pub fn execute_blocks_and_extract(
    blocks: Vec<(Vec<Operation>, Control)>,
    entry_block: usize,
) -> U256 {
    let program = create_branching_program(blocks, entry_block);
    let bytecode = ir_to_bytecode(program);
    execute_bytecode(bytecode).expect("Failed to execute blocks and extract result")
}

/// Find the last operation that produces a result and return its local ID
fn find_result_local(operations: &[Operation]) -> Option<u32> {
    operations.iter().rev().find_map(|op| match op {
        // Extract result from operations that produce a value
        Operation::Add(InlineOperands { outs: [result], .. })
        | Operation::Sub(InlineOperands { outs: [result], .. })
        | Operation::Mul(InlineOperands { outs: [result], .. })
        | Operation::Div(InlineOperands { outs: [result], .. })
        | Operation::Mod(InlineOperands { outs: [result], .. })
        | Operation::Exp(InlineOperands { outs: [result], .. })
        | Operation::SignExtend(InlineOperands { outs: [result], .. })
        | Operation::SDiv(InlineOperands { outs: [result], .. })
        | Operation::SMod(InlineOperands { outs: [result], .. })
        | Operation::Lt(InlineOperands { outs: [result], .. })
        | Operation::Gt(InlineOperands { outs: [result], .. })
        | Operation::SLt(InlineOperands { outs: [result], .. })
        | Operation::SGt(InlineOperands { outs: [result], .. })
        | Operation::Eq(InlineOperands { outs: [result], .. })
        | Operation::And(InlineOperands { outs: [result], .. })
        | Operation::Or(InlineOperands { outs: [result], .. })
        | Operation::Xor(InlineOperands { outs: [result], .. })
        | Operation::Shl(InlineOperands { outs: [result], .. })
        | Operation::Shr(InlineOperands { outs: [result], .. })
        | Operation::Sar(InlineOperands { outs: [result], .. })
        | Operation::Byte(InlineOperands { outs: [result], .. })
        | Operation::Keccak256(InlineOperands { outs: [result], .. })
        | Operation::IsZero(InlineOperands { outs: [result], .. })
        | Operation::Not(InlineOperands { outs: [result], .. })
        | Operation::Balance(InlineOperands { outs: [result], .. })
        | Operation::ExtCodeSize(InlineOperands { outs: [result], .. })
        | Operation::ExtCodeHash(InlineOperands { outs: [result], .. })
        | Operation::SLoad(InlineOperands { outs: [result], .. })
        | Operation::TLoad(InlineOperands { outs: [result], .. })
        | Operation::CallDataLoad(InlineOperands { outs: [result], .. })
        | Operation::BlobHash(InlineOperands { outs: [result], .. })
        | Operation::Address(InlineOperands { outs: [result], .. })
        | Operation::Origin(InlineOperands { outs: [result], .. })
        | Operation::Caller(InlineOperands { outs: [result], .. })
        | Operation::CallValue(InlineOperands { outs: [result], .. })
        | Operation::CallDataSize(InlineOperands { outs: [result], .. })
        | Operation::GasPrice(InlineOperands { outs: [result], .. })
        | Operation::ReturnDataSize(InlineOperands { outs: [result], .. })
        | Operation::SelfBalance(InlineOperands { outs: [result], .. })
        | Operation::Number(InlineOperands { outs: [result], .. })
        | Operation::Difficulty(InlineOperands { outs: [result], .. })
        | Operation::GasLimit(InlineOperands { outs: [result], .. })
        | Operation::ChainId(InlineOperands { outs: [result], .. })
        | Operation::Coinbase(InlineOperands { outs: [result], .. })
        | Operation::Timestamp(InlineOperands { outs: [result], .. })
        | Operation::BaseFee(InlineOperands { outs: [result], .. })
        | Operation::BlobBaseFee(InlineOperands { outs: [result], .. })
        | Operation::Gas(InlineOperands { outs: [result], .. })
        | Operation::AcquireFreePointer(InlineOperands { outs: [result], .. }) => {
            Some(result.get())
        }

        Operation::AddMod(AllocatedIns { outs: [result], .. })
        | Operation::MulMod(AllocatedIns { outs: [result], .. }) => Some(result.get()),

        Operation::MemoryLoad(MemoryLoadData { out, .. }) => Some(out.get()),

        _ => None,
    })
}
/// Helper to create a sequence of LocalSetSmallConst operations
pub fn set_locals(values: &[(u32, u32)]) -> Vec<Operation> {
    values
        .iter()
        .map(|&(local_id, value)| {
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(local_id), value })
        })
        .collect()
}

/// Helper to create multiple zero-valued locals
pub fn set_zero_locals(start_id: u32, count: u32) -> Vec<Operation> {
    (start_id..start_id + count)
        .map(|id| Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(id), value: 0 }))
        .collect()
}

/// Helper to create a memory store operation with less boilerplate
pub fn memory_store(address_local: u32, value_local: u32, byte_size: u8) -> Operation {
    let io_size = match byte_size {
        1 => IRMemoryIOByteSize::B1,
        32 => IRMemoryIOByteSize::B32,
        _ => panic!("Unsupported byte_size: {}", byte_size),
    };
    Operation::MemoryStore(MemoryStoreData {
        ptr: LocalId::new(address_local),
        value: LocalId::new(value_local),
        io_size,
    })
}

/// Helper to create a memory load operation with less boilerplate
pub fn memory_load(result_local: u32, address_local: u32, byte_size: u8) -> Operation {
    let io_size = match byte_size {
        1 => IRMemoryIOByteSize::B1,
        32 => IRMemoryIOByteSize::B32,
        _ => panic!("Unsupported byte_size: {}", byte_size),
    };
    Operation::MemoryLoad(MemoryLoadData {
        out: LocalId::new(result_local),
        ptr: LocalId::new(address_local),
        io_size,
    })
}

/// Helper to add return operations to an operations vector
pub fn with_return(mut ops: Vec<Operation>, result_local: u32) -> Vec<Operation> {
    ops.extend(create_return_for_local(result_local, result_local + 1, result_local + 2));
    ops
}

/// Helper that combines the common pattern: create_simple_program -> ir_to_bytecode ->
/// execute_and_get_result
pub fn execute_operations(operations: Vec<Operation>) -> Result<U256, String> {
    let program = create_simple_program(operations);
    let bytecode = ir_to_bytecode(program);
    execute_bytecode(bytecode)
}

/// Helper that adds return and then executes operations
pub fn execute_operations_with_return(
    operations: Vec<Operation>,
    result_local: u32,
) -> Result<U256, String> {
    let ops_with_return = with_return(operations, result_local);
    execute_operations(ops_with_return)
}
