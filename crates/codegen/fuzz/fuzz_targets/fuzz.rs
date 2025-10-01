#![no_main]

use libfuzzer_sys::fuzz_target;
use eth_ir_data::*;
use eth_ir_data::operation::*;
use eth_ir_codegen::{Translator, TranslatorConfig};

// Modular fuzzer with improved maintainability
fuzz_target!(|data: &[u8]| {
    let program = ProgramGenerator::new(data).generate();
    FuzzTester::new().test_program(program);
});

// Configuration constants
mod config {
    pub const MAX_LOCALS: usize = 100;
    pub const MAX_OPERATIONS: usize = 200;
    pub const MAX_BLOCKS: usize = 10;
    pub const DEFAULT_BYTE_SIZE: u8 = 32;
}

// Main program generator with strategy pattern
struct ProgramGenerator<'a> {
    data: &'a [u8],
    strategy: GenerationStrategy,
}

#[derive(Debug, Clone, Copy)]
enum GenerationStrategy {
    Empty,
    Simple,
    ControlFlow,
    MemoryStress,
    EdgeCases,
    Complex,
}

impl<'a> ProgramGenerator<'a> {
    fn new(data: &'a [u8]) -> Self {
        let strategy = if data.is_empty() {
            GenerationStrategy::Empty
        } else {
            match data[0] % 5 {
                0 => GenerationStrategy::Simple,
                1 => GenerationStrategy::ControlFlow,
                2 => GenerationStrategy::MemoryStress,
                3 => GenerationStrategy::EdgeCases,
                _ => GenerationStrategy::Complex,
            }
        };

        Self { data, strategy }
    }

    fn generate(&self) -> EthIRProgram {
        match self.strategy {
            GenerationStrategy::Empty => EmptyProgramBuilder::new().build(),
            GenerationStrategy::Simple => SimpleProgramBuilder::new(self.data).build(),
            GenerationStrategy::ControlFlow => ControlFlowBuilder::new(self.data).build(),
            GenerationStrategy::MemoryStress => MemoryStressBuilder::new(self.data).build(),
            GenerationStrategy::EdgeCases => EdgeCaseBuilder::new(self.data).build(),
            GenerationStrategy::Complex => ComplexProgramBuilder::new(self.data).build(),
        }
    }
}

// Base builder with common functionality
struct ProgramBuilder {
    operations: IndexVec<OperationIndex, Operation>,
    locals: IndexVec<LocalIndex, LocalId>,
    basic_blocks: IndexVec<BasicBlockId, BasicBlock>,
    data_segments_start: IndexVec<DataId, DataOffset>,
    data_bytes: IndexVec<DataOffset, u8>,
    large_consts: IndexVec<LargeConstId, alloy_primitives::U256>,
    cases: IndexVec<CasesId, Cases>,
}

impl ProgramBuilder {
    fn new() -> Self {
        Self {
            operations: IndexVec::new(),
            locals: IndexVec::new(),
            basic_blocks: IndexVec::new(),
            data_segments_start: IndexVec::new(),
            data_bytes: IndexVec::new(),
            large_consts: IndexVec::new(),
            cases: IndexVec::new(),
        }
    }

    fn add_locals(&mut self, count: usize) {
        for i in 0..count {
            self.locals.push(LocalId::from_usize(i));
        }
    }

    fn ensure_terminator(&mut self) {
        if !self.has_terminator() {
            self.operations.push(Operation::Stop);
        }
    }

    fn has_terminator(&self) -> bool {
        self.operations.last().map_or(false, |op| {
            matches!(op, Operation::Stop | Operation::Return(_) | Operation::Revert(_) | Operation::Invalid)
        })
    }

    fn build(mut self) -> EthIRProgram {
        self.ensure_terminator();

        // Add default basic block if none exist
        if self.basic_blocks.is_empty() {
            self.basic_blocks.push(BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(0)..OperationIndex::from_usize(self.operations.len()),
                control: Control::LastOpTerminates,
            });
        }

        EthIRProgram {
            init_entry: FunctionId::from_usize(0),
            main_entry: None,
            functions: index_vec![Function {
                entry: BasicBlockId::from_usize(0),
                outputs: 0
            }],
            basic_blocks: self.basic_blocks,
            operations: self.operations,
            locals: self.locals,
            data_segments_start: self.data_segments_start,
            data_bytes: self.data_bytes,
            large_consts: self.large_consts,
            cases: self.cases,
        }
    }
}

// Operation generators (composable)
struct OperationGenerator;

impl OperationGenerator {
    fn arithmetic_ops() -> &'static [fn(usize, usize) -> Operation] {
        &[
            |local_id, num_locals| Operation::Add(Self::binary_op(local_id, num_locals)),
            |local_id, num_locals| Operation::Sub(Self::binary_op(local_id, num_locals)),
            |local_id, num_locals| Operation::Mul(Self::binary_op(local_id, num_locals)),
            |local_id, num_locals| Operation::Div(Self::binary_op(local_id, num_locals)),
            |local_id, num_locals| Operation::Mod(Self::binary_op(local_id, num_locals)),
        ]
    }


    fn memory_ops() -> &'static [fn(usize, usize) -> Operation] {
        &[
            |local_id, num_locals| Operation::MemoryStore(MemoryStore {
                address: LocalId::from_usize(local_id),
                value: LocalId::from_usize((local_id + 1) % num_locals),
                byte_size: config::DEFAULT_BYTE_SIZE,
            }),
            |local_id, num_locals| Operation::MemoryLoad(MemoryLoad {
                address: LocalId::from_usize(local_id),
                result: LocalId::from_usize((local_id + 1) % num_locals),
                byte_size: config::DEFAULT_BYTE_SIZE,
            }),
        ]
    }

    fn binary_op(local_id: usize, num_locals: usize) -> TwoInOneOut {
        TwoInOneOut {
            arg1: LocalId::from_usize(local_id),
            arg2: LocalId::from_usize((local_id + 1) % num_locals),
            result: LocalId::from_usize((local_id + 2) % num_locals),
        }
    }

    fn select_random_op(ops: &[fn(usize, usize) -> Operation], seed: u8, local_id: usize, num_locals: usize) -> Operation {
        let index = (seed as usize) % ops.len();
        ops[index](local_id, num_locals)
    }
}

// Specialized builders
struct EmptyProgramBuilder;

impl EmptyProgramBuilder {
    fn new() -> Self { Self }

    fn build(self) -> EthIRProgram {
        ProgramBuilder::new().build()
    }
}

struct SimpleProgramBuilder<'a> {
    data: &'a [u8],
}

impl<'a> SimpleProgramBuilder<'a> {
    fn new(data: &'a [u8]) -> Self { Self { data } }

    fn build(self) -> EthIRProgram {
        let mut builder = ProgramBuilder::new();

        let num_locals = self.data.get(0)
            .map(|&b| (b as usize % config::MAX_LOCALS) + 1)
            .unwrap_or(5);

        builder.add_locals(num_locals);

        let num_ops = self.data.get(1)
            .map(|&b| (b as usize % config::MAX_OPERATIONS) + 1)
            .unwrap_or(10);

        for i in 0..num_ops.min(self.data.len() / 2) {
            let byte = self.data.get(i * 2).unwrap_or(&0);
            let local_id = i % num_locals;

            let operation = match byte % 3 {
                0 => Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::from_usize(local_id),
                    value: *byte as u64,
                }),
                1 => OperationGenerator::select_random_op(
                    OperationGenerator::arithmetic_ops(),
                    *byte,
                    local_id,
                    num_locals
                ),
                _ => OperationGenerator::select_random_op(
                    OperationGenerator::memory_ops(),
                    *byte,
                    local_id,
                    num_locals
                ),
            };

            builder.operations.push(operation);
        }

        builder.build()
    }
}

struct ControlFlowBuilder<'a> {
    data: &'a [u8],
}

impl<'a> ControlFlowBuilder<'a> {
    fn new(data: &'a [u8]) -> Self { Self { data } }

    fn build(self) -> EthIRProgram {
        let mut builder = ProgramBuilder::new();

        let num_blocks = self.data.get(0)
            .map(|&b| (b as usize % config::MAX_BLOCKS) + 1)
            .unwrap_or(3);

        builder.add_locals(20);

        let mut op_counter = 0;

        for block_id in 0..num_blocks {
            let block_start = op_counter;

            // Add operations to block
            for i in 0..3 {
                builder.operations.push(Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::from_usize(block_id % 20),
                    value: (block_id + i) as u64,
                }));
                op_counter += 1;
            }

            // Determine control flow
            let control = if block_id == num_blocks - 1 {
                builder.operations.push(Operation::Stop);
                op_counter += 1;
                Control::LastOpTerminates
            } else {
                let control_type = self.data.get(block_id + 1).unwrap_or(&0) % 3;
                match control_type {
                    0 => Control::ContinuesTo(BasicBlockId::from_usize((block_id + 1) % num_blocks)),
                    1 => Control::Branches(Branch {
                        condition: LocalId::from_usize(block_id % 20),
                        zero_target: BasicBlockId::from_usize((block_id + 1) % num_blocks),
                        non_zero_target: BasicBlockId::from_usize((block_id + 2) % num_blocks),
                    }),
                    _ => Control::LastOpTerminates,
                }
            };

            builder.basic_blocks.push(BasicBlock {
                inputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                outputs: LocalIndex::from_usize(0)..LocalIndex::from_usize(0),
                operations: OperationIndex::from_usize(block_start)..OperationIndex::from_usize(op_counter),
                control,
            });
        }

        builder.build()
    }
}

struct MemoryStressBuilder<'a> {
    data: &'a [u8],
}

impl<'a> MemoryStressBuilder<'a> {
    fn new(data: &'a [u8]) -> Self { Self { data } }

    fn build(self) -> EthIRProgram {
        let mut builder = ProgramBuilder::new();
        builder.add_locals(20);

        // Test various memory allocation patterns
        for (i, chunk) in self.data.chunks(4).enumerate().take(30) {
            if chunk.is_empty() { break; }

            let local_id = i % 20;
            let size_local = LocalId::from_usize(local_id);
            let ptr_local = LocalId::from_usize((local_id + 1) % 20);

            // Variable allocation sizes
            let size_op = match chunk[0] % 6 {
                0 => Operation::LocalSetSmallConst(SetSmallConst {
                    local: size_local,
                    value: 0 // Zero size allocation
                }),
                1 => Operation::LocalSetSmallConst(SetSmallConst {
                    local: size_local,
                    value: 1
                }),
                2 => Operation::LocalSetSmallConst(SetSmallConst {
                    local: size_local,
                    value: 32
                }),
                3 => Operation::LocalSetSmallConst(SetSmallConst {
                    local: size_local,
                    value: 1024
                }),
                4 => Operation::LocalSetSmallConst(SetSmallConst {
                    local: size_local,
                    value: 65536 // Large allocation
                }),
                _ => Operation::LocalSetSmallConst(SetSmallConst {
                    local: size_local,
                    value: u32::MAX as u64 // Over-limit allocation
                }),
            };
            builder.operations.push(size_op);

            // Different allocation types
            let alloc_op = match chunk.get(1).unwrap_or(&0) % 4 {
                0 => Operation::DynamicAllocZeroed(OneInOneOut {
                    arg1: size_local,
                    result: ptr_local,
                }),
                1 => Operation::DynamicAllocAnyBytes(OneInOneOut {
                    arg1: size_local,
                    result: ptr_local,
                }),
                2 => Operation::LocalAllocZeroed(OneInOneOut {
                    arg1: size_local,
                    result: ptr_local,
                }),
                _ => Operation::LocalAllocAnyBytes(OneInOneOut {
                    arg1: size_local,
                    result: ptr_local,
                }),
            };
            builder.operations.push(alloc_op);

            // Memory operations with varying byte sizes
            let byte_size = match chunk.get(2).unwrap_or(&32) % 5 {
                0 => 1,
                1 => 8,
                2 => 16,
                3 => 32,
                _ => 64,
            };

            builder.operations.push(Operation::MemoryStore(MemoryStore {
                address: ptr_local,
                value: LocalId::from_usize((local_id + 3) % 20),
                byte_size,
            }));

            builder.operations.push(Operation::MemoryLoad(MemoryLoad {
                address: ptr_local,
                result: LocalId::from_usize((local_id + 4) % 20),
                byte_size,
            }));
        }

        builder.build()
    }
}

struct EdgeCaseBuilder<'a> {
    data: &'a [u8],
}

impl<'a> EdgeCaseBuilder<'a> {
    fn new(data: &'a [u8]) -> Self { Self { data } }

    fn build(self) -> EthIRProgram {
        let mut builder = ProgramBuilder::new();
        builder.add_locals(20);

        // Generate edge case operations
        for (i, &byte) in self.data.iter().enumerate().take(30) {
            let local_id = i % 20;

            let operation = match byte % 12 {
                // Division by zero
                0 => Operation::Div(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize(local_id), // Same value, likely zero
                    result: LocalId::from_usize((local_id + 1) % 20),
                }),
                // Modulo by zero
                1 => Operation::Mod(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize(local_id),
                    result: LocalId::from_usize((local_id + 1) % 20),
                }),
                // Large shift operations
                2 => {
                    builder.operations.push(Operation::LocalSetSmallConst(SetSmallConst {
                        local: LocalId::from_usize(local_id),
                        value: 256, // Shift by 256 bits
                    }));
                    Operation::Shl(TwoInOneOut {
                        arg1: LocalId::from_usize((local_id + 1) % 20),
                        arg2: LocalId::from_usize(local_id),
                        result: LocalId::from_usize((local_id + 2) % 20),
                    })
                },
                // Self-referential operations
                3 => Operation::Add(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize(local_id),
                    result: LocalId::from_usize(local_id), // Result overwrites input
                }),
                // Oversized memory operations
                4 => Operation::MemoryStore(MemoryStore {
                    address: LocalId::from_usize(local_id),
                    value: LocalId::from_usize((local_id + 1) % 20),
                    byte_size: 255, // Too large
                }),
                // Invalid terminators
                5 => Operation::Invalid,
                6 => Operation::Revert(TwoInZeroOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize((local_id + 1) % 20),
                }),
                // Multiple consecutive terminators
                7 => {
                    builder.operations.push(Operation::Stop);
                    Operation::Return(TwoInZeroOut {
                        arg1: LocalId::from_usize(local_id),
                        arg2: LocalId::from_usize((local_id + 1) % 20),
                    })
                },
                // Bitwise edge cases
                8 => Operation::Xor(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize(local_id), // XOR with self = 0
                    result: LocalId::from_usize((local_id + 1) % 20),
                }),
                // Sign extension edge cases
                9 => Operation::SignExtend(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id), // Large byte index
                    arg2: LocalId::from_usize((local_id + 1) % 20),
                    result: LocalId::from_usize((local_id + 2) % 20),
                }),
                // Signed comparison edge cases
                10 => Operation::SLt(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize((local_id + 1) % 20),
                    result: LocalId::from_usize((local_id + 2) % 20),
                }),
                _ => Operation::NoOp,
            };

            builder.operations.push(operation);
        }

        builder.build()
    }
}

struct ComplexProgramBuilder<'a> {
    data: &'a [u8],
}

impl<'a> ComplexProgramBuilder<'a> {
    fn new(data: &'a [u8]) -> Self { Self { data } }

    fn build(self) -> EthIRProgram {
        let mut builder = ProgramBuilder::new();

        let num_locals = self.data.get(0)
            .map(|&b| (b as usize % config::MAX_LOCALS) + 1)
            .unwrap_or(50);

        builder.add_locals(num_locals);

        // Generate large constants
        for chunk in self.data.chunks(32).take(5) {
            let mut bytes = [0u8; 32];
            for (i, &byte) in chunk.iter().enumerate() {
                if i < 32 { bytes[i] = byte; }
            }
            builder.large_consts.push(alloy_primitives::U256::from_be_bytes(bytes));
        }

        // Generate data segments
        for chunk in self.data.chunks(64).take(3) {
            builder.data_segments_start.push(DataOffset::from_usize(builder.data_bytes.len()));
            for &byte in chunk {
                builder.data_bytes.push(byte);
            }
        }

        // Complex operations mixing everything
        for (i, chunk) in self.data.chunks(5).enumerate().take(50) {
            if chunk.is_empty() { break; }

            let local_id = i % num_locals;

            let operation = match chunk[0] % 20 {
                // Large constant usage
                0 if !builder.large_consts.is_empty() => {
                    Operation::LocalSetLargeConst(SetLargeConst {
                        local: LocalId::from_usize(local_id),
                        cid: LargeConstId::from_usize(i % builder.large_consts.len()),
                    })
                },
                // Data reference
                1 if !builder.data_segments_start.is_empty() => {
                    Operation::LocalSetDataOffset(SetDataOffset {
                        segment_id: DataId::from_usize(i % builder.data_segments_start.len()),
                        local: LocalId::from_usize(local_id),
                    })
                },
                // Environmental operations
                2 => Operation::Address(ZeroInOneOut {
                    result: LocalId::from_usize(local_id),
                }),
                3 => Operation::Origin(ZeroInOneOut {
                    result: LocalId::from_usize(local_id),
                }),
                4 => Operation::GasPrice(ZeroInOneOut {
                    result: LocalId::from_usize(local_id),
                }),
                5 => Operation::BlockHash(OneInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    result: LocalId::from_usize((local_id + 1) % num_locals),
                }),
                // Calldata operations
                6 => Operation::CallDataSize(ZeroInOneOut {
                    result: LocalId::from_usize(local_id),
                }),
                7 => Operation::CallDataLoad(OneInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    result: LocalId::from_usize((local_id + 1) % num_locals),
                }),
                // Crypto operations
                8 => Operation::Keccak256(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize((local_id + 1) % num_locals),
                    result: LocalId::from_usize((local_id + 2) % num_locals),
                }),
                // Advanced arithmetic
                9 => Operation::AddMod(LargeInOneOut {
                    args_start: LocalIndex::from_usize(local_id),
                    result: LocalId::from_usize((local_id + 3) % num_locals),
                }),
                10 => Operation::MulMod(LargeInOneOut {
                    args_start: LocalIndex::from_usize(local_id),
                    result: LocalId::from_usize((local_id + 3) % num_locals),
                }),
                // Signed operations
                11 => Operation::SDiv(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize((local_id + 1) % num_locals),
                    result: LocalId::from_usize((local_id + 2) % num_locals),
                }),
                12 => Operation::SMod(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize((local_id + 1) % num_locals),
                    result: LocalId::from_usize((local_id + 2) % num_locals),
                }),
                // Bitwise operations
                13 => Operation::And(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize((local_id + 1) % num_locals),
                    result: LocalId::from_usize((local_id + 2) % num_locals),
                }),
                14 => Operation::Or(TwoInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize((local_id + 1) % num_locals),
                    result: LocalId::from_usize((local_id + 2) % num_locals),
                }),
                15 => Operation::Not(OneInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    result: LocalId::from_usize((local_id + 1) % num_locals),
                }),
                // Storage operations
                16 => Operation::SStore(TwoInZeroOut {
                    arg1: LocalId::from_usize(local_id),
                    arg2: LocalId::from_usize((local_id + 1) % num_locals),
                }),
                17 => Operation::SLoad(OneInOneOut {
                    arg1: LocalId::from_usize(local_id),
                    result: LocalId::from_usize((local_id + 1) % num_locals),
                }),
                // Gas operations
                18 => Operation::Gas(ZeroInOneOut {
                    result: LocalId::from_usize(local_id),
                }),
                _ => Operation::LocalSetSmallConst(SetSmallConst {
                    local: LocalId::from_usize(local_id),
                    value: chunk[0] as u64,
                }),
            };

            builder.operations.push(operation);
        }

        builder.build()
    }
}

// Testing infrastructure
struct FuzzTester {
    configs: Vec<TranslatorConfig>,
}

impl FuzzTester {
    fn new() -> Self {
        Self {
            configs: vec![
                TranslatorConfig { enable_bounds_checking: true },
                TranslatorConfig { enable_bounds_checking: false },
            ],
        }
    }

    fn test_program(&self, program: EthIRProgram) {
        for config in &self.configs {
            let mut translator = Translator::with_config(program.clone(), *config);
            match translator.translate() {
                Ok(()) => {
                    let asm = translator.into_asm();
                    if !program.operations.is_empty() {
                        assert!(!asm.is_empty(), "Empty assembly for non-empty program");
                    }
                }
                Err(_) => {
                    // Errors are acceptable - we're testing for panics
                }
            }
        }
    }
}