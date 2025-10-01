#![no_main]

use libfuzzer_sys::fuzz_target;
use eth_ir_data::*;
use eth_ir_data::operation::*;
use eth_ir_codegen::Translator;

fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    let program = ProgramGenerator::new(data).generate();
    FuzzTester::new().test_program(program);
});

// Configuration constants
mod config {
    pub const MAX_LOCALS: usize = 100;
    pub const MAX_OPERATIONS: usize = 200;
}

// Main program generator - directly driven by fuzzer input
struct ProgramGenerator<'a> {
    data: &'a [u8],
    offset: usize,
}

impl<'a> ProgramGenerator<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, offset: 0 }
    }

    fn next_byte(&mut self) -> u8 {
        if self.offset >= self.data.len() {
            return 0;
        }
        let byte = self.data[self.offset];
        self.offset += 1;
        byte
    }

    fn next_usize(&mut self, max: usize) -> usize {
        (self.next_byte() as usize % max).max(1)
    }

    fn generate(&mut self) -> EthIRProgram {
        let mut builder = ProgramBuilder::new();

        let num_locals = self.next_usize(config::MAX_LOCALS);
        builder.add_locals(num_locals);

        let num_ops = self.next_usize(config::MAX_OPERATIONS);

        for _ in 0..num_ops {
            if self.offset >= self.data.len() {
                break;
            }

            let op = self.generate_operation(num_locals);
            builder.operations.push(op);
        }

        builder.build()
    }

    fn generate_operation(&mut self, num_locals: usize) -> Operation {
        let byte = self.next_byte();
        let local_id = self.next_byte() as usize % num_locals;

        match byte % 30 {
            0 => Operation::LocalSetSmallConst(SetSmallConst {
                local: LocalId::from_usize(local_id),
                value: self.next_byte() as u64,
            }),
            1 => Operation::Add(self.gen_two_in_one_out(num_locals)),
            2 => Operation::Sub(self.gen_two_in_one_out(num_locals)),
            3 => Operation::Mul(self.gen_two_in_one_out(num_locals)),
            4 => Operation::Div(self.gen_two_in_one_out(num_locals)),
            5 => Operation::Mod(self.gen_two_in_one_out(num_locals)),
            6 => Operation::And(self.gen_two_in_one_out(num_locals)),
            7 => Operation::Or(self.gen_two_in_one_out(num_locals)),
            8 => Operation::Xor(self.gen_two_in_one_out(num_locals)),
            9 => Operation::Not(self.gen_one_in_one_out(num_locals)),
            10 => Operation::Lt(self.gen_two_in_one_out(num_locals)),
            11 => Operation::Gt(self.gen_two_in_one_out(num_locals)),
            12 => Operation::Eq(self.gen_two_in_one_out(num_locals)),
            13 => Operation::IsZero(self.gen_one_in_one_out(num_locals)),
            14 => Operation::Shl(self.gen_two_in_one_out(num_locals)),
            15 => Operation::Shr(self.gen_two_in_one_out(num_locals)),
            16 => Operation::Sar(self.gen_two_in_one_out(num_locals)),
            17 => Operation::SDiv(self.gen_two_in_one_out(num_locals)),
            18 => Operation::SMod(self.gen_two_in_one_out(num_locals)),
            19 => Operation::SLt(self.gen_two_in_one_out(num_locals)),
            20 => Operation::SGt(self.gen_two_in_one_out(num_locals)),
            21 => Operation::SignExtend(self.gen_two_in_one_out(num_locals)),
            22 => Operation::MemoryStore(MemoryStore {
                address: LocalId::from_usize(self.next_byte() as usize % num_locals),
                value: LocalId::from_usize(self.next_byte() as usize % num_locals),
                byte_size: if self.next_byte() % 2 == 0 { 1 } else { 32 },
            }),
            23 => Operation::MemoryLoad(MemoryLoad {
                address: LocalId::from_usize(self.next_byte() as usize % num_locals),
                result: LocalId::from_usize(self.next_byte() as usize % num_locals),
                byte_size: 32,
            }),
            24 => Operation::DynamicAllocZeroed(self.gen_one_in_one_out(num_locals)),
            25 => Operation::DynamicAllocAnyBytes(self.gen_one_in_one_out(num_locals)),
            26 => Operation::LocalAllocZeroed(self.gen_one_in_one_out(num_locals)),
            27 => Operation::LocalAllocAnyBytes(self.gen_one_in_one_out(num_locals)),
            28 => Operation::NoOp,
            _ => Operation::Stop,
        }
    }

    fn gen_two_in_one_out(&mut self, num_locals: usize) -> TwoInOneOut {
        TwoInOneOut {
            arg1: LocalId::from_usize(self.next_byte() as usize % num_locals),
            arg2: LocalId::from_usize(self.next_byte() as usize % num_locals),
            result: LocalId::from_usize(self.next_byte() as usize % num_locals),
        }
    }

    fn gen_one_in_one_out(&mut self, num_locals: usize) -> OneInOneOut {
        OneInOneOut {
            arg1: LocalId::from_usize(self.next_byte() as usize % num_locals),
            result: LocalId::from_usize(self.next_byte() as usize % num_locals),
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


// Testing infrastructure
struct FuzzTester;

impl FuzzTester {
    fn new() -> Self {
        Self
    }

    fn test_program(&self, program: EthIRProgram) {
        let mut translator = Translator::new(program.clone());
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