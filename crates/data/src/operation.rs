use crate::index::*;

#[derive(Debug, Clone)]
pub struct ZeroInOneOut {
    pub result: LocalId,
}

#[derive(Debug, Clone)]
pub struct OneInOneOut {
    pub arg1: LocalId,
    pub result: LocalId,
}

#[derive(Debug, Clone)]
pub struct MemoryLoad {
    pub address: LocalId,
    pub result: LocalId,
    pub byte_size: u8,
}

#[derive(Debug, Clone)]
pub struct MemoryStore {
    pub address: LocalId,
    pub value: LocalId,
    pub byte_size: u8,
}

#[derive(Debug, Clone)]
pub struct TwoInOneOut {
    pub arg1: LocalId,
    pub arg2: LocalId,
    pub result: LocalId,
}

#[derive(Debug, Clone)]
pub struct OneInZeroOut {
    pub arg1: LocalId,
}

#[derive(Debug, Clone)]
pub struct TwoInZeroOut {
    pub arg1: LocalId,
    pub arg2: LocalId,
}

#[derive(Debug, Clone)]
pub struct ThreeInZeroOut {
    pub arg1: LocalId,
    pub arg2: LocalId,
    pub arg3: LocalId,
}

/// Expects args to be stored contiguously in the IR arena (`args_start..args_start + ARGS_COUNT`).
#[derive(Debug, Clone)]
pub struct LargeInOneOut<const ARGS_COUNT: u32> {
    pub args_start: LocalIndex,
    pub result: LocalId,
}

/// Expects args to be stored contiguously in the IR arena (`args_start..args_start + ARGS_COUNT`).
#[derive(Debug, Clone)]
pub struct LargeInZeroOut<const ARGS_COUNT: u32> {
    pub args_start: LocalIndex,
}

/// Expects args and outputs to be stored contiguously in the IR arena:
/// - Arguments: `args_start..outputs_start`
/// - Outputs: `outputs_start..outputs_start + functions[function].outputs`
#[derive(Debug, Clone)]
pub struct InternalCall {
    pub function: FunctionId,
    pub args_start: LocalIndex,
    pub outputs_start: LocalIndex,
}

#[derive(Debug, Clone)]
#[repr(C, packed(4))]
pub struct SetSmallConst {
    pub local: LocalId,
    pub value: u64,
}

#[derive(Debug, Clone)]
pub struct SetLargeConst {
    pub local: LocalId,
    pub value: LargeConstId,
}

#[derive(Debug, Clone)]
pub struct SetDataOffset {
    pub local: LocalId,
    pub value: std::ops::Range<DataOffset>,
}

/// All possible IR operations. Modeled such that the alignment of `Operation` is 4 and is no larger
/// than 16 bytes. For operations requiring more data we store an index to the respective IR arena.
/// Operations are either
/// - EVM Opcodes
/// - IR Memory Primitives
/// - Simple Statements (local reassignment / constant assignment)
/// - Internal Call
#[derive(Debug, Clone)]
pub enum Operation {
    // ========== EVM Opcodes ==========
    // Arithmetic Operations
    Add(TwoInOneOut),
    Mul(TwoInOneOut),
    Sub(TwoInOneOut),
    Div(TwoInOneOut),
    SDiv(TwoInOneOut),
    Mod(TwoInOneOut),
    SMod(TwoInOneOut),
    AddMod(LargeInOneOut<3>),
    MulMod(LargeInOneOut<3>),
    Exp(TwoInOneOut),
    SignExtend(TwoInOneOut),

    // Comparison & Bitwise Logic Operations
    Lt(TwoInOneOut),
    Gt(TwoInOneOut),
    SLt(TwoInOneOut),
    SGt(TwoInOneOut),
    Eq(TwoInOneOut),
    IsZero(OneInOneOut),
    And(TwoInOneOut),
    Or(TwoInOneOut),
    Xor(TwoInOneOut),
    Not(OneInOneOut),
    Byte(TwoInOneOut),
    Shl(TwoInOneOut),
    Shr(TwoInOneOut),
    Sar(TwoInOneOut),

    // SHA3
    Keccak256(TwoInOneOut),

    // Environmental Information
    Address(ZeroInOneOut),
    Balance(OneInOneOut),
    Origin(ZeroInOneOut),
    Caller(ZeroInOneOut),
    CallValue(ZeroInOneOut),
    CallDataLoad(OneInOneOut),
    CallDataSize(ZeroInOneOut),
    CallDataCopy(ThreeInZeroOut),
    CodeSize(ZeroInOneOut),
    CodeCopy(ThreeInZeroOut),
    GasPrice(ZeroInOneOut),
    ExtCodeSize(OneInOneOut),
    ExtCodeCopy(LargeInZeroOut<4>),
    ReturnDataSize(ZeroInOneOut),
    ReturnDataCopy(ThreeInZeroOut),
    ExtCodeHash(OneInOneOut),

    // Block Information
    BlockHash(OneInOneOut),
    Coinbase(ZeroInOneOut),
    Timestamp(ZeroInOneOut),
    Number(ZeroInOneOut),
    Difficulty(ZeroInOneOut),
    GasLimit(ZeroInOneOut),
    ChainId(ZeroInOneOut),
    SelfBalance(ZeroInOneOut),
    BaseFee(ZeroInOneOut),
    BlobHash(OneInOneOut),
    BlobBaseFee(ZeroInOneOut),

    // Stack, Memory, Storage and Flow Operations
    SLoad(OneInOneOut),
    SStore(TwoInZeroOut),
    Gas(ZeroInOneOut),
    TLoad(OneInOneOut),
    TStore(TwoInZeroOut),
    MCopy(ThreeInZeroOut),

    // Logging Operations
    Log0(TwoInZeroOut),
    Log1(ThreeInZeroOut),
    Log2(LargeInZeroOut<4>),
    Log3(LargeInZeroOut<5>),
    Log4(LargeInZeroOut<6>),

    // System Operations
    Create(LargeInOneOut<3>),
    Create2(LargeInOneOut<4>),
    Call(LargeInOneOut<7>),
    CallCode(LargeInOneOut<7>),
    DelegateCall(LargeInOneOut<6>),
    StaticCall(LargeInOneOut<6>),
    Return(TwoInZeroOut),
    Stop,
    Revert(TwoInZeroOut),
    Invalid,
    SelfDestruct(OneInZeroOut),

    // ========== IR Memory Primitives ==========
    DynamicAllocZeroed(OneInOneOut),
    DynamicAllocAnyBytes(OneInOneOut),
    LocalAllocZeroed(OneInOneOut),
    LocalAllocAnyBytes(OneInOneOut),
    AcquireFreePointer(ZeroInOneOut),
    DynamicAllocUsingFreePointer(TwoInZeroOut),

    // Memory Operations (byte_size: 1-32)
    MemoryLoad(MemoryLoad),
    MemoryStore(MemoryStore),

    // ========== Simple Statements ==========
    LocalSet(OneInOneOut),
    LocalSetSmallConst(SetSmallConst),
    LocalSetLargeConst(SetLargeConst),
    LocalSetDataOffset(SetDataOffset),

    // ========== Internal Call ==========
    InternalCall(InternalCall),
    InternalReturn(OneInZeroOut),

    // ========== Bytecode Introspection ==========
    RuntimeStartOffset(ZeroInOneOut),
    InitEndOffset(ZeroInOneOut),
    RuntimeLength(ZeroInOneOut),
}

impl Operation {
    pub fn is_terminator(&self) -> bool {
        use Operation as O;
        matches!(self, O::Return(_) | O::Stop | O::Revert(_) | O::Invalid | O::SelfDestruct(_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operation_memory_layout() {
        // Check individual struct sizes
        assert_eq!(std::mem::size_of::<LocalId>(), 4);
        assert_eq!(std::mem::size_of::<ZeroInOneOut>(), 4);
        assert_eq!(std::mem::size_of::<OneInOneOut>(), 8);
        assert_eq!(std::mem::size_of::<TwoInOneOut>(), 12);
        assert_eq!(std::mem::size_of::<MemoryLoad>(), 12);
        assert_eq!(std::mem::size_of::<MemoryStore>(), 12);
        assert_eq!(std::mem::size_of::<SetSmallConst>(), 12);
        assert_eq!(std::mem::size_of::<SetLargeConst>(), 8);
        assert_eq!(std::mem::size_of::<SetDataOffset>(), 12);

        assert_eq!(std::mem::size_of::<Operation>(), 16, "changed desired operation size");
        assert_eq!(std::mem::align_of::<Operation>(), 4, "changed desired operation alignment");
    }
}
