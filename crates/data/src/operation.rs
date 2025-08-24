use crate::index::*;
use std::fmt;

/// Trait for operations that have arguments
pub trait HasArgs {
    /// Get the arguments as a vector of LocalIds
    fn get_args(&self, locals: &IndexSlice<LocalIndex, [LocalId]>) -> Vec<LocalId>;
}

/// Trait for operations that have a result
pub trait HasResult {
    /// Get the result LocalId
    fn get_result(&self) -> LocalId;
}

// Macro to implement traits for operation structs
macro_rules! impl_op_traits {
    // For operations with result and fixed args
    ($type:ty, result: $result:ident, args: [$($arg:ident),*]) => {
        impl HasResult for $type {
            fn get_result(&self) -> LocalId {
                self.$result
            }
        }

        impl HasArgs for $type {
            fn get_args(&self, _locals: &IndexSlice<LocalIndex, [LocalId]>) -> Vec<LocalId> {
                vec![$(self.$arg),*]
            }
        }
    };

    // For operations with only args
    ($type:ty, args: [$($arg:ident),*]) => {
        impl HasArgs for $type {
            fn get_args(&self, _locals: &IndexSlice<LocalIndex, [LocalId]>) -> Vec<LocalId> {
                vec![$(self.$arg),*]
            }
        }
    };

    // For operations with result and no args
    ($type:ty, result: $result:ident) => {
        impl HasResult for $type {
            fn get_result(&self) -> LocalId {
                self.$result
            }
        }

        impl HasArgs for $type {
            fn get_args(&self, _locals: &IndexSlice<LocalIndex, [LocalId]>) -> Vec<LocalId> {
                vec![]
            }
        }
    };
}

#[derive(Debug, Clone)]
pub struct ZeroInOneOut {
    pub result: LocalId,
}
impl_op_traits!(ZeroInOneOut, result: result);

#[derive(Debug, Clone)]
pub struct OneInOneOut {
    pub arg1: LocalId,
    pub result: LocalId,
}
impl_op_traits!(OneInOneOut, result: result, args: [arg1]);

#[derive(Debug, Clone)]
pub struct MemoryLoad {
    pub address: LocalId,
    pub result: LocalId,
    pub byte_size: u8,
}
impl_op_traits!(MemoryLoad, result: result, args: [address]);

#[derive(Debug, Clone)]
pub struct MemoryStore {
    pub address: LocalId,
    pub value: LocalId,
    pub byte_size: u8,
}
impl_op_traits!(MemoryStore, args: [address, value]);

#[derive(Debug, Clone)]
pub struct TwoInOneOut {
    pub arg1: LocalId,
    pub arg2: LocalId,
    pub result: LocalId,
}
impl_op_traits!(TwoInOneOut, result: result, args: [arg1, arg2]);

#[derive(Debug, Clone)]
pub struct OneInZeroOut {
    pub arg1: LocalId,
}
impl_op_traits!(OneInZeroOut, args: [arg1]);

#[derive(Debug, Clone)]
pub struct TwoInZeroOut {
    pub arg1: LocalId,
    pub arg2: LocalId,
}
impl_op_traits!(TwoInZeroOut, args: [arg1, arg2]);

#[derive(Debug, Clone)]
pub struct ThreeInZeroOut {
    pub arg1: LocalId,
    pub arg2: LocalId,
    pub arg3: LocalId,
}
impl_op_traits!(ThreeInZeroOut, args: [arg1, arg2, arg3]);

/// Expects args to be stored contiguously in the IR arena (`args_start..args_start + ARGS_COUNT`).
#[derive(Debug, Clone)]
pub struct LargeInOneOut<const ARGS_COUNT: u32> {
    pub args_start: LocalIndex,
    pub result: LocalId,
}

impl<const ARGS_COUNT: u32> LargeInOneOut<ARGS_COUNT> {
    pub const ARGS_COUNT: u32 = ARGS_COUNT;
}

impl<const ARGS_COUNT: u32> HasResult for LargeInOneOut<ARGS_COUNT> {
    fn get_result(&self) -> LocalId {
        self.result
    }
}

impl<const ARGS_COUNT: u32> HasArgs for LargeInOneOut<ARGS_COUNT> {
    fn get_args(&self, locals: &IndexSlice<LocalIndex, [LocalId]>) -> Vec<LocalId> {
        (0..ARGS_COUNT).map(|i| locals[LocalIndex::new(self.args_start.get() + i)]).collect()
    }
}

/// Expects args to be stored contiguously in the IR arena (`args_start..args_start + ARGS_COUNT`).
#[derive(Debug, Clone)]
pub struct LargeInZeroOut<const ARGS_COUNT: u32> {
    pub args_start: LocalIndex,
}

impl<const ARGS_COUNT: u32> LargeInZeroOut<ARGS_COUNT> {
    pub const ARGS_COUNT: u32 = ARGS_COUNT;
}

impl<const ARGS_COUNT: u32> HasArgs for LargeInZeroOut<ARGS_COUNT> {
    fn get_args(&self, locals: &IndexSlice<LocalIndex, [LocalId]>) -> Vec<LocalId> {
        (0..ARGS_COUNT).map(|i| locals[LocalIndex::new(self.args_start.get() + i)]).collect()
    }
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
    pub cid: LargeConstId,
}

#[derive(Debug, Clone)]
pub struct SetDataOffset {
    pub local: LocalId,
    pub segment_id: DataId,
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
    NoOp,

    // ========== Internal Call ==========
    InternalCall(InternalCall),

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

// Macro for formatting operations
macro_rules! fmt_op {
    // For operations with result and arguments
    ($f:expr, $name:expr, $op:expr, $locals:expr) => {{
        write!($f, "${} = {}", $op.get_result(), $name)?;
        for arg in $op.get_args($locals) {
            write!($f, " ${}", arg)?;
        }
        Ok(())
    }};

    // For operations with only arguments (no result)
    (no_result, $f:expr, $name:expr, $op:expr, $locals:expr) => {{
        write!($f, "{}", $name)?;
        for arg in $op.get_args($locals) {
            write!($f, " ${}", arg)?;
        }
        Ok(())
    }};

    // For simple operations with no args or result
    (simple, $f:expr, $name:expr) => {{ write!($f, "{}", $name) }};
}

impl Operation {
    /// Format the operation
    pub fn fmt_display(
        &self,
        f: &mut fmt::Formatter<'_>,
        locals: &IndexSlice<LocalIndex, [LocalId]>,
        large_consts: &IndexSlice<LargeConstId, [alloy_primitives::U256]>,
    ) -> fmt::Result {
        use Operation as O;

        match self {
            // Simple operations
            O::Stop => fmt_op!(simple, f, "stop"),
            O::NoOp => fmt_op!(simple, f, "noop"),
            O::Invalid => fmt_op!(simple, f, "invalid"),

            // Arithmetic operations
            O::Add(op) => fmt_op!(f, "add", op, locals),
            O::Mul(op) => fmt_op!(f, "mul", op, locals),
            O::Sub(op) => fmt_op!(f, "sub", op, locals),
            O::Div(op) => fmt_op!(f, "div", op, locals),
            O::SDiv(op) => fmt_op!(f, "sdiv", op, locals),
            O::Mod(op) => fmt_op!(f, "mod", op, locals),
            O::SMod(op) => fmt_op!(f, "smod", op, locals),
            O::AddMod(op) => fmt_op!(f, "addmod", op, locals),
            O::MulMod(op) => fmt_op!(f, "mulmod", op, locals),
            O::Exp(op) => fmt_op!(f, "exp", op, locals),
            O::SignExtend(op) => fmt_op!(f, "signextend", op, locals),

            // Comparison operations
            O::Lt(op) => fmt_op!(f, "lt", op, locals),
            O::Gt(op) => fmt_op!(f, "gt", op, locals),
            O::SLt(op) => fmt_op!(f, "slt", op, locals),
            O::SGt(op) => fmt_op!(f, "sgt", op, locals),
            O::Eq(op) => fmt_op!(f, "eq", op, locals),
            O::IsZero(op) => fmt_op!(f, "iszero", op, locals),

            // Bitwise operations
            O::And(op) => fmt_op!(f, "and", op, locals),
            O::Or(op) => fmt_op!(f, "or", op, locals),
            O::Xor(op) => fmt_op!(f, "xor", op, locals),
            O::Not(op) => fmt_op!(f, "not", op, locals),
            O::Byte(op) => fmt_op!(f, "byte", op, locals),
            O::Shl(op) => fmt_op!(f, "shl", op, locals),
            O::Shr(op) => fmt_op!(f, "shr", op, locals),
            O::Sar(op) => fmt_op!(f, "sar", op, locals),

            // Hash operations
            O::Keccak256(op) => fmt_op!(f, "keccak256", op, locals),

            // Environmental information
            O::Address(op) => fmt_op!(f, "address", op, locals),
            O::Balance(op) => fmt_op!(f, "balance", op, locals),
            O::Origin(op) => fmt_op!(f, "origin", op, locals),
            O::Caller(op) => fmt_op!(f, "caller", op, locals),
            O::CallValue(op) => fmt_op!(f, "callvalue", op, locals),
            O::CallDataLoad(op) => fmt_op!(f, "calldataload", op, locals),
            O::CallDataSize(op) => fmt_op!(f, "calldatasize", op, locals),
            O::CodeSize(op) => fmt_op!(f, "codesize", op, locals),
            O::GasPrice(op) => fmt_op!(f, "gasprice", op, locals),
            O::ExtCodeSize(op) => fmt_op!(f, "extcodesize", op, locals),
            O::ReturnDataSize(op) => fmt_op!(f, "returndatasize", op, locals),
            O::ExtCodeHash(op) => fmt_op!(f, "extcodehash", op, locals),

            // Block information
            O::BlockHash(op) => fmt_op!(f, "blockhash", op, locals),
            O::Coinbase(op) => fmt_op!(f, "coinbase", op, locals),
            O::Timestamp(op) => fmt_op!(f, "timestamp", op, locals),
            O::Number(op) => fmt_op!(f, "number", op, locals),
            O::Difficulty(op) => fmt_op!(f, "difficulty", op, locals),
            O::GasLimit(op) => fmt_op!(f, "gaslimit", op, locals),
            O::ChainId(op) => fmt_op!(f, "chainid", op, locals),
            O::SelfBalance(op) => fmt_op!(f, "selfbalance", op, locals),
            O::BaseFee(op) => fmt_op!(f, "basefee", op, locals),
            O::BlobHash(op) => fmt_op!(f, "blobhash", op, locals),
            O::BlobBaseFee(op) => fmt_op!(f, "blobbasefee", op, locals),
            O::Gas(op) => fmt_op!(f, "gas", op, locals),

            // Storage operations
            O::SLoad(op) => fmt_op!(f, "sload", op, locals),
            O::SStore(op) => fmt_op!(no_result, f, "sstore", op, locals),
            O::TLoad(op) => fmt_op!(f, "tload", op, locals),
            O::TStore(op) => fmt_op!(no_result, f, "tstore", op, locals),

            // Memory copy operations
            O::CallDataCopy(op) => fmt_op!(no_result, f, "calldatacopy", op, locals),
            O::CodeCopy(op) => fmt_op!(no_result, f, "codecopy", op, locals),
            O::ReturnDataCopy(op) => fmt_op!(no_result, f, "returndatacopy", op, locals),
            O::ExtCodeCopy(op) => fmt_op!(no_result, f, "extcodecopy", op, locals),
            O::MCopy(op) => fmt_op!(no_result, f, "mcopy", op, locals),

            // Log operations
            O::Log0(op) => fmt_op!(no_result, f, "log0", op, locals),
            O::Log1(op) => fmt_op!(no_result, f, "log1", op, locals),
            O::Log2(op) => fmt_op!(no_result, f, "log2", op, locals),
            O::Log3(op) => fmt_op!(no_result, f, "log3", op, locals),
            O::Log4(op) => fmt_op!(no_result, f, "log4", op, locals),

            // System operations
            O::Create(op) => fmt_op!(f, "create", op, locals),
            O::Create2(op) => fmt_op!(f, "create2", op, locals),
            O::Call(op) => fmt_op!(f, "call", op, locals),
            O::CallCode(op) => fmt_op!(f, "callcode", op, locals),
            O::DelegateCall(op) => fmt_op!(f, "delegatecall", op, locals),
            O::StaticCall(op) => fmt_op!(f, "staticcall", op, locals),
            O::Return(op) => fmt_op!(no_result, f, "return", op, locals),
            O::Revert(op) => fmt_op!(no_result, f, "revert", op, locals),
            O::SelfDestruct(op) => fmt_op!(no_result, f, "selfdestruct", op, locals),

            // Memory allocation operations
            O::DynamicAllocZeroed(op) => fmt_op!(f, "malloc", op, locals),
            O::DynamicAllocAnyBytes(op) => fmt_op!(f, "malloc_any", op, locals),
            O::LocalAllocZeroed(op) => fmt_op!(f, "lalloc", op, locals),
            O::LocalAllocAnyBytes(op) => fmt_op!(f, "lalloc_any", op, locals),
            O::AcquireFreePointer(op) => fmt_op!(f, "get_free_ptr", op, locals),
            O::DynamicAllocUsingFreePointer(op) => {
                fmt_op!(no_result, f, "malloc_with_free", op, locals)
            }

            // Memory operations
            O::MemoryLoad(op) => {
                write!(f, "${} = mload{} ${}", op.result, op.byte_size, op.address)
            }
            O::MemoryStore(op) => {
                write!(f, "mstore{} ${} ${}", op.byte_size, op.address, op.value)
            }

            // Local operations
            O::LocalSet(op) => write!(f, "${} = ${}", op.result, op.arg1),
            O::LocalSetSmallConst(op) => {
                let value = op.value;
                write!(f, "${} = {:#x}", op.local, value)
            }
            O::LocalSetLargeConst(op) => {
                let value = &large_consts[op.cid];
                write!(f, "${} = {:#x}", op.local, value)
            }
            O::LocalSetDataOffset(op) => write!(f, "${} = .{}", op.local, op.segment_id),

            // Internal call - special handling needed
            O::InternalCall(_) => {
                // This needs special handling in the main display function
                // as it requires access to function information
                write!(f, "icall")
            }

            // Bytecode introspection operations
            O::RuntimeStartOffset(op) => fmt_op!(f, "runtime_start_offset", op, locals),
            O::InitEndOffset(op) => fmt_op!(f, "init_end_offset", op, locals),
            O::RuntimeLength(op) => fmt_op!(f, "runtime_length", op, locals),
        }
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
        assert_eq!(std::mem::size_of::<SetDataOffset>(), 8);

        assert_eq!(std::mem::size_of::<Operation>(), 16, "changed desired operation size");
        assert_eq!(std::mem::align_of::<Operation>(), 4, "changed desired operation alignment");
    }
}
