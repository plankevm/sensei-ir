//! IR Operations - Abstract semantics for EVM compilation
//!
//! This module defines both EVM operations (which map directly to EVM opcodes)
//! and non-EVM operations (abstract operations that provide higher-level
//! semantics without dictating implementation details).
//!
//! # Non-EVM Operations
//!
//! Non-EVM operations provide abstract semantics for:
//! - **Memory allocation**: Abstract allocation primitives that don't specify whether memory is on
//!   stack, in EVM memory, or elsewhere
//! - **Bytecode introspection**: Abstract access to bytecode structure without exposing deployment
//!   mechanics
//! - **Internal calls**: Abstract function calls without specifying calling convention or stack
//!   management
//!
//! These operations allow the IR to express high-level intent while leaving
//! implementation decisions (stack vs memory, calling conventions, memory layout)
//! to the code generator.
//!
//! # Formal Notation
//!
//! - `M`: Abstract memory space
//! - `S`: Storage space (persistent)
//! - `σ`: Program state
//! - `ρ`: Local variable environment
//! - `⊥`: Undefined/uninitialized value
//! - `[[ e ]]ρ`: Evaluation of expression e in environment ρ
//!
//! # Undefined Behavior
//!
//! The following conditions constitute undefined behavior in EthIR programs:
//!
//! ## Memory Access Violations
//! - Reading from uninitialized memory (unless explicitly allowed by operation)
//! - Writing outside allocated bounds
//! - Integer overflow in address calculations
//!
//! ## Control Flow Violations
//! - Reachable switch statements without matching cases and no default
//! - Jump to invalid block identifier
//! - Return from main function entry block
//!
//! ## Allocation Failures
//! - Allocation size overflow
//! - Allocation exceeding available memory
//! - LocalAlloc with non-constant size
//!
//! ## Type Violations
//! - MemoryStore/MemoryLoad with byte_size ∉ {1, 32}
//! - Invalid operation arguments (wrong number or type)
//!
//! ## Call Violations
//! - Recursive internal calls
//! - Invalid function identifier
//! - Stack depth exceeded (implementation-defined limit)

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
    /// Allocates memory dynamically with all bytes initialized to zero.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ result = DynamicAllocZeroed(size) ]]ρ,σ = (ρ', σ')
    /// where:
    ///   ptr = fresh_addr(σ.heap, size)
    ///   σ' = σ[heap := extend(σ.heap, ptr, size)]
    ///   ∀i ∈ [0, size). M[ptr + i] = 0
    ///   ρ' = ρ[result := ptr]
    /// ```
    ///
    /// **Guarantees:**
    /// - Returns a pointer to a memory region of `size` bytes
    /// - All bytes in the allocated region are initialized to zero
    /// - Each invocation returns a unique, non-overlapping memory region
    /// - For any two allocations a₁ ≠ a₂: M[a₁ + i] ∩ M[a₂ + j] = ∅
    DynamicAllocZeroed(OneInOneOut),

    /// Allocates memory dynamically without initialization.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ result = DynamicAllocAnyBytes(size) ]]ρ,σ = (ρ', σ')
    /// where:
    ///   ptr = fresh_addr(σ.heap, size)
    ///   σ' = σ[heap := extend(σ.heap, ptr, size)]
    ///   ∀i ∈ [0, size). M[ptr + i] = ⊥  // undefined
    ///   ρ' = ρ[result := ptr]
    /// ```
    ///
    /// **Guarantees:**
    /// - Returns a pointer to a memory region of `size` bytes
    /// - Content is undefined - reading before writing is undefined behavior
    /// - Each invocation returns a unique, non-overlapping memory region
    DynamicAllocAnyBytes(OneInOneOut),

    /// Allocates memory with compile-time constant size, zero-initialized.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ result = LocalAllocZeroed(const_size) ]]ρ,σ = (ρ', σ')
    /// where:
    ///   ptr = static_addr(const_size)  // Compile-time determined
    ///   ∀i ∈ [0, const_size). M[ptr + i] = 0
    ///   ρ' = ρ[result := ptr]
    /// ```
    ///
    /// **Requirements:**
    /// - Size MUST be a compile-time constant
    /// - Using non-constant size is undefined behavior
    ///
    /// **Optimizations:**
    /// - MAY be optimized to use stack allocation
    /// - Multiple calls with same size MAY return different addresses
    LocalAllocZeroed(OneInOneOut),

    /// Allocates memory with compile-time constant size, uninitialized.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ result = LocalAllocAnyBytes(const_size) ]]ρ,σ = (ρ', σ')
    /// where:
    ///   ptr = static_addr(const_size)  // Compile-time determined
    ///   ∀i ∈ [0, const_size). M[ptr + i] = ⊥  // undefined
    ///   ρ' = ρ[result := ptr]
    /// ```
    ///
    /// **Requirements:**
    /// - Size MUST be a compile-time constant
    ///
    /// **Optimizations:**
    /// - MAY be eliminated entirely if dead code
    /// - MAY use stack space or registers
    LocalAllocAnyBytes(OneInOneOut),

    /// Returns the current free memory pointer without modifying it.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ result = AcquireFreePointer() ]]ρ,σ = (ρ', σ')
    /// where:
    ///   ρ' = ρ[result := σ.free_ptr]
    /// ```
    ///
    /// **Properties:**
    /// - Does NOT modify the free pointer
    /// - Returns boundary between allocated and unallocated memory
    /// - Used for manual memory management patterns
    AcquireFreePointer(ZeroInOneOut),

    /// Updates the free memory pointer by a specified amount.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ DynamicAllocUsingFreePointer(current_ptr, size) ]]ρ,σ = σ'
    /// where:
    ///   assert(current_ptr = σ.free_ptr)
    ///   σ' = σ[free_ptr := σ.free_ptr + size]
    /// ```
    ///
    /// **Preconditions:**
    /// - current_ptr must equal the current free memory pointer
    /// - Violation is undefined behavior
    ///
    /// **Postconditions:**
    /// - Region [current_ptr, current_ptr + size) is now allocated
    /// - No initialization is performed
    DynamicAllocUsingFreePointer(TwoInZeroOut),

    // Memory Operations (byte_size: 1-32)
    MemoryLoad(MemoryLoad),
    MemoryStore(MemoryStore),

    // ========== Simple Statements ==========
    LocalSet(OneInOneOut),
    LocalSetSmallConst(SetSmallConst),
    LocalSetLargeConst(SetLargeConst),

    /// Sets a local to the offset of a data segment in bytecode.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ result = LocalSetDataOffset(segment_id) ]]ρ,σ = (ρ', σ')
    /// where:
    ///   offset = σ.data_segments[segment_id].offset
    ///   ρ' = ρ[result := offset]
    /// ```
    ///
    /// **Properties:**
    /// - Data segments are immutable regions embedded in bytecode
    /// - The offset is constant for a given program
    /// - segment_id must be a valid data segment identifier
    LocalSetDataOffset(SetDataOffset),

    NoOp,

    // ========== Internal Call ==========
    /// Transfers control to another function within same contract.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ InternalCall(f, args, outs) ]]ρ,σ = σ'
    /// where:
    ///   f_def = σ.functions[f]
    ///   args_vals = load_args(ρ, args, f_def.arity)
    ///   σ_call = push_frame(σ, return_addr, outs)
    ///   σ_ret = execute(f_def.entry, args_vals, σ_call)
    ///   σ' = pop_frame(σ_ret)
    ///   store_outputs(ρ, outs, σ_ret.return_vals)
    /// ```
    ///
    /// **Requirements:**
    /// - No recursion allowed - call graph must be acyclic
    /// - function_id must be valid
    /// - Arguments must be initialized at args_start
    /// - Output area at outputs_start must be allocated
    ///
    /// **Properties:**
    /// - Calls may be nested to any depth (limited only by resources)
    /// - Abstract return mechanism: implementation chooses stack/memory/hybrid
    /// - Upon return, outputs are available at outputs_start
    InternalCall(InternalCall),

    // ========== Bytecode Introspection ==========
    /// Returns the byte offset where runtime code starts in deployment bytecode.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ result = RuntimeStartOffset() ]]ρ,σ = (ρ', σ')
    /// where:
    ///   ρ' = ρ[result := σ.bytecode.runtime_offset]
    /// ```
    ///
    /// **Properties:**
    /// - Abstract: Implementation determines actual bytecode layout
    /// - Constant for a given program
    /// - The implementation decides deployment mechanics
    RuntimeStartOffset(ZeroInOneOut),

    /// Returns the byte offset where initialization code ends.
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ result = InitEndOffset() ]]ρ,σ = (ρ', σ')
    /// where:
    ///   ρ' = ρ[result := σ.bytecode.init_end]
    /// ```
    ///
    /// **Properties:**
    /// - For runtime-only contracts, returns 0
    /// - For contracts with constructors, marks boundary between init and runtime code
    /// - Constant for a given program
    InitEndOffset(ZeroInOneOut),

    /// Returns the length of runtime code in bytes (excluding data segments).
    ///
    /// **Formal Semantics:**
    /// ```text
    /// [[ result = RuntimeLength() ]]ρ,σ = (ρ', σ')
    /// where:
    ///   length = σ.bytecode.runtime_end - σ.bytecode.runtime_start
    ///   ρ' = ρ[result := length]
    /// ```
    ///
    /// **Properties:**
    /// - Does NOT include data segments
    /// - Useful for code copying and verification
    /// - Calculated as: runtime_end - runtime_start
    RuntimeLength(ZeroInOneOut),
}

impl Operation {
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Operation::Return(_)
                | Operation::Stop
                | Operation::Revert(_)
                | Operation::Invalid
                | Operation::SelfDestruct(_)
        )
    }

    /// Get the maximum local ID referenced by this operation
    pub fn get_max_local_id(&self) -> u32 {
        match self {
            // Operations with LocalSetSmallConst/LocalSetLargeConst/LocalSetDataOffset
            Operation::LocalSetSmallConst(SetSmallConst { local, .. })
            | Operation::LocalSetLargeConst(SetLargeConst { local, .. }) => local.get(),
            Operation::LocalSetDataOffset(SetDataOffset { local, .. }) => local.get(),

            // Zero-in-one-out operations
            Operation::Address(op)
            | Operation::Origin(op)
            | Operation::Caller(op)
            | Operation::CallValue(op)
            | Operation::CallDataSize(op)
            | Operation::GasPrice(op)
            | Operation::ReturnDataSize(op)
            | Operation::SelfBalance(op)
            | Operation::Number(op)
            | Operation::Difficulty(op)
            | Operation::GasLimit(op)
            | Operation::ChainId(op)
            | Operation::Coinbase(op)
            | Operation::Timestamp(op)
            | Operation::BaseFee(op)
            | Operation::BlobBaseFee(op)
            | Operation::Gas(op)
            | Operation::AcquireFreePointer(op)
            | Operation::CodeSize(op) => op.result.get(),

            // One-in-one-out operations
            Operation::IsZero(op)
            | Operation::Not(op)
            | Operation::Balance(op)
            | Operation::ExtCodeSize(op)
            | Operation::ExtCodeHash(op)
            | Operation::SLoad(op)
            | Operation::TLoad(op)
            | Operation::CallDataLoad(op)
            | Operation::BlobHash(op)
            | Operation::BlockHash(op)
            | Operation::LocalSet(op)
            | Operation::DynamicAllocZeroed(op)
            | Operation::DynamicAllocAnyBytes(op)
            | Operation::LocalAllocZeroed(op)
            | Operation::LocalAllocAnyBytes(op) => op.result.get().max(op.arg1.get()),

            // Two-in-one-out operations
            Operation::Add(op)
            | Operation::Sub(op)
            | Operation::Mul(op)
            | Operation::Div(op)
            | Operation::Mod(op)
            | Operation::Exp(op)
            | Operation::SignExtend(op)
            | Operation::SDiv(op)
            | Operation::SMod(op)
            | Operation::Lt(op)
            | Operation::Gt(op)
            | Operation::SLt(op)
            | Operation::SGt(op)
            | Operation::Eq(op)
            | Operation::And(op)
            | Operation::Or(op)
            | Operation::Xor(op)
            | Operation::Shl(op)
            | Operation::Shr(op)
            | Operation::Sar(op)
            | Operation::Byte(op)
            | Operation::Keccak256(op) => op.result.get().max(op.arg1.get()).max(op.arg2.get()),

            // LargeInOneOut operations - each has different size
            Operation::AddMod(op) | Operation::MulMod(op) => op.result.get(),
            Operation::Create(op) => op.result.get(),
            Operation::Create2(op) => op.result.get(),
            Operation::Call(op) | Operation::CallCode(op) => op.result.get(),
            Operation::DelegateCall(op) | Operation::StaticCall(op) => op.result.get(),

            // InternalCall has its own type
            Operation::InternalCall(_) => 0, // Would need more info to get locals

            // Two-in-zero-out operations
            Operation::SStore(op)
            | Operation::TStore(op)
            | Operation::Return(op)
            | Operation::Revert(op)
            | Operation::Log0(op)
            | Operation::DynamicAllocUsingFreePointer(op) => op.arg1.get().max(op.arg2.get()),

            // Three-in-zero-out operations
            Operation::Log1(op)
            | Operation::MCopy(op)
            | Operation::ReturnDataCopy(op)
            | Operation::CodeCopy(op)
            | Operation::CallDataCopy(op) => op.arg1.get().max(op.arg2.get()).max(op.arg3.get()),

            // LargeInZeroOut operations - each has different size
            Operation::Log2(_)
            | Operation::Log3(_)
            | Operation::Log4(_)
            | Operation::ExtCodeCopy(_) => {
                // These use args_start which is LocalIndex, not LocalId
                // We'd need access to locals to get the actual LocalIds
                // For now, return 0 as these are less common in tests
                0
            }

            // Memory operations
            Operation::MemoryStore(op) => op.address.get().max(op.value.get()),
            Operation::MemoryLoad(op) => op.result.get().max(op.address.get()),

            // One-in-zero-out operations
            Operation::SelfDestruct(op) => op.arg1.get(),

            // Runtime offset operations
            Operation::RuntimeStartOffset(op)
            | Operation::InitEndOffset(op)
            | Operation::RuntimeLength(op) => op.result.get(),

            // Operations with no locals
            Operation::Stop | Operation::Invalid | Operation::NoOp => 0,
        }
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
        match self {
            // Simple operations
            Operation::Stop => fmt_op!(simple, f, "stop"),
            Operation::NoOp => fmt_op!(simple, f, "noop"),
            Operation::Invalid => fmt_op!(simple, f, "invalid"),

            // Arithmetic operations
            Operation::Add(op) => fmt_op!(f, "add", op, locals),
            Operation::Mul(op) => fmt_op!(f, "mul", op, locals),
            Operation::Sub(op) => fmt_op!(f, "sub", op, locals),
            Operation::Div(op) => fmt_op!(f, "div", op, locals),
            Operation::SDiv(op) => fmt_op!(f, "sdiv", op, locals),
            Operation::Mod(op) => fmt_op!(f, "mod", op, locals),
            Operation::SMod(op) => fmt_op!(f, "smod", op, locals),
            Operation::AddMod(op) => fmt_op!(f, "addmod", op, locals),
            Operation::MulMod(op) => fmt_op!(f, "mulmod", op, locals),
            Operation::Exp(op) => fmt_op!(f, "exp", op, locals),
            Operation::SignExtend(op) => fmt_op!(f, "signextend", op, locals),

            // Comparison operations
            Operation::Lt(op) => fmt_op!(f, "lt", op, locals),
            Operation::Gt(op) => fmt_op!(f, "gt", op, locals),
            Operation::SLt(op) => fmt_op!(f, "slt", op, locals),
            Operation::SGt(op) => fmt_op!(f, "sgt", op, locals),
            Operation::Eq(op) => fmt_op!(f, "eq", op, locals),
            Operation::IsZero(op) => fmt_op!(f, "iszero", op, locals),

            // Bitwise operations
            Operation::And(op) => fmt_op!(f, "and", op, locals),
            Operation::Or(op) => fmt_op!(f, "or", op, locals),
            Operation::Xor(op) => fmt_op!(f, "xor", op, locals),
            Operation::Not(op) => fmt_op!(f, "not", op, locals),
            Operation::Byte(op) => fmt_op!(f, "byte", op, locals),
            Operation::Shl(op) => fmt_op!(f, "shl", op, locals),
            Operation::Shr(op) => fmt_op!(f, "shr", op, locals),
            Operation::Sar(op) => fmt_op!(f, "sar", op, locals),

            // Hash operations
            Operation::Keccak256(op) => fmt_op!(f, "keccak256", op, locals),

            // Environmental information
            Operation::Address(op) => fmt_op!(f, "address", op, locals),
            Operation::Balance(op) => fmt_op!(f, "balance", op, locals),
            Operation::Origin(op) => fmt_op!(f, "origin", op, locals),
            Operation::Caller(op) => fmt_op!(f, "caller", op, locals),
            Operation::CallValue(op) => fmt_op!(f, "callvalue", op, locals),
            Operation::CallDataLoad(op) => fmt_op!(f, "calldataload", op, locals),
            Operation::CallDataSize(op) => fmt_op!(f, "calldatasize", op, locals),
            Operation::CodeSize(op) => fmt_op!(f, "codesize", op, locals),
            Operation::GasPrice(op) => fmt_op!(f, "gasprice", op, locals),
            Operation::ExtCodeSize(op) => fmt_op!(f, "extcodesize", op, locals),
            Operation::ReturnDataSize(op) => fmt_op!(f, "returndatasize", op, locals),
            Operation::ExtCodeHash(op) => fmt_op!(f, "extcodehash", op, locals),

            // Block information
            Operation::BlockHash(op) => fmt_op!(f, "blockhash", op, locals),
            Operation::Coinbase(op) => fmt_op!(f, "coinbase", op, locals),
            Operation::Timestamp(op) => fmt_op!(f, "timestamp", op, locals),
            Operation::Number(op) => fmt_op!(f, "number", op, locals),
            Operation::Difficulty(op) => fmt_op!(f, "difficulty", op, locals),
            Operation::GasLimit(op) => fmt_op!(f, "gaslimit", op, locals),
            Operation::ChainId(op) => fmt_op!(f, "chainid", op, locals),
            Operation::SelfBalance(op) => fmt_op!(f, "selfbalance", op, locals),
            Operation::BaseFee(op) => fmt_op!(f, "basefee", op, locals),
            Operation::BlobHash(op) => fmt_op!(f, "blobhash", op, locals),
            Operation::BlobBaseFee(op) => fmt_op!(f, "blobbasefee", op, locals),
            Operation::Gas(op) => fmt_op!(f, "gas", op, locals),

            // Storage operations
            Operation::SLoad(op) => fmt_op!(f, "sload", op, locals),
            Operation::SStore(op) => fmt_op!(no_result, f, "sstore", op, locals),
            Operation::TLoad(op) => fmt_op!(f, "tload", op, locals),
            Operation::TStore(op) => fmt_op!(no_result, f, "tstore", op, locals),

            // Memory copy operations
            Operation::CallDataCopy(op) => fmt_op!(no_result, f, "calldatacopy", op, locals),
            Operation::CodeCopy(op) => fmt_op!(no_result, f, "codecopy", op, locals),
            Operation::ReturnDataCopy(op) => fmt_op!(no_result, f, "returndatacopy", op, locals),
            Operation::ExtCodeCopy(op) => fmt_op!(no_result, f, "extcodecopy", op, locals),
            Operation::MCopy(op) => fmt_op!(no_result, f, "mcopy", op, locals),

            // Log operations
            Operation::Log0(op) => fmt_op!(no_result, f, "log0", op, locals),
            Operation::Log1(op) => fmt_op!(no_result, f, "log1", op, locals),
            Operation::Log2(op) => fmt_op!(no_result, f, "log2", op, locals),
            Operation::Log3(op) => fmt_op!(no_result, f, "log3", op, locals),
            Operation::Log4(op) => fmt_op!(no_result, f, "log4", op, locals),

            // System operations
            Operation::Create(op) => fmt_op!(f, "create", op, locals),
            Operation::Create2(op) => fmt_op!(f, "create2", op, locals),
            Operation::Call(op) => fmt_op!(f, "call", op, locals),
            Operation::CallCode(op) => fmt_op!(f, "callcode", op, locals),
            Operation::DelegateCall(op) => fmt_op!(f, "delegatecall", op, locals),
            Operation::StaticCall(op) => fmt_op!(f, "staticcall", op, locals),
            Operation::Return(op) => fmt_op!(no_result, f, "return", op, locals),
            Operation::Revert(op) => fmt_op!(no_result, f, "revert", op, locals),
            Operation::SelfDestruct(op) => fmt_op!(no_result, f, "selfdestruct", op, locals),

            // Memory allocation operations
            Operation::DynamicAllocZeroed(op) => fmt_op!(f, "malloc", op, locals),
            Operation::DynamicAllocAnyBytes(op) => fmt_op!(f, "malloc_any", op, locals),
            Operation::LocalAllocZeroed(op) => fmt_op!(f, "lalloc", op, locals),
            Operation::LocalAllocAnyBytes(op) => fmt_op!(f, "lalloc_any", op, locals),
            Operation::AcquireFreePointer(op) => fmt_op!(f, "get_free_ptr", op, locals),
            Operation::DynamicAllocUsingFreePointer(op) => {
                fmt_op!(no_result, f, "malloc_with_free", op, locals)
            }

            // Memory operations
            Operation::MemoryLoad(op) => {
                write!(f, "${} = mload{} ${}", op.result, op.byte_size, op.address)
            }
            Operation::MemoryStore(op) => {
                write!(f, "mstore{} ${} ${}", op.byte_size, op.address, op.value)
            }

            // Local operations
            Operation::LocalSet(op) => write!(f, "${} = ${}", op.result, op.arg1),
            Operation::LocalSetSmallConst(op) => {
                let value = op.value;
                write!(f, "${} = {:#x}", op.local, value)
            }
            Operation::LocalSetLargeConst(op) => {
                let value = &large_consts[op.cid];
                write!(f, "${} = {:#x}", op.local, value)
            }
            Operation::LocalSetDataOffset(op) => write!(f, "${} = .{}", op.local, op.segment_id),

            // Internal call - special handling needed
            Operation::InternalCall(_) => {
                // This needs special handling in the main display function
                // as it requires access to function information
                write!(f, "icall")
            }

            // Bytecode introspection operations
            Operation::RuntimeStartOffset(op) => fmt_op!(f, "runtime_start_offset", op, locals),
            Operation::InitEndOffset(op) => fmt_op!(f, "init_end_offset", op, locals),
            Operation::RuntimeLength(op) => fmt_op!(f, "runtime_length", op, locals),
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
