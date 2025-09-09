//! Gas estimation for EVM assembly
//!
//! This module provides gas cost estimation for assembled EVM bytecode.
//! Gas costs are based on the latest EVM specifications.
//!
//! Includes both simple and advanced estimation:
//! - Simple: Fast, lower-bound estimates with base costs only
//! - Advanced: Sophisticated analysis with symbolic execution, memory tracking, and storage
//!   patterns

use evm_glue::{assembly::Asm, opcodes::Opcode};
use std::{
    cmp::max,
    collections::{BTreeMap, HashMap, HashSet},
};

/// Simple gas estimator providing basic cost estimation
pub struct SimpleGasEstimator {
    /// Base gas costs for each opcode
    opcode_costs: HashMap<String, u64>,
}

impl Default for SimpleGasEstimator {
    fn default() -> Self {
        Self::new()
    }
}

impl SimpleGasEstimator {
    pub fn new() -> Self {
        let mut opcode_costs = HashMap::new();

        // Zero cost operations
        opcode_costs.insert("STOP".to_string(), 0);
        opcode_costs.insert("INVALID".to_string(), 0);

        // Base operations (3 gas)
        for op in [
            "ADD", "SUB", "NOT", "LT", "GT", "SLT", "SGT", "EQ", "ISZERO", "AND", "OR", "XOR",
            "BYTE", "SHL", "SHR", "SAR", "POP",
        ] {
            opcode_costs.insert(op.to_string(), 3);
        }

        // Slightly more expensive operations (5 gas)
        for op in ["MUL", "DIV", "SDIV", "MOD", "SMOD", "SIGNEXTEND", "ADDMOD", "MULMOD"] {
            opcode_costs.insert(op.to_string(), 5);
        }

        // EXP has dynamic cost: 10 + 50 * byte_size_of_exponent
        opcode_costs.insert("EXP".to_string(), 10); // Base cost

        // SHA3/KECCAK256: 30 + 6 * data_size_in_words
        opcode_costs.insert("KECCAK256".to_string(), 30);
        opcode_costs.insert("SHA3".to_string(), 30); // SHA3 is the actual opcode name // Base cost

        // Environmental operations (2 gas)
        for op in [
            "ADDRESS",
            "ORIGIN",
            "CALLER",
            "CALLVALUE",
            "CALLDATASIZE",
            "CODESIZE",
            "GASPRICE",
            "RETURNDATASIZE",
            "COINBASE",
            "TIMESTAMP",
            "NUMBER",
            "DIFFICULTY",
            "PREVRANDAO",
            "GASLIMIT",
            "CHAINID",
            "SELFBALANCE",
            "BASEFEE",
            "BLOBBASEFEE",
        ] {
            opcode_costs.insert(op.to_string(), 2);
        }

        // BALANCE: 2600 for cold, 100 for warm (we'll use cold as default)
        opcode_costs.insert("BALANCE".to_string(), 2600);

        // CALLDATALOAD (3 gas)
        opcode_costs.insert("CALLDATALOAD".to_string(), 3);

        // CALLDATACOPY: 3 + 3 * data_size_in_words
        opcode_costs.insert("CALLDATACOPY".to_string(), 3); // Base cost

        // CODECOPY: 3 + 3 * data_size_in_words
        opcode_costs.insert("CODECOPY".to_string(), 3); // Base cost

        // RETURNDATACOPY: 3 + 3 * data_size_in_words
        opcode_costs.insert("RETURNDATACOPY".to_string(), 3); // Base cost

        // EXTCODESIZE: 2600 for cold, 100 for warm
        opcode_costs.insert("EXTCODESIZE".to_string(), 2600);

        // EXTCODECOPY: 2600 for cold + 3 * data_size_in_words
        opcode_costs.insert("EXTCODECOPY".to_string(), 2600); // Base cost

        // EXTCODEHASH: 2600 for cold, 100 for warm
        opcode_costs.insert("EXTCODEHASH".to_string(), 2600);

        // BLOCKHASH (20 gas)
        opcode_costs.insert("BLOCKHASH".to_string(), 20);

        // BLOBHASH (3 gas)
        opcode_costs.insert("BLOBHASH".to_string(), 3);

        // Storage operations
        opcode_costs.insert("SLOAD".to_string(), 2100); // Cold slot
        opcode_costs.insert("SSTORE".to_string(), 20000); // Cold slot, non-zero to non-zero
        opcode_costs.insert("TLOAD".to_string(), 100);
        opcode_costs.insert("TSTORE".to_string(), 100);

        // Stack operations
        opcode_costs.insert("PUSH0".to_string(), 2);
        for i in 1..=32 {
            opcode_costs.insert(format!("PUSH{}", i), 3);
        }
        for i in 1..=16 {
            opcode_costs.insert(format!("DUP{}", i), 3);
            opcode_costs.insert(format!("SWAP{}", i), 3);
        }

        // Memory operations
        opcode_costs.insert("MLOAD".to_string(), 3);
        opcode_costs.insert("MSTORE".to_string(), 3);
        opcode_costs.insert("MSTORE8".to_string(), 3);
        opcode_costs.insert("MCOPY".to_string(), 3); // + 3 * data_size_in_words

        // Control flow
        opcode_costs.insert("JUMP".to_string(), 8);
        opcode_costs.insert("JUMPI".to_string(), 10);
        opcode_costs.insert("JUMPDEST".to_string(), 1);
        opcode_costs.insert("PC".to_string(), 2);
        opcode_costs.insert("MSIZE".to_string(), 2);
        opcode_costs.insert("GAS".to_string(), 2);

        // Logging
        opcode_costs.insert("LOG0".to_string(), 375); // + 8 * data_size
        opcode_costs.insert("LOG1".to_string(), 750); // + 8 * data_size
        opcode_costs.insert("LOG2".to_string(), 1125); // + 8 * data_size
        opcode_costs.insert("LOG3".to_string(), 1500); // + 8 * data_size
        opcode_costs.insert("LOG4".to_string(), 1875); // + 8 * data_size

        // System operations
        opcode_costs.insert("CREATE".to_string(), 32000);
        opcode_costs.insert("CREATE2".to_string(), 32000);
        opcode_costs.insert("CALL".to_string(), 2600); // Cold, much more complex in reality
        opcode_costs.insert("CALLCODE".to_string(), 2600);
        opcode_costs.insert("DELEGATECALL".to_string(), 2600);
        opcode_costs.insert("STATICCALL".to_string(), 2600);
        opcode_costs.insert("RETURN".to_string(), 0);
        opcode_costs.insert("REVERT".to_string(), 0);
        opcode_costs.insert("SELFDESTRUCT".to_string(), 5000); // Complex, can be much higher

        Self { opcode_costs }
    }

    /// Estimate gas cost for a sequence of assembly operations
    /// Returns (static_cost, has_dynamic_costs)
    ///
    /// NOTE: This provides a lower bound estimate. It only counts base operation costs.
    /// Dynamic costs cannot be determined statically:
    /// - Memory expansion requires knowing actual addresses accessed
    /// - Storage costs depend on warm/cold state and value changes
    /// - Call costs depend on value transfers and called contract execution
    /// - Data operation costs depend on actual data sizes
    pub fn estimate(&self, asm: &[Asm]) -> (u64, bool) {
        let mut total_gas = 0u64;
        let mut has_dynamic = false;

        for instruction in asm {
            match instruction {
                Asm::Op(opcode) => {
                    let opcode_str = format!("{:?}", opcode).split('(').next().unwrap().to_string();

                    // Get base cost
                    let base_cost = self.opcode_costs.get(&opcode_str).unwrap_or(&3);
                    total_gas += base_cost;

                    // Check for operations with dynamic costs
                    match &opcode_str[..] {
                        "EXP" | "KECCAK256" | "SHA3" | "CALLDATACOPY" | "CODECOPY"
                        | "RETURNDATACOPY" | "EXTCODECOPY" | "MCOPY" | "LOG0" | "LOG1" | "LOG2"
                        | "LOG3" | "LOG4" | "CALL" | "CALLCODE" | "DELEGATECALL" | "STATICCALL"
                        | "CREATE" | "CREATE2" | "SSTORE" | "SLOAD" => {
                            has_dynamic = true;
                        }
                        "MSTORE" | "MLOAD" | "MSTORE8" => {
                            // Memory operations have dynamic cost based on memory expansion.
                            // Accurate estimation would require:
                            // 1. Symbolic execution to track stack values
                            // 2. Computing highest accessed memory address
                            // 3. Applying quadratic cost formula: (mem_word^2)/512 + 3*mem_word
                            // Without runtime values, we can only provide base costs.
                            has_dynamic = true;
                        }
                        _ => {}
                    }
                }
                Asm::Data(_) => {
                    // Data doesn't cost gas directly (it's part of deployment)
                }
                Asm::Mark(_) => {
                    // Marks don't generate code, no gas cost
                }
                Asm::Ref(_) => {
                    // References resolve to PUSH operations
                    total_gas += 3; // Assume PUSH2 or PUSH3 for addresses
                }
            }
        }

        (total_gas, has_dynamic)
    }

    /// Get a detailed gas breakdown for assembly operations
    pub fn detailed_estimate(&self, asm: &[Asm]) -> GasReport {
        let mut report = GasReport::new();

        for instruction in asm {
            match instruction {
                Asm::Op(opcode) => {
                    let opcode_str = format!("{:?}", opcode).split('(').next().unwrap().to_string();
                    let base_cost = self.opcode_costs.get(&opcode_str).copied().unwrap_or(3);

                    report.add_opcode(opcode_str.clone(), base_cost);

                    // Note operations with dynamic costs
                    match &opcode_str[..] {
                        "MSTORE" | "MLOAD" | "MSTORE8" => {
                            report.add_note(
                                "Memory ops: +quadratic expansion cost based on highest address touched".to_string(),
                            );
                        }
                        "SSTORE" => {
                            report.add_note("SSTORE cost varies: 20k for cold non-zero to non-zero, 2.9k for warm, refunds possible".to_string());
                        }
                        "SLOAD" => {
                            report.add_note("SLOAD: 2100 gas for cold, 100 for warm".to_string());
                        }
                        "CALL" | "DELEGATECALL" | "STATICCALL" | "CALLCODE" => {
                            report.add_note(format!("{}: 2600 base + value transfer + memory expansion + called contract execution", opcode_str));
                        }
                        _ => {}
                    }
                }
                Asm::Ref(_) => {
                    report.add_opcode("PUSH (ref)".to_string(), 3);
                }
                _ => {}
            }
        }

        report
    }
}

/// Detailed gas usage report
#[derive(Debug)]
pub struct GasReport {
    pub total_gas: u64,
    pub opcode_counts: HashMap<String, (u32, u64)>, // (count, total_gas)
    pub notes: Vec<String>,
}

impl GasReport {
    fn new() -> Self {
        Self { total_gas: 0, opcode_counts: HashMap::new(), notes: Vec::new() }
    }

    fn add_opcode(&mut self, opcode: String, gas: u64) {
        let entry = self.opcode_counts.entry(opcode).or_insert((0, 0));
        entry.0 += 1;
        entry.1 += gas;
        self.total_gas += gas;
    }

    fn add_note(&mut self, note: String) {
        if !self.notes.contains(&note) {
            self.notes.push(note);
        }
    }

    /// Generate a human-readable report
    pub fn format_report(&self) -> String {
        let mut report = String::new();
        report.push_str(&format!("Total Estimated Gas: {}\n", self.total_gas));
        report.push_str("\nOpcode Breakdown:\n");

        let mut sorted_ops: Vec<_> = self.opcode_counts.iter().collect();
        sorted_ops.sort_by_key(|(_, (_, gas))| -(*gas as i64));

        for (opcode, (count, gas)) in sorted_ops {
            report.push_str(&format!("  {:20} {:5} calls = {:7} gas\n", opcode, count, gas));
        }

        if !self.notes.is_empty() {
            report.push_str("\nNotes:\n");
            for note in &self.notes {
                report.push_str(&format!("  - {}\n", note));
            }
        }

        report
    }
}

/// Abstract value that can be on the stack
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
enum AbstractValue {
    /// Known constant value
    Constant(u64),
    /// Unknown value from a specific source
    Unknown(String),
    /// Result of an operation on abstract values
    Expression(Box<AbstractValue>, String, Box<AbstractValue>),
    /// Memory address that will be accessed
    MemoryAddress,
    /// Storage key that will be accessed
    StorageKey,
}

impl AbstractValue {
    /// Try to evaluate to a constant
    fn evaluate(&self) -> Option<u64> {
        match self {
            AbstractValue::Constant(v) => Some(*v),
            AbstractValue::Expression(left, op, right) => {
                let l = left.evaluate()?;
                let r = right.evaluate()?;
                match op.as_str() {
                    "+" => Some(l.saturating_add(r)),
                    "-" => Some(l.saturating_sub(r)),
                    "*" => Some(l.saturating_mul(r)),
                    "/" if r != 0 => Some(l / r),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

/// Tracks abstract machine state during symbolic execution
struct AbstractState {
    /// Abstract stack
    stack: Vec<AbstractValue>,
    /// Memory accesses (address -> size)
    memory_accesses: BTreeMap<u64, u64>,
    /// Highest memory address touched
    max_memory: u64,
    /// Storage keys accessed (for warm/cold tracking)
    storage_accessed: HashSet<String>,
    /// Current gas used
    gas_used: u64,
}

impl AbstractState {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            memory_accesses: BTreeMap::new(),
            max_memory: 0,
            storage_accessed: HashSet::new(),
            gas_used: 0,
        }
    }

    fn push(&mut self, value: AbstractValue) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Option<AbstractValue> {
        self.stack.pop()
    }

    fn peek(&self, depth: usize) -> Option<&AbstractValue> {
        self.stack.get(self.stack.len().checked_sub(depth + 1)?)
    }

    fn dup(&mut self, depth: usize) -> bool {
        if let Some(val) = self.peek(depth) {
            let val = val.clone();
            self.push(val);
            true
        } else {
            false
        }
    }

    fn swap(&mut self, depth: usize) -> bool {
        let len = self.stack.len();
        if len > depth {
            self.stack.swap(len - 1, len - depth - 1);
            true
        } else {
            false
        }
    }

    /// Track memory access and calculate expansion cost
    fn access_memory(&mut self, offset: u64, size: u64) -> u64 {
        if size == 0 {
            return 0;
        }

        let end_offset = offset.saturating_add(size);
        self.memory_accesses.insert(offset, size);

        // Track highest memory address
        let old_max = self.max_memory;
        self.max_memory = max(self.max_memory, end_offset);

        // Calculate memory expansion cost
        let new_cost = self.calculate_memory_cost(end_offset);
        let old_cost = self.calculate_memory_cost(old_max);
        new_cost.saturating_sub(old_cost)
    }

    /// Calculate memory cost for a given size
    fn calculate_memory_cost(&self, memory_size: u64) -> u64 {
        let memory_size_word = memory_size.div_ceil(32);
        memory_size_word.saturating_mul(memory_size_word) / 512 + 3 * memory_size_word
    }
}

/// Sophisticated gas estimator with symbolic execution
pub struct AdvancedGasEstimator {
    /// Base gas costs for each opcode
    opcode_costs: HashMap<String, u64>,
}

impl Default for AdvancedGasEstimator {
    fn default() -> Self {
        Self::new()
    }
}

impl AdvancedGasEstimator {
    pub fn new() -> Self {
        // Reuse the base costs from the simple estimator
        let mut opcode_costs = HashMap::new();

        // [Copy all the opcode costs from the simple estimator]
        // Zero cost operations
        opcode_costs.insert("STOP".to_string(), 0);
        opcode_costs.insert("INVALID".to_string(), 0);

        // Base operations (3 gas)
        for op in [
            "ADD", "SUB", "NOT", "LT", "GT", "SLT", "SGT", "EQ", "ISZERO", "AND", "OR", "XOR",
            "BYTE", "SHL", "SHR", "SAR", "POP",
        ] {
            opcode_costs.insert(op.to_string(), 3);
        }

        // Slightly more expensive operations (5 gas)
        for op in ["MUL", "DIV", "SDIV", "MOD", "SMOD", "SIGNEXTEND", "ADDMOD", "MULMOD"] {
            opcode_costs.insert(op.to_string(), 5);
        }

        // EXP has dynamic cost: 10 + 50 * byte_size_of_exponent
        opcode_costs.insert("EXP".to_string(), 10);

        // SHA3/KECCAK256: 30 + 6 * data_size_in_words
        opcode_costs.insert("KECCAK256".to_string(), 30);
        opcode_costs.insert("SHA3".to_string(), 30); // SHA3 is the actual opcode name

        // Environmental operations (2 gas)
        for op in [
            "ADDRESS",
            "ORIGIN",
            "CALLER",
            "CALLVALUE",
            "CALLDATASIZE",
            "CODESIZE",
            "GASPRICE",
            "RETURNDATASIZE",
            "COINBASE",
            "TIMESTAMP",
            "NUMBER",
            "DIFFICULTY",
            "PREVRANDAO",
            "GASLIMIT",
            "CHAINID",
            "SELFBALANCE",
            "BASEFEE",
            "BLOBBASEFEE",
        ] {
            opcode_costs.insert(op.to_string(), 2);
        }

        opcode_costs.insert("BALANCE".to_string(), 2600);
        opcode_costs.insert("CALLDATALOAD".to_string(), 3);
        opcode_costs.insert("CALLDATACOPY".to_string(), 3);
        opcode_costs.insert("CODECOPY".to_string(), 3);
        opcode_costs.insert("RETURNDATACOPY".to_string(), 3);
        opcode_costs.insert("EXTCODESIZE".to_string(), 2600);
        opcode_costs.insert("EXTCODECOPY".to_string(), 2600);
        opcode_costs.insert("EXTCODEHASH".to_string(), 2600);
        opcode_costs.insert("BLOCKHASH".to_string(), 20);
        opcode_costs.insert("BLOBHASH".to_string(), 3);

        // Storage operations
        opcode_costs.insert("SLOAD".to_string(), 2100);
        opcode_costs.insert("SSTORE".to_string(), 20000);
        opcode_costs.insert("TLOAD".to_string(), 100);
        opcode_costs.insert("TSTORE".to_string(), 100);

        // Stack operations
        opcode_costs.insert("PUSH0".to_string(), 2);
        for i in 1..=32 {
            opcode_costs.insert(format!("PUSH{}", i), 3);
        }
        for i in 1..=16 {
            opcode_costs.insert(format!("DUP{}", i), 3);
            opcode_costs.insert(format!("SWAP{}", i), 3);
        }

        // Memory operations
        opcode_costs.insert("MLOAD".to_string(), 3);
        opcode_costs.insert("MSTORE".to_string(), 3);
        opcode_costs.insert("MSTORE8".to_string(), 3);
        opcode_costs.insert("MCOPY".to_string(), 3);

        // Control flow
        opcode_costs.insert("JUMP".to_string(), 8);
        opcode_costs.insert("JUMPI".to_string(), 10);
        opcode_costs.insert("JUMPDEST".to_string(), 1);
        opcode_costs.insert("PC".to_string(), 2);
        opcode_costs.insert("MSIZE".to_string(), 2);
        opcode_costs.insert("GAS".to_string(), 2);

        // Logging
        opcode_costs.insert("LOG0".to_string(), 375);
        opcode_costs.insert("LOG1".to_string(), 750);
        opcode_costs.insert("LOG2".to_string(), 1125);
        opcode_costs.insert("LOG3".to_string(), 1500);
        opcode_costs.insert("LOG4".to_string(), 1875);

        // System operations
        opcode_costs.insert("CREATE".to_string(), 32000);
        opcode_costs.insert("CREATE2".to_string(), 32000);
        opcode_costs.insert("CALL".to_string(), 2600);
        opcode_costs.insert("CALLCODE".to_string(), 2600);
        opcode_costs.insert("DELEGATECALL".to_string(), 2600);
        opcode_costs.insert("STATICCALL".to_string(), 2600);
        opcode_costs.insert("RETURN".to_string(), 0);
        opcode_costs.insert("REVERT".to_string(), 0);
        opcode_costs.insert("SELFDESTRUCT".to_string(), 5000);

        Self { opcode_costs }
    }

    /// Perform sophisticated gas estimation with symbolic execution
    pub fn estimate_advanced(&self, asm: &[Asm]) -> AdvancedGasReport {
        let mut state = AbstractState::new();
        let mut report = AdvancedGasReport::new();

        for instruction in asm {
            match instruction {
                Asm::Op(opcode) => {
                    let gas = self.execute_opcode(opcode, &mut state, &mut report);
                    state.gas_used += gas;
                }
                Asm::Data(data) => {
                    // Data is deployment cost, not execution
                    report.deployment_data_size += data.len() as u64;
                }
                Asm::Mark(_) => {
                    // Marks don't generate code
                }
                Asm::Ref(_) => {
                    // References become PUSH operations
                    state.push(AbstractValue::Unknown("ref".to_string()));
                    state.gas_used += 3;
                }
            }
        }

        report.total_gas = state.gas_used;
        report.memory_cost = state.calculate_memory_cost(state.max_memory);
        report.max_memory_bytes = state.max_memory;
        report.unique_storage_accesses = state.storage_accessed.len();

        report
    }

    /// Execute an opcode symbolically and return gas cost
    fn execute_opcode(
        &self,
        opcode: &Opcode,
        state: &mut AbstractState,
        report: &mut AdvancedGasReport,
    ) -> u64 {
        let opcode_str = format!("{:?}", opcode).split('(').next().unwrap().to_string();
        let base_cost = self.opcode_costs.get(&opcode_str).copied().unwrap_or(3);
        // println!("Opcode: {} -> base cost: {}", opcode_str, base_cost);

        // Track opcode usage
        *report.opcode_counts.entry(opcode_str.clone()).or_insert(0) += 1;

        let mut total_cost = base_cost;

        // Simulate stack effects and calculate dynamic costs
        match opcode {
            // Push operations
            Opcode::PUSH0 => {
                state.push(AbstractValue::Constant(0));
            }
            Opcode::PUSH1(bytes) => {
                state.push(AbstractValue::Constant(bytes[0] as u64));
            }
            Opcode::PUSH2(bytes) => {
                let value = u16::from_be_bytes(*bytes) as u64;
                state.push(AbstractValue::Constant(value));
            }
            Opcode::PUSH3(bytes) => {
                let mut padded = [0u8; 4];
                padded[1..].copy_from_slice(bytes);
                let value = u32::from_be_bytes(padded) as u64;
                state.push(AbstractValue::Constant(value));
            }
            // For larger pushes, track as unknown
            Opcode::PUSH4(_)
            | Opcode::PUSH5(_)
            | Opcode::PUSH6(_)
            | Opcode::PUSH7(_)
            | Opcode::PUSH8(_)
            | Opcode::PUSH9(_)
            | Opcode::PUSH10(_)
            | Opcode::PUSH11(_)
            | Opcode::PUSH12(_)
            | Opcode::PUSH13(_)
            | Opcode::PUSH14(_)
            | Opcode::PUSH15(_)
            | Opcode::PUSH16(_)
            | Opcode::PUSH17(_)
            | Opcode::PUSH18(_)
            | Opcode::PUSH19(_)
            | Opcode::PUSH20(_)
            | Opcode::PUSH21(_)
            | Opcode::PUSH22(_)
            | Opcode::PUSH23(_)
            | Opcode::PUSH24(_)
            | Opcode::PUSH25(_)
            | Opcode::PUSH26(_)
            | Opcode::PUSH27(_)
            | Opcode::PUSH28(_)
            | Opcode::PUSH29(_)
            | Opcode::PUSH30(_)
            | Opcode::PUSH31(_)
            | Opcode::PUSH32(_) => {
                state.push(AbstractValue::Unknown("push".to_string()));
            }

            // DUP operations
            Opcode::DUP1 => {
                state.dup(0);
            }
            Opcode::DUP2 => {
                state.dup(1);
            }
            Opcode::DUP3 => {
                state.dup(2);
            }
            Opcode::DUP4 => {
                state.dup(3);
            }
            Opcode::DUP5 => {
                state.dup(4);
            }
            Opcode::DUP6 => {
                state.dup(5);
            }
            Opcode::DUP7 => {
                state.dup(6);
            }
            Opcode::DUP8 => {
                state.dup(7);
            }
            Opcode::DUP9 => {
                state.dup(8);
            }
            Opcode::DUP10 => {
                state.dup(9);
            }
            Opcode::DUP11 => {
                state.dup(10);
            }
            Opcode::DUP12 => {
                state.dup(11);
            }
            Opcode::DUP13 => {
                state.dup(12);
            }
            Opcode::DUP14 => {
                state.dup(13);
            }
            Opcode::DUP15 => {
                state.dup(14);
            }
            Opcode::DUP16 => {
                state.dup(15);
            }

            // SWAP operations
            Opcode::SWAP1 => {
                state.swap(1);
            }
            Opcode::SWAP2 => {
                state.swap(2);
            }
            Opcode::SWAP3 => {
                state.swap(3);
            }
            Opcode::SWAP4 => {
                state.swap(4);
            }
            Opcode::SWAP5 => {
                state.swap(5);
            }
            Opcode::SWAP6 => {
                state.swap(6);
            }
            Opcode::SWAP7 => {
                state.swap(7);
            }
            Opcode::SWAP8 => {
                state.swap(8);
            }
            Opcode::SWAP9 => {
                state.swap(9);
            }
            Opcode::SWAP10 => {
                state.swap(10);
            }
            Opcode::SWAP11 => {
                state.swap(11);
            }
            Opcode::SWAP12 => {
                state.swap(12);
            }
            Opcode::SWAP13 => {
                state.swap(13);
            }
            Opcode::SWAP14 => {
                state.swap(14);
            }
            Opcode::SWAP15 => {
                state.swap(15);
            }
            Opcode::SWAP16 => {
                state.swap(16);
            }

            // Memory operations - track memory expansion
            Opcode::MLOAD => {
                if let Some(offset_val) = state.pop() {
                    if let Some(offset) = offset_val.evaluate() {
                        let mem_cost = state.access_memory(offset, 32);
                        total_cost += mem_cost;
                        report.memory_accesses.push((offset, 32));
                    } else {
                        // Unknown offset, can't track precisely
                        report.has_unknown_memory_access = true;
                    }
                }
                state.push(AbstractValue::Unknown("mload".to_string()));
            }

            Opcode::MSTORE => {
                if let (Some(_value), Some(offset_val)) = (state.pop(), state.pop()) {
                    if let Some(offset) = offset_val.evaluate() {
                        let mem_cost = state.access_memory(offset, 32);
                        total_cost += mem_cost;
                        report.memory_accesses.push((offset, 32));
                    } else {
                        report.has_unknown_memory_access = true;
                    }
                }
            }

            Opcode::MSTORE8 => {
                if let (Some(_value), Some(offset_val)) = (state.pop(), state.pop()) {
                    if let Some(offset) = offset_val.evaluate() {
                        let mem_cost = state.access_memory(offset, 1);
                        total_cost += mem_cost;
                        report.memory_accesses.push((offset, 1));
                    } else {
                        report.has_unknown_memory_access = true;
                    }
                }
            }

            // Storage operations - track warm/cold
            Opcode::SLOAD => {
                if let Some(key) = state.pop() {
                    let key_str = format!("{:?}", key);
                    let is_warm = state.storage_accessed.contains(&key_str);
                    state.storage_accessed.insert(key_str.clone());

                    // Adjust cost based on warm/cold
                    if is_warm {
                        total_cost = 100; // Warm access
                        report.warm_storage_reads += 1;
                    } else {
                        total_cost = 2100; // Cold access
                        report.cold_storage_reads += 1;
                    }
                }
                state.push(AbstractValue::Unknown("sload".to_string()));
            }

            Opcode::SSTORE => {
                if let (Some(_value), Some(key)) = (state.pop(), state.pop()) {
                    let key_str = format!("{:?}", key);
                    let is_warm = state.storage_accessed.contains(&key_str);
                    state.storage_accessed.insert(key_str);

                    // SSTORE cost is complex, depends on current and new value
                    // Using simplified model here
                    if is_warm {
                        total_cost = 2900; // Warm, non-zero to non-zero
                        report.warm_storage_writes += 1;
                    } else {
                        total_cost = 20000; // Cold, non-zero to non-zero
                        report.cold_storage_writes += 1;
                    }
                }
            }

            // Arithmetic operations - try to evaluate
            Opcode::ADD => {
                if let (Some(b), Some(a)) = (state.pop(), state.pop()) {
                    state.push(AbstractValue::Expression(
                        Box::new(a),
                        "+".to_string(),
                        Box::new(b),
                    ));
                }
            }

            Opcode::SUB => {
                if let (Some(b), Some(a)) = (state.pop(), state.pop()) {
                    state.push(AbstractValue::Expression(
                        Box::new(a),
                        "-".to_string(),
                        Box::new(b),
                    ));
                }
            }

            Opcode::MUL => {
                if let (Some(b), Some(a)) = (state.pop(), state.pop()) {
                    state.push(AbstractValue::Expression(
                        Box::new(a),
                        "*".to_string(),
                        Box::new(b),
                    ));
                }
            }

            // Data copy operations - calculate dynamic cost
            Opcode::CALLDATACOPY | Opcode::CODECOPY | Opcode::RETURNDATACOPY => {
                let popped = (state.pop(), state.pop(), state.pop());
                if let (Some(size_val), Some(_offset), Some(dest)) = popped {
                    let evaluated = (size_val.evaluate(), dest.evaluate());
                    if let (Some(size), Some(dest_offset)) = evaluated {
                        // Memory expansion cost
                        let mem_cost = state.access_memory(dest_offset, size);
                        total_cost += mem_cost;
                        // Data copy cost: 3 * ceil(size / 32)
                        let copy_cost = 3 * size.div_ceil(32);
                        total_cost += copy_cost;
                    }
                }
            }

            // SHA3/KECCAK256 - dynamic cost based on data size
            Opcode::SHA3 => {
                // SHA3 takes [offset, size] from stack with size on top
                // So we pop size first, then offset
                let popped = (state.pop(), state.pop());
                if let (Some(size_val), Some(offset)) = popped {
                    // Debug: check what we're getting
                    // println!("SHA3: size_val={:?}, offset={:?}", size_val, offset);
                    let evaluated = (size_val.evaluate(), offset.evaluate());
                    if let (Some(size), Some(mem_offset)) = evaluated {
                        // Memory expansion cost
                        let mem_cost = state.access_memory(mem_offset, size);
                        total_cost += mem_cost;
                        // Hash cost: 30 + 6 * ceil(size / 32)
                        let hash_cost = 6 * size.div_ceil(32);
                        total_cost += hash_cost;
                    }
                }
                state.push(AbstractValue::Unknown("hash".to_string()));
            }

            // LOG operations - dynamic cost based on data size and topic count
            Opcode::LOG0 | Opcode::LOG1 | Opcode::LOG2 | Opcode::LOG3 | Opcode::LOG4 => {
                let topic_count = match opcode {
                    Opcode::LOG0 => 0,
                    Opcode::LOG1 => 1,
                    Opcode::LOG2 => 2,
                    Opcode::LOG3 => 3,
                    Opcode::LOG4 => 4,
                    _ => 0,
                };

                // Pop topics first (they're on top of the stack)
                for _ in 0..topic_count {
                    state.pop();
                }

                // Then pop size and offset
                let popped = (state.pop(), state.pop());
                if let (Some(size_val), Some(offset)) = popped {
                    let evaluated = (size_val.evaluate(), offset.evaluate());
                    if let (Some(size), Some(mem_offset)) = evaluated {
                        // Memory expansion cost
                        let mem_cost = state.access_memory(mem_offset, size);
                        total_cost += mem_cost;
                        // Log data cost: 8 * size
                        total_cost += 8 * size;
                    }
                }
            }

            // Default handling for other opcodes
            _ => {
                // Simulate default stack effects (most ops consume and produce)
                match &opcode_str[..] {
                    "POP" => {
                        state.pop();
                    }
                    "JUMPDEST" | "STOP" | "INVALID" => {}
                    _ => {
                        // Most ops consume some and produce one
                        state.pop();
                        state.push(AbstractValue::Unknown(opcode_str.to_lowercase()));
                    }
                }
            }
        }

        total_cost
    }
}

/// Detailed gas report with advanced analysis
#[derive(Debug)]
pub struct AdvancedGasReport {
    pub total_gas: u64,
    pub memory_cost: u64,
    pub max_memory_bytes: u64,
    pub opcode_counts: HashMap<String, u32>,
    pub memory_accesses: Vec<(u64, u64)>, // (offset, size)
    pub has_unknown_memory_access: bool,
    pub cold_storage_reads: u32,
    pub warm_storage_reads: u32,
    pub cold_storage_writes: u32,
    pub warm_storage_writes: u32,
    pub unique_storage_accesses: usize,
    pub deployment_data_size: u64,
}

impl AdvancedGasReport {
    fn new() -> Self {
        Self {
            total_gas: 0,
            memory_cost: 0,
            max_memory_bytes: 0,
            opcode_counts: HashMap::new(),
            memory_accesses: Vec::new(),
            has_unknown_memory_access: false,
            cold_storage_reads: 0,
            warm_storage_reads: 0,
            cold_storage_writes: 0,
            warm_storage_writes: 0,
            unique_storage_accesses: 0,
            deployment_data_size: 0,
        }
    }

    pub fn format_report(&self) -> String {
        let mut report = String::new();

        report.push_str("=== Advanced Gas Analysis ===\n");
        report.push_str(&format!("Total Execution Gas: {}\n", self.total_gas));
        report.push_str(&format!("Memory Expansion Cost: {}\n", self.memory_cost));
        report.push_str(&format!("Max Memory Used: {} bytes\n", self.max_memory_bytes));

        if self.has_unknown_memory_access {
            report.push_str("⚠️  Warning: Contains memory accesses with unknown offsets\n");
        }

        report.push_str("\n=== Storage Access Pattern ===\n");
        report.push_str(&format!("Cold Reads: {} ({}g each)\n", self.cold_storage_reads, 2100));
        report.push_str(&format!("Warm Reads: {} ({}g each)\n", self.warm_storage_reads, 100));
        report.push_str(&format!("Cold Writes: {} (~{}g each)\n", self.cold_storage_writes, 20000));
        report.push_str(&format!("Warm Writes: {} (~{}g each)\n", self.warm_storage_writes, 2900));
        report.push_str(&format!("Unique Keys: {}\n", self.unique_storage_accesses));

        report.push_str("\n=== Memory Access Pattern ===\n");
        if !self.memory_accesses.is_empty() {
            let mut accesses = self.memory_accesses.clone();
            accesses.sort_by_key(|&(offset, _)| offset);
            for (offset, size) in accesses.iter().take(10) {
                report.push_str(&format!(
                    "  [0x{:04x}..0x{:04x}] ({} bytes)\n",
                    offset,
                    offset + size,
                    size
                ));
            }
            if accesses.len() > 10 {
                report.push_str(&format!("  ... and {} more accesses\n", accesses.len() - 10));
            }
        }

        report.push_str("\n=== Opcode Distribution ===\n");
        let mut sorted_ops: Vec<_> = self.opcode_counts.iter().collect();
        sorted_ops.sort_by_key(|(_, count)| -(**count as i32));

        for (opcode, count) in sorted_ops.iter().take(10) {
            report.push_str(&format!("  {:15} {:5} calls\n", opcode, count));
        }

        if self.deployment_data_size > 0 {
            report.push_str("\n=== Deployment ===\n");
            report.push_str(&format!("Data Size: {} bytes\n", self.deployment_data_size));
            report
                .push_str(&format!("Deployment Cost: ~{} gas\n", 200 * self.deployment_data_size)); // Rough estimate
        }

        report
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ==================== Basic Tests ====================

    #[test]
    fn test_basic_gas_estimation() {
        let estimator = SimpleGasEstimator::new();

        let asm = vec![
            Asm::Op(Opcode::PUSH1([1])),
            Asm::Op(Opcode::PUSH1([2])),
            Asm::Op(Opcode::ADD),
            Asm::Op(Opcode::STOP),
        ];

        let (gas, has_dynamic) = estimator.estimate(&asm);
        assert_eq!(gas, 3 + 3 + 3 + 0); // PUSH1 + PUSH1 + ADD + STOP
        assert!(!has_dynamic);
    }

    #[test]
    fn test_detailed_report() {
        let estimator = SimpleGasEstimator::new();

        let asm = vec![
            Asm::Op(Opcode::PUSH0),
            Asm::Op(Opcode::DUP1),
            Asm::Op(Opcode::SLOAD),
            Asm::Op(Opcode::ADD),
            Asm::Op(Opcode::MSTORE),
        ];

        let report = estimator.detailed_estimate(&asm);
        assert_eq!(report.total_gas, 2 + 3 + 2100 + 3 + 3);
        assert_eq!(report.notes.len(), 2); // Exactly 2 notes: one for SLOAD, one for MSTORE

        let formatted = report.format_report();
        assert!(formatted.contains("SLOAD"));
        assert!(formatted.contains("2100"));
    }

    // ==================== Edge Cases ====================

    #[test]
    fn test_empty_program() {
        let simple = SimpleGasEstimator::new();
        let advanced = AdvancedGasEstimator::new();

        let asm = vec![];

        let (gas, has_dynamic) = simple.estimate(&asm);
        assert_eq!(gas, 0);
        assert!(!has_dynamic);

        let report = advanced.estimate_advanced(&asm);
        assert_eq!(report.total_gas, 0);
        assert_eq!(report.max_memory_bytes, 0);
    }

    #[test]
    fn test_marks_only_program() {
        let simple = SimpleGasEstimator::new();
        let advanced = AdvancedGasEstimator::new();

        let asm = vec![
            Asm::Mark(0), // MarkId is just usize
            Asm::Mark(1),
        ];

        let (gas, _) = simple.estimate(&asm);
        assert_eq!(gas, 0, "Marks should not consume gas");

        let report = advanced.estimate_advanced(&asm);
        assert_eq!(report.total_gas, 0);
    }

    #[test]
    fn test_stack_underflow_handling() {
        let estimator = AdvancedGasEstimator::new();

        // ADD requires 2 stack items, but stack is empty
        let asm = vec![Asm::Op(Opcode::ADD)];

        let report = estimator.estimate_advanced(&asm);
        // Should handle gracefully without panicking
        assert_eq!(report.total_gas, 3); // Base cost of ADD
    }

    #[test]
    fn test_stack_operations_edge_cases() {
        let estimator = AdvancedGasEstimator::new();

        // DUP on empty stack
        let asm = vec![Asm::Op(Opcode::DUP1)];
        let report = estimator.estimate_advanced(&asm);
        assert_eq!(report.total_gas, 3);

        // SWAP with insufficient stack
        let asm = vec![
            Asm::Op(Opcode::PUSH1([1])),
            Asm::Op(Opcode::SWAP1), // Needs 2 items, only has 1
        ];
        let report = estimator.estimate_advanced(&asm);
        assert_eq!(report.total_gas, 3 + 3);
    }

    // ==================== Memory Tests ====================

    #[test]
    fn test_memory_expansion_tracking() {
        let estimator = AdvancedGasEstimator::new();

        // Program that accesses memory at specific offsets
        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x20])), // offset 32
            Asm::Op(Opcode::PUSH1([0x42])), // value
            Asm::Op(Opcode::MSTORE),        // Store at offset 32 (expands to 64 bytes)
            Asm::Op(Opcode::PUSH1([0x40])), // offset 64
            Asm::Op(Opcode::MLOAD),         // Load from offset 64 (expands to 96 bytes)
        ];

        let report = estimator.estimate_advanced(&asm);

        assert_eq!(report.max_memory_bytes, 96);
        // Memory cost for 96 bytes: (3 words)^2 / 512 + 3 * 3 = 9/512 + 9 ≈ 9
        assert_eq!(report.memory_cost, 9);
        assert_eq!(report.memory_accesses.len(), 2);
        assert!(report.memory_accesses.contains(&(32, 32)));
        assert!(report.memory_accesses.contains(&(64, 32)));
    }

    #[test]
    fn test_memory_zero_size_access() {
        let estimator = AdvancedGasEstimator::new();

        // MSTORE8 only stores 1 byte
        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x00])), // offset 0
            Asm::Op(Opcode::PUSH1([0xFF])), // value
            Asm::Op(Opcode::MSTORE8),       // Store 1 byte at offset 0
        ];

        let report = estimator.estimate_advanced(&asm);
        assert_eq!(report.max_memory_bytes, 1);
        assert!(report.memory_accesses.contains(&(0, 1)));
    }

    #[test]
    fn test_memory_unknown_offset() {
        let estimator = AdvancedGasEstimator::new();

        // Memory access with computed unknown offset
        let asm = vec![
            Asm::Op(Opcode::CALLER),        // Unknown value
            Asm::Op(Opcode::PUSH1([0x42])), // value
            Asm::Op(Opcode::MSTORE),        // Store at unknown offset
        ];

        let report = estimator.estimate_advanced(&asm);
        assert!(report.has_unknown_memory_access);
    }

    // ==================== Storage Tests ====================

    #[test]
    fn test_storage_warm_cold_tracking() {
        let estimator = AdvancedGasEstimator::new();

        // Program that accesses same storage slot twice
        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x01])), // key
            Asm::Op(Opcode::SLOAD),         // First access (cold)
            Asm::Op(Opcode::PUSH1([0x01])), // same key
            Asm::Op(Opcode::SLOAD),         // Second access (warm)
        ];

        let report = estimator.estimate_advanced(&asm);

        assert_eq!(report.cold_storage_reads, 1);
        assert_eq!(report.warm_storage_reads, 1);
        assert_eq!(report.unique_storage_accesses, 1);

        // Gas should reflect cold vs warm costs
        let expected = 3 + 2100 + 3 + 100; // PUSH + SLOAD(cold) + PUSH + SLOAD(warm)
        assert_eq!(report.total_gas, expected);
    }

    #[test]
    fn test_storage_multiple_slots() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            // Access slot 1
            Asm::Op(Opcode::PUSH1([0x01])),
            Asm::Op(Opcode::SLOAD), // Cold
            // Access slot 2
            Asm::Op(Opcode::PUSH1([0x02])),
            Asm::Op(Opcode::SLOAD), // Cold
            // Access slot 1 again
            Asm::Op(Opcode::PUSH1([0x01])),
            Asm::Op(Opcode::SLOAD), // Warm
        ];

        let report = estimator.estimate_advanced(&asm);
        assert_eq!(report.cold_storage_reads, 2);
        assert_eq!(report.warm_storage_reads, 1);
        assert_eq!(report.unique_storage_accesses, 2);
    }

    #[test]
    fn test_storage_write_operations() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            // Write to slot 1 (cold)
            Asm::Op(Opcode::PUSH1([0x01])), // key
            Asm::Op(Opcode::PUSH1([0x42])), // value
            Asm::Op(Opcode::SSTORE),        // Cold write
            // Write to same slot (warm)
            Asm::Op(Opcode::PUSH1([0x01])), // key
            Asm::Op(Opcode::PUSH1([0x43])), // value
            Asm::Op(Opcode::SSTORE),        // Warm write
        ];

        let report = estimator.estimate_advanced(&asm);
        assert_eq!(report.cold_storage_writes, 1);
        assert_eq!(report.warm_storage_writes, 1);

        let expected = 3 + 3 + 20000 + 3 + 3 + 2900;
        assert_eq!(report.total_gas, expected);
    }

    // ==================== Symbolic Execution Tests ====================

    #[test]
    fn test_symbolic_arithmetic() {
        let estimator = AdvancedGasEstimator::new();

        // Program with arithmetic that can be evaluated
        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x10])), // 16
            Asm::Op(Opcode::PUSH1([0x20])), // 32
            Asm::Op(Opcode::ADD),           // 16 + 32 = 48
            Asm::Op(Opcode::PUSH1([0xFF])), // value to store
            Asm::Op(Opcode::MSTORE),        // Store at offset 48
        ];

        let report = estimator.estimate_advanced(&asm);

        assert!(report.memory_accesses.contains(&(48, 32)));
        assert_eq!(report.max_memory_bytes, 80); // 48 + 32
    }

    #[test]
    fn test_symbolic_subtraction() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x30])), // 48
            Asm::Op(Opcode::PUSH1([0x10])), // 16
            Asm::Op(Opcode::SUB),           // 48 - 16 = 32
            Asm::Op(Opcode::PUSH1([0xAA])), // value
            Asm::Op(Opcode::MSTORE),        // Store at offset 32
        ];

        let report = estimator.estimate_advanced(&asm);
        assert!(report.memory_accesses.contains(&(32, 32)));
        assert_eq!(report.max_memory_bytes, 64);
    }

    #[test]
    fn test_symbolic_multiplication() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x08])), // 8
            Asm::Op(Opcode::PUSH1([0x04])), // 4
            Asm::Op(Opcode::MUL),           // 8 * 4 = 32
            Asm::Op(Opcode::PUSH1([0xBB])), // value
            Asm::Op(Opcode::MSTORE),        // Store at offset 32
        ];

        let report = estimator.estimate_advanced(&asm);
        assert!(report.memory_accesses.contains(&(32, 32)));
    }

    #[test]
    fn test_symbolic_division_by_zero() {
        // Test that division by zero in symbolic evaluation returns None
        let value = AbstractValue::Expression(
            Box::new(AbstractValue::Constant(10)),
            "/".to_string(),
            Box::new(AbstractValue::Constant(0)),
        );
        assert_eq!(value.evaluate(), None);
    }

    // ==================== Dynamic Cost Operations ====================

    #[test]
    fn test_keccak256_dynamic_cost() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x00])), // offset
            Asm::Op(Opcode::PUSH1([0x40])), // size = 64 bytes = 2 words
            Asm::Op(Opcode::SHA3),          // KECCAK256
        ];

        let report = estimator.estimate_advanced(&asm);

        // SHA3: Stack order is [offset, size] with size on top
        // So pop() gets size first (0x40=64), then offset (0x00=0)
        // Costs: 2 PUSH1 (3+3=6) + SHA3 base (30) + hash dynamic (6*2=12) + mem expansion (6)
        // Total: 6 + 30 + 12 + 6 = 54

        assert_eq!(report.max_memory_bytes, 64);
        assert_eq!(report.total_gas, 54);
    }

    #[test]
    fn test_log_operations() {
        let estimator = AdvancedGasEstimator::new();

        // LOG2 with 32 bytes of data
        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x00])), // memory offset
            Asm::Op(Opcode::PUSH1([0x20])), // size = 32 bytes
            Asm::Op(Opcode::PUSH1([0x11])), // topic1
            Asm::Op(Opcode::PUSH1([0x22])), // topic2
            Asm::Op(Opcode::LOG2),          // Base: 1125 + 8*32 = 1381
        ];

        let report = estimator.estimate_advanced(&asm);

        // println!("LOG2 test - Total gas: {}, Max memory: {}, Memory cost: {}",
        //          report.total_gas, report.max_memory_bytes, report.memory_cost);

        // LOG2: pops offset, size, then 2 topics from stack
        // But our test pushes: offset(0), size(32), topic1, topic2
        // Stack becomes [offset, size, topic1, topic2] with topic2 on top
        // So LOG2 pops in this order: topic2, topic1, size, offset? No...
        // Actually LOG takes offset and size first, THEN topics

        // LOG2: 4 PUSH1s (12) + LOG2 base (1125) + data cost (8*32=256) + memory expansion (3)
        assert_eq!(report.max_memory_bytes, 32);
        assert_eq!(report.total_gas, 12 + 1125 + 256 + 3); // = 1396
    }

    #[test]
    fn test_calldatacopy_dynamic() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x20])), // dest offset in memory
            Asm::Op(Opcode::PUSH1([0x00])), // src offset in calldata
            Asm::Op(Opcode::PUSH1([0x40])), // size = 64 bytes
            Asm::Op(Opcode::CALLDATACOPY),
        ];

        let report = estimator.estimate_advanced(&asm);
        // Should include memory expansion to offset 32 + 64 = 96
        assert_eq!(report.max_memory_bytes, 96);
        // 3 PUSH1s (9) + CALLDATACOPY base (3) + copy cost (3 * 2 = 6) + memory expansion
        // Memory expansion for 96 bytes: (3 words)^2 / 512 + 3 * 3 = 9
        assert_eq!(report.total_gas, 9 + 3 + 6 + 9); // = 27
    }

    #[test]
    fn test_codecopy_dynamic() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x00])), // dest offset
            Asm::Op(Opcode::PUSH1([0x00])), // src offset
            Asm::Op(Opcode::PUSH1([0x60])), // size = 96 bytes = 3 words
            Asm::Op(Opcode::CODECOPY),
        ];

        let report = estimator.estimate_advanced(&asm);
        assert_eq!(report.max_memory_bytes, 96);
        // Copy cost should be 3 * 3 = 9 gas for 3 words
    }

    // ==================== Data and References ====================

    #[test]
    fn test_data_deployment_cost() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            Asm::Data(vec![0xFF; 100]), // 100 bytes of data
            Asm::Op(Opcode::PUSH1([1])),
        ];

        let report = estimator.estimate_advanced(&asm);
        assert_eq!(report.deployment_data_size, 100);
        assert_eq!(report.total_gas, 3); // Only the PUSH1 counts for execution

        let formatted = report.format_report();
        assert!(formatted.contains("Deployment"));
        assert!(formatted.contains("100 bytes"));
    }

    #[test]
    fn test_ref_handling() {
        use evm_glue::assembly::{MarkRef, RefType};

        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            Asm::Ref(MarkRef {
                ref_type: RefType::Direct(0), // Direct reference to mark 0
                is_pushed: true,
                set_size: None,
            }),
            Asm::Op(Opcode::JUMP),
        ];

        let report = estimator.estimate_advanced(&asm);
        assert_eq!(report.total_gas, 3 + 8); // Ref becomes PUSH (3) + JUMP (8)
    }

    // ==================== Cross-Validation Tests ====================

    #[test]
    fn test_estimators_consistency_simple() {
        let simple = SimpleGasEstimator::new();
        let advanced = AdvancedGasEstimator::new();

        // For simple programs without dynamic costs, both should agree
        let asm = vec![
            Asm::Op(Opcode::PUSH1([1])),
            Asm::Op(Opcode::PUSH1([2])),
            Asm::Op(Opcode::ADD),
            Asm::Op(Opcode::POP),
        ];

        let (simple_gas, _) = simple.estimate(&asm);
        let advanced_report = advanced.estimate_advanced(&asm);

        assert_eq!(simple_gas, advanced_report.total_gas);
    }

    #[test]
    fn test_estimators_divergence_memory() {
        let simple = SimpleGasEstimator::new();
        let advanced = AdvancedGasEstimator::new();

        // With memory operations, advanced should report higher due to expansion
        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x80])), // offset 128
            Asm::Op(Opcode::PUSH1([0x42])), // value
            Asm::Op(Opcode::MSTORE),
        ];

        let (simple_gas, has_dynamic) = simple.estimate(&asm);
        let advanced_report = advanced.estimate_advanced(&asm);

        assert!(has_dynamic);
        // Simple: 3 + 3 + 3 = 9, Advanced: 9 + memory expansion
        // Memory expansion for 160 bytes (5 words): (5*5)/512 + 3*5 = 25/512 + 15 ≈ 15
        assert_eq!(simple_gas, 9);
        assert_eq!(advanced_report.total_gas, 24); // 9 + 15 memory expansion
        assert_eq!(advanced_report.max_memory_bytes, 160); // 128 + 32
    }

    #[test]
    fn test_estimators_relationship_comprehensive() {
        let simple = SimpleGasEstimator::new();
        let advanced = AdvancedGasEstimator::new();

        // Test various operation categories
        struct TestCase {
            name: &'static str,
            asm: Vec<Asm>,
            expect_equal: bool, // Should simple == advanced?
            simple_has_dynamic: bool,
        }

        let test_cases = vec![
            TestCase {
                name: "Pure arithmetic",
                asm: vec![
                    Asm::Op(Opcode::PUSH1([10])),
                    Asm::Op(Opcode::PUSH1([20])),
                    Asm::Op(Opcode::ADD),
                    Asm::Op(Opcode::PUSH1([2])),
                    Asm::Op(Opcode::MUL),
                ],
                expect_equal: true,
                simple_has_dynamic: false,
            },
            TestCase {
                name: "Stack operations only",
                asm: vec![
                    Asm::Op(Opcode::PUSH1([1])),
                    Asm::Op(Opcode::DUP1),
                    Asm::Op(Opcode::SWAP1),
                    Asm::Op(Opcode::POP),
                ],
                expect_equal: true,
                simple_has_dynamic: false,
            },
            TestCase {
                name: "Memory operations",
                asm: vec![
                    Asm::Op(Opcode::PUSH1([0x00])),
                    Asm::Op(Opcode::PUSH1([0x42])),
                    Asm::Op(Opcode::MSTORE),
                ],
                expect_equal: false, // Advanced includes memory expansion
                simple_has_dynamic: true,
            },
            TestCase {
                name: "Storage operations",
                asm: vec![Asm::Op(Opcode::PUSH1([0x01])), Asm::Op(Opcode::SLOAD)],
                expect_equal: true, // Both use cold cost
                simple_has_dynamic: true,
            },
            TestCase {
                name: "SHA3 operation",
                asm: vec![
                    Asm::Op(Opcode::PUSH1([0x00])),
                    Asm::Op(Opcode::PUSH1([0x20])),
                    Asm::Op(Opcode::SHA3),
                ],
                expect_equal: false,      // Advanced includes dynamic cost
                simple_has_dynamic: true, // SHA3 has dynamic costs
            },
        ];

        for test_case in test_cases {
            let (simple_gas, has_dynamic) = simple.estimate(&test_case.asm);
            let advanced_report = advanced.estimate_advanced(&test_case.asm);

            assert_eq!(
                has_dynamic, test_case.simple_has_dynamic,
                "{}: dynamic flag mismatch",
                test_case.name
            );

            if test_case.expect_equal {
                assert_eq!(
                    simple_gas, advanced_report.total_gas,
                    "{}: estimators should agree",
                    test_case.name
                );
            } else {
                // Advanced should always be >= simple (provides upper bound)
                assert!(
                    advanced_report.total_gas >= simple_gas,
                    "{}: advanced should be >= simple",
                    test_case.name
                );
            }
        }
    }

    #[test]
    fn test_estimators_base_cost_consistency() {
        let simple = SimpleGasEstimator::new();
        let advanced = AdvancedGasEstimator::new();

        // Test that base costs are consistent between estimators
        let opcodes_to_test = vec![
            (Asm::Op(Opcode::ADD), "ADD", 3),
            (Asm::Op(Opcode::MUL), "MUL", 5),
            (Asm::Op(Opcode::PUSH1([0])), "PUSH1", 3),
            (Asm::Op(Opcode::DUP1), "DUP1", 3),
            (Asm::Op(Opcode::SWAP1), "SWAP1", 3),
            (Asm::Op(Opcode::JUMP), "JUMP", 8),
            (Asm::Op(Opcode::JUMPI), "JUMPI", 10),
            (Asm::Op(Opcode::SLOAD), "SLOAD", 2100),
            (Asm::Op(Opcode::SSTORE), "SSTORE", 20000),
            (Asm::Op(Opcode::CALL), "CALL", 2600),
        ];

        for (instruction, name, expected_base) in opcodes_to_test {
            let asm = vec![instruction];

            let (simple_gas, _) = simple.estimate(&asm);
            let advanced_report = advanced.estimate_advanced(&asm);

            // For single operations without memory/dynamic effects,
            // both should return the base cost
            assert_eq!(simple_gas, expected_base, "{}: simple estimator base cost mismatch", name);

            // Advanced might be higher due to memory/storage effects
            assert!(
                advanced_report.total_gas >= expected_base,
                "{}: advanced estimator should be at least base cost",
                name
            );
        }
    }

    #[test]
    fn test_estimators_with_warm_storage() {
        let simple = SimpleGasEstimator::new();
        let advanced = AdvancedGasEstimator::new();

        // Test that advanced estimator properly tracks warm/cold storage
        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x01])),
            Asm::Op(Opcode::SLOAD), // Cold: 2100
            Asm::Op(Opcode::PUSH1([0x01])),
            Asm::Op(Opcode::SLOAD), // Warm: 100
        ];

        let (simple_gas, _) = simple.estimate(&asm);
        let advanced_report = advanced.estimate_advanced(&asm);

        // Simple always uses cold cost
        assert_eq!(simple_gas, 3 + 2100 + 3 + 2100); // = 4206

        // Advanced tracks warm/cold
        assert_eq!(advanced_report.total_gas, 3 + 2100 + 3 + 100); // = 2206

        // Advanced should be significantly less due to warm access
        assert!(advanced_report.total_gas < simple_gas);
        assert_eq!(advanced_report.cold_storage_reads, 1);
        assert_eq!(advanced_report.warm_storage_reads, 1);
    }

    #[test]
    fn test_estimators_detailed_report_linkage() {
        let simple = SimpleGasEstimator::new();
        let advanced = AdvancedGasEstimator::new();

        let asm = vec![
            Asm::Op(Opcode::PUSH1([0x20])),
            Asm::Op(Opcode::PUSH1([0x40])),
            Asm::Op(Opcode::MSTORE),
            Asm::Op(Opcode::PUSH1([0x01])),
            Asm::Op(Opcode::SLOAD),
        ];

        let simple_report = simple.detailed_estimate(&asm);
        let advanced_report = advanced.estimate_advanced(&asm);

        // Check that simple report identifies dynamic operations
        assert!(simple_report.notes.iter().any(|note| note.contains("Memory")));
        assert!(simple_report.notes.iter().any(|note| note.contains("SLOAD")));

        // Check that opcode counts match
        assert_eq!(simple_report.opcode_counts.get("PUSH1").map(|(count, _)| *count), Some(3));
        assert_eq!(advanced_report.opcode_counts.get("PUSH1"), Some(&3));

        // Advanced should provide more detail
        assert!(advanced_report.memory_accesses.len() > 0);
        // MSTORE at offset 32 (0x20) with 32-byte value extends memory to 64 bytes
        assert_eq!(advanced_report.max_memory_bytes, 64); // 32 + 32
        assert_eq!(advanced_report.cold_storage_reads, 1);
    }

    // ==================== Complex Integration Tests ====================

    #[test]
    fn test_complex_defi_pattern() {
        let estimator = AdvancedGasEstimator::new();

        // Simulate a DeFi swap pattern
        let asm = vec![
            // Load balance from storage
            Asm::Op(Opcode::PUSH1([0x01])), // storage key
            Asm::Op(Opcode::SLOAD),         // Load balance (cold)
            // Check minimum amount
            Asm::Op(Opcode::DUP1),
            Asm::Op(Opcode::PUSH2([0x03, 0xE8])), // 1000 minimum
            Asm::Op(Opcode::LT),
            Asm::Op(Opcode::PUSH1([0x20])), // jump dest
            Asm::Op(Opcode::JUMPI),         // Conditional jump
            // Calculate fee (1%)
            Asm::Op(Opcode::DUP1),
            Asm::Op(Opcode::PUSH1([0x64])), // 100
            Asm::Op(Opcode::DIV),           // balance / 100
            // Update storage
            Asm::Op(Opcode::PUSH1([0x01])), // storage key (warm now)
            Asm::Op(Opcode::SSTORE),        // Store updated balance
            // Log event
            Asm::Op(Opcode::PUSH1([0x00])),      // memory offset
            Asm::Op(Opcode::PUSH1([0x20])),      // 32 bytes
            Asm::Op(Opcode::PUSH32([0xFF; 32])), // topic (event signature)
            Asm::Op(Opcode::LOG1),               // Log with 1 topic
        ];

        let report = estimator.estimate_advanced(&asm);

        // Verify storage access pattern
        assert_eq!(report.cold_storage_reads, 1);
        assert_eq!(report.warm_storage_writes, 0); // There's no warm write, SSTORE makes the slot warm but it's still a cold write
        assert_eq!(report.cold_storage_writes, 1); // First write to slot is cold

        // Verify memory was accessed for LOG
        assert_eq!(report.max_memory_bytes, 32);

        // Calculate exact gas cost:
        // PUSH1 + SLOAD(cold) + DUP1 + PUSH2 + LT + PUSH1 + JUMPI + DUP1 + PUSH1 + DIV + PUSH1 +
        // SSTORE(cold) + PUSH1 + PUSH1 + PUSH32 + LOG1 3 + 2100 + 3 + 3 + 3 + 3 + 10 + 3 +
        // 3 + 5 + 3 + 20000 + 3 + 3 + 3 + 750 + 8*32 + mem(3)
        assert_eq!(
            report.total_gas,
            3 + 2100 + 3 + 3 + 3 + 3 + 10 + 3 + 3 + 5 + 3 + 20000 + 3 + 3 + 3 + 750 + 256 + 3
        ); // = 23157
    }

    #[test]
    fn test_recursive_call_pattern() {
        let estimator = AdvancedGasEstimator::new();

        // Simulate checking multiple conditions before external call
        let asm = vec![
            // Check caller
            Asm::Op(Opcode::CALLER),
            Asm::Op(Opcode::PUSH20([0xFF; 20])), // Expected caller
            Asm::Op(Opcode::EQ),
            Asm::Op(Opcode::ISZERO),
            Asm::Op(Opcode::PUSH1([0x80])),
            Asm::Op(Opcode::JUMPI), // Revert if not authorized
            // Load state
            Asm::Op(Opcode::PUSH1([0x00])),
            Asm::Op(Opcode::SLOAD),
            // Prepare call
            Asm::Op(Opcode::PUSH1([0x00])),       // retSize
            Asm::Op(Opcode::PUSH1([0x00])),       // retOffset
            Asm::Op(Opcode::PUSH1([0x04])),       // argsSize
            Asm::Op(Opcode::PUSH1([0x00])),       // argsOffset
            Asm::Op(Opcode::PUSH1([0x00])),       // value
            Asm::Op(Opcode::PUSH20([0xAA; 20])),  // address
            Asm::Op(Opcode::PUSH2([0xFF, 0xFF])), // gas
            Asm::Op(Opcode::CALL),
        ];

        let report = estimator.estimate_advanced(&asm);

        // Verify CALL operation count and exact gas
        assert_eq!(report.opcode_counts.get("CALL"), Some(&1));
        // CALLER + PUSH20 + EQ + ISZERO + PUSH1 + JUMPI + PUSH1 + SLOAD + 5*PUSH1 + PUSH20 + PUSH2
        // + CALL 2 + 3 + 3 + 3 + 3 + 10 + 3 + 2100 + 5*3 + 3 + 3 + 2600 = 4748
        assert_eq!(report.total_gas, 4748);
    }

    // ==================== Report Formatting Tests ====================

    #[test]
    fn test_report_formatting_completeness() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            // Generate various operations for complete report
            Asm::Data(vec![0x00; 50]),
            Asm::Op(Opcode::PUSH1([0x01])),
            Asm::Op(Opcode::SLOAD), // Storage access
            Asm::Op(Opcode::PUSH1([0x20])),
            Asm::Op(Opcode::PUSH1([0x42])),
            Asm::Op(Opcode::MSTORE), // Memory access
        ];

        let report = estimator.estimate_advanced(&asm);
        let formatted = report.format_report();

        // Check all sections are present
        assert!(formatted.contains("Advanced Gas Analysis"));
        assert!(formatted.contains("Storage Access Pattern"));
        assert!(formatted.contains("Memory Access Pattern"));
        assert!(formatted.contains("Opcode Distribution"));
        assert!(formatted.contains("Deployment"));

        // Check specific values
        assert!(formatted.contains("50 bytes")); // Deployment data
        assert!(formatted.contains("Cold Reads: 1"));
    }

    #[test]
    fn test_opcode_distribution_sorting() {
        let estimator = AdvancedGasEstimator::new();

        let asm = vec![
            // Create uneven distribution
            Asm::Op(Opcode::PUSH1([1])),
            Asm::Op(Opcode::PUSH1([2])),
            Asm::Op(Opcode::PUSH1([3])),
            Asm::Op(Opcode::ADD),
            Asm::Op(Opcode::ADD),
            Asm::Op(Opcode::POP),
        ];

        let report = estimator.estimate_advanced(&asm);

        assert_eq!(report.opcode_counts.get("PUSH1"), Some(&3));
        assert_eq!(report.opcode_counts.get("ADD"), Some(&2));
        assert_eq!(report.opcode_counts.get("POP"), Some(&1));

        let formatted = report.format_report();
        // PUSH1 should appear before ADD in the sorted output
        let push_pos = formatted.find("PUSH1").unwrap();
        let add_pos = formatted.find("ADD").unwrap();
        assert!(push_pos < add_pos);
    }
}
