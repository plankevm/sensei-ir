use super::op_data::*;
use crate::EthIRProgram;
use crate::index::*;
use std::fmt;

fn fmt_locals(f: &mut impl fmt::Write, mut locals: impl Iterator<Item = LocalId>) -> fmt::Result {
    let Some(first) = locals.next() else {
        return Ok(());
    };
    write!(f, "${}", first)?;
    for local in locals {
        write!(f, " ${}", local)?;
    }
    Ok(())
}

pub trait OpFmt {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, ir: &EthIRProgram) -> fmt::Result;
}

impl<const INS: usize, const OUTS: usize> OpFmt for InlineOperands<INS, OUTS> {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, _ir: &EthIRProgram) -> fmt::Result {
        fmt_locals(f, self.outs.iter().copied())?;
        if self.outs.is_empty() {
            write!(f, "{}", mnemonic)?;
        } else {
            write!(f, " = {}", mnemonic)?;
        }
        if !self.ins.is_empty() {
            write!(f, " ")?;
        }
        fmt_locals(f, self.ins.iter().copied())
    }
}

impl<const INS: usize, const OUTS: usize> OpFmt for AllocatedIns<INS, OUTS> {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, ir: &EthIRProgram) -> fmt::Result {
        fmt_locals(f, self.outs.iter().copied())?;
        if self.outs.is_empty() {
            write!(f, "{}", mnemonic)?;
        } else {
            write!(f, " = {}", mnemonic)?;
        }
        let ins = &ir.locals[self.ins_start..self.ins_start + INS as u32];
        if INS > 0 {
            write!(f, " ")?;
        }
        fmt_locals(f, ins.iter().copied())
    }
}

impl OpFmt for StaticAllocData {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, _ir: &EthIRProgram) -> fmt::Result {
        write!(f, "${} = {} {} #{}", self.ptr_out, mnemonic, self.size, self.alloc_id)
    }
}

impl OpFmt for SetSmallConstData {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, _ir: &EthIRProgram) -> fmt::Result {
        write!(f, "${} = {} {:#x}", self.sets, mnemonic, self.value)
    }
}

impl OpFmt for SetLargeConstData {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, ir: &EthIRProgram) -> fmt::Result {
        write!(f, "${} = {} {:#x}", self.sets, mnemonic, ir.large_consts[self.value])
    }
}

impl OpFmt for InternalCallData {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, ir: &EthIRProgram) -> fmt::Result {
        let ins = &ir.locals[self.ins_start..self.outs_start];
        let outs = &ir.locals
            [self.outs_start..self.outs_start + ir.functions[self.function].get_outputs()];
        fmt_locals(f, outs.iter().copied())?;
        if outs.len() > 0 {
            write!(f, " = {}", mnemonic)?;
        } else {
            write!(f, "{}", mnemonic)?;
        }
        if ins.len() > 0 {
            write!(f, " ")?;
        }
        fmt_locals(f, ins.iter().copied())
    }
}

impl OpFmt for MemoryLoadData {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, _ir: &EthIRProgram) -> fmt::Result {
        write!(f, "${} = {} {}, {}", self.out, mnemonic, self.io_size as u8, self.ptr)
    }
}

impl OpFmt for MemoryStoreData {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, _ir: &EthIRProgram) -> fmt::Result {
        write!(f, "{} {}, {}, {}", mnemonic, self.io_size as u8, self.ptr, self.value)
    }
}

impl OpFmt for SetDataOffsetData {
    fn op_fmt(&self, mnemonic: &str, f: &mut impl fmt::Write, _ir: &EthIRProgram) -> fmt::Result {
        write!(f, "${} = {} .{}", self.sets, mnemonic, self.segment_id)
    }
}
