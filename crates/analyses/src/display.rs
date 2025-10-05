use eth_ir_data::EthIRProgram;
use std::fmt;

use super::BasicBlockOwnershipAndReachability;

pub fn display_program(program: &EthIRProgram) -> String {
    DisplayHelper(program).to_string()
}

struct DisplayHelper<'a>(&'a EthIRProgram);

impl<'a> fmt::Display for DisplayHelper<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_program(f, self.0)
    }
}

pub fn fmt_program(f: &mut fmt::Formatter<'_>, program: &EthIRProgram) -> fmt::Result {
    // Analyze basic block ownership
    let ownership = BasicBlockOwnershipAndReachability::analyze(program);

    // Display functions with their owned basic blocks
    for (func_id, _func) in program.functions.iter_enumerated() {
        writeln!(f, "fn @{}:", func_id)?;

        // Display all basic blocks owned by this function
        for bb_id in ownership.blocks_owned_by(func_id) {
            let bb = &program.basic_blocks[bb_id];
            bb.fmt_display(f, bb_id, program)?;
            writeln!(f)?;
        }
    }

    // Display unreachable basic blocks
    let mut unreachable = ownership.unreachable_blocks().peekable();
    if unreachable.peek().is_some() {
        writeln!(f, "// Unreachable basic blocks")?;
        for bb_id in unreachable {
            let bb = &program.basic_blocks[bb_id];
            bb.fmt_display(f, bb_id, program)?;
            writeln!(f)?;
        }
    }

    // Display data segments
    if !program.data_segments_start.is_empty() {
        writeln!(f)?;

        for (segment_id, _) in program.data_segments_start.iter_enumerated() {
            write!(f, "data .{segment_id} ")?;

            // Display hex bytes for the segment
            let range = program.get_segment_range(segment_id);
            write!(f, "0x")?;
            for i in range.start.get()..range.end.get() {
                write!(f, "{:02x}", program.data_bytes[eth_ir_data::DataOffset::new(i)])?;
            }
            writeln!(f)?;
        }
    }

    Ok(())
}
