use crate::{EthIRProgram, Operation};
use std::{collections::BTreeMap, ops::Range};

#[derive(Debug)]
pub struct DataSegmentAnalysis {
    /// Maps segment start positions to (end, id)
    segments: BTreeMap<u32, (u32, u32)>,
    /// Set of segments that are actually referenced in the program
    referenced_segments: std::collections::HashSet<u32>,
}

impl DataSegmentAnalysis {
    pub fn analyze(program: &EthIRProgram) -> Self {
        let mut segments = BTreeMap::new();
        let mut referenced_segments = std::collections::HashSet::new();
        let mut next_id = 0;

        // Collect all data offset references
        let mut referenced_ranges = Vec::new();
        for op in program.operations.iter() {
            if let Operation::LocalSetDataOffset(set) = op {
                let range = set.value.start.get()..set.value.end.get();
                referenced_ranges.push(range);
            }
        }

        // Sort ranges for processing
        referenced_ranges.sort_by_key(|r| r.start);

        // Assign IDs to all segments (including gaps)
        let mut current_pos = 0u32;

        for range in referenced_ranges {
            // Add gap segment if there's a gap
            if current_pos < range.start {
                segments.insert(current_pos, (range.start, next_id));
                next_id += 1;
            }

            // Add the referenced segment
            let segment_id = next_id;
            segments.insert(range.start, (range.end, segment_id));
            referenced_segments.insert(segment_id);
            next_id += 1;

            current_pos = range.end;
        }

        // Add final segment if there's remaining data
        let data_len = program.data_bytes.len() as u32;
        if current_pos < data_len {
            segments.insert(current_pos, (data_len, next_id));
        }

        Self { segments, referenced_segments }
    }

    pub fn get_segment_id(&self, range: &Range<u32>) -> Option<u32> {
        self.segments.get(&range.start).filter(|(end, _)| *end == range.end).map(|(_, id)| *id)
    }

    pub fn segments(&self) -> &BTreeMap<u32, (u32, u32)> {
        &self.segments
    }

    pub fn referenced_segments(&self) -> &std::collections::HashSet<u32> {
        &self.referenced_segments
    }
}