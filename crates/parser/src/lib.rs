pub mod emit;
mod lexer;
pub mod parser;
use smallvec::SmallVec;

use parser::Span;

pub fn highlight_span<'src>(
    out: &mut impl std::fmt::Write,
    source: &'src str,
    span: Span,
    line_range: usize,
) {
    let mut lines: SmallVec<[usize; 1024]> = SmallVec::new();
    lines.extend(source.char_indices().filter_map(|(i, c)| (c == '\n').then_some(i)));

    let start_line = lines.binary_search(&span.start).unwrap_or_else(|x| x);
    let end_line = lines.binary_search(&span.end).unwrap_or_else(|x| x);
    assert_eq!(start_line, end_line, "TODO: Multiline highlight");
    let line = start_line;

    let show_start = line.saturating_sub(line_range);
    let show_end = (line + line_range).min(lines.len());

    let dig_width = show_end.checked_ilog10().unwrap_or(0) + 1;

    for i in show_start..=show_end {
        let line_start = lines.get(i.wrapping_sub(1)).map_or(0, |&i| i + 1);
        let line_end = lines[i];

        writeln!(out, "{:>2$} | {}", i + 1, &source[line_start..line_end], dig_width as usize,)
            .expect("write failed");
        if i == line {
            for _ in line_start..span.start + dig_width as usize + 3 {
                write!(out, " ").unwrap();
            }
            for _ in span.start..span.end {
                write!(out, "^").unwrap();
            }
            writeln!(out).unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::emit::EmitConfig;

    use super::*;
    use bumpalo::{Bump, collections::String as BString};

    #[test]
    fn test_simple_function1() {
        let input = r#"
    fn init:
        init_entry -> c0 c32 {
            c0 = const 0
            c32 = const 32
            a = calldataload c0
            b = calldataload c32
            c = add a b
            mstore 32 c0 c
            => @do_return
        }
        do_return offset len {
            return offset len
        }


    "#;

        let expected = r#"
    fn @0:
        @0 $0 $1 {
            $2 = add $0 $1
            $3 = 0x0
            $4 = 0x20
            return $3 $4
        }
    "#;

        let arena = Bump::with_capacity(8000);
        let ast = parser::parse(input, &arena).unwrap_or_else(|err| {
            let err = &err[0];
            let mut out = BString::with_capacity_in(200, &arena);
            highlight_span(&mut out, input, err.span().clone(), 2);
            panic!("{}\n{:?}", out, err);
        });
        let ir = emit::emit_ir(&arena, &ast, EmitConfig::default_no_run()).unwrap_or_else(|err| {
            let mut out = BString::with_capacity_in(400, &arena);
            for span in err.spans.iter() {
                highlight_span(&mut out, input, span.clone(), 0);
            }
            panic!("{}{}", out, err.reason);
        });

        println!("{}", eth_ir_data::display_program(&ir));
    }
}
