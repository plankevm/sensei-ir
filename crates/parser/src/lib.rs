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
    lines.push(source.len());

    let line = lines.partition_point(|&idx| idx < span.start);
    let show_start = line.saturating_sub(line_range);
    let show_end = (line + line_range).min(lines.len().saturating_sub(1));

    let dig_width = show_end.checked_ilog10().unwrap_or(0) + 1;

    for i in show_start..=show_end {
        let line_start = lines.get(i.wrapping_sub(1)).map_or(0, |&idx| idx + 1);
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
    use bumpalo::{Bump, collections::String as BString};

    use super::*;
    use crate::emit::{self, EmitConfig};

    fn parse_and_display<'a>(source: &str, config: EmitConfig<'a>) -> String {
        let arena = Bump::with_capacity(8_192);
        let ast = parser::parse(source.as_ref(), &arena).unwrap_or_else(|err| {
            let err = &err[0];
            let mut out = BString::with_capacity_in(200, &arena);
            highlight_span(&mut out, source.as_ref(), err.span().clone(), 2);
            panic!("{}\n{:?}", out, err);
        });

        let ir = emit::emit_ir(&arena, &ast, config).unwrap_or_else(|err| {
            let mut out = BString::with_capacity_in(400, &arena);
            for span in err.spans.iter() {
                highlight_span(&mut out, source.as_ref(), span.clone(), 0);
            }
            panic!("{}{}", out, err.reason);
        });

        eth_ir_data::display_program(&ir)
    }

    fn assert_parse_format<'a>(input: &str, expected: &str, config: EmitConfig<'a>) {
        let actual = parse_and_display(input, config);
        assert_eq!(actual.trim(), expected.trim(), "unexpected IR\ninput:\n{input}");
    }

    #[test]
    fn test_simple_function1() {
        let input = r#"
            fn init:
                entry {
                    stop
                }
            fn main:
                entry {
                    c0 = const 0
                    c32 = const 32
                    a = calldataload c0
                    b = calldataload c32
                    c = add a b
                    return c0 c32
                }
        "#;

        let expected = r#"
Functions:
    fn @0 -> entry @0  (outputs: 0)
    fn @1 -> entry @1  (outputs: 0)

Basic Blocks:
    @0 {
        stop
    }

    @1 {
        $0 = const 0x0
        $1 = const 0x20
        $2 = calldataload $0
        $3 = calldataload $1
        $4 = add $2 $3
        return $0 $1
    }
"#;

        assert_parse_format(input, expected, EmitConfig::default());
    }
}
