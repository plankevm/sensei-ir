use alloy_primitives::U256;
use chumsky::prelude::*;
use std::ops::Range;

pub fn parse_hex_allow_uneven(s: &str) -> Result<Vec<u8>, &str> {
    let mut parsed_bytes = Vec::with_capacity(s.len().div_ceil(2));
    if s.len() % 2 == 1 {
        parsed_bytes.push(u8::from_str_radix(&s[0..1], 16).map_err(|_| &s[0..1])?);
    }
    for i in 0..s.len() / 2 {
        let i = i * 2 + s.len() % 2;
        parsed_bytes.push(u8::from_str_radix(&s[i..i + 2], 16).map_err(|_| &s[i..i + 2])?);
    }
    Ok(parsed_bytes)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    // Punctuation
    Colon,
    Newline,
    ThinArrow,  // ->
    ThickArrow, // =>
    Question,
    Equals,
    LeftBrace,
    RightBrace,
    Comma,

    // Keywords
    Data,
    Fn,

    // Identifiers and references
    Identifier(&'src str),
    Label(&'src str),               // @label
    DataOffsetReference(&'src str), // .data_ref

    // Literals
    DecLiteral(U256),
    HexLiteral(Box<[u8]>),
}

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<(Token<'src>, Range<usize>)>> {
    let punctuation = choice((
        just("\n").to(Token::Newline),
        just("=>").to(Token::ThickArrow),
        just("->").to(Token::ThinArrow),
        just(":").to(Token::Colon),
        just("?").to(Token::Question),
        just("=").to(Token::Equals),
        just("{").to(Token::LeftBrace),
        just("}").to(Token::RightBrace),
        just(",").to(Token::Comma),
    ));

    let keyword =
        choice((text::keyword("data").to(Token::Data), text::keyword("fn").to(Token::Fn)));

    let ident = text::ident().map(Token::Identifier);
    let label = just("@").ignore_then(text::ident()).map(Token::Label);
    let data_offset_ref = just(".").ignore_then(text::ident()).map(Token::DataOffsetReference);

    let dec_literal =
        text::digits(10).repeated().at_least(1).to_slice().map(|s: &str| {
            Token::DecLiteral(s.parse().expect("expected valid digits in dec literal"))
        });
    let hex_literal = just("0x")
        .ignore_then(text::digits(16).repeated().at_least(1).to_slice())
        .map(|s: &str| {
            Token::HexLiteral(
                parse_hex_allow_uneven(s)
                    .expect("expected valid hex in hex literal")
                    .into_boxed_slice(),
            )
        });

    // Comments
    let single_line_comment =
        just("//").then(any().and_is(text::newline().not()).repeated()).ignored();

    let multi_line_comment =
        just("/*").then(any().and_is(just("*/").not()).repeated()).then(just("*/")).ignored();

    let comment = single_line_comment.or(multi_line_comment);

    // Main token parser - order matters!
    let token =
        choice((keyword, ident, punctuation, label, data_offset_ref, hex_literal, dec_literal));

    // Whitespace (excluding newlines which are tokens)
    let whitespace = any().filter(|&c: &char| c.is_whitespace() && c != '\n').ignored().repeated();

    token
        .map_with(|t, e| {
            let span: SimpleSpan = e.span();
            (t, span.into_range())
        })
        .padded_by(whitespace)
        .padded_by(comment.padded_by(whitespace).repeated())
        .repeated()
        .collect()
}

pub fn lex(input: &str) -> Result<Vec<(Token<'_>, Range<usize>)>, Vec<EmptyErr>> {
    lexer().parse(input).into_result()
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy_primitives::hex;

    use Token as Ty;

    #[test]
    fn test_basic_tokens() {
        let input = "fn data : -> => ? = _ { } , @label .dataref 123 0xFF";
        let result = lex(input).unwrap().into_iter().map(|(t, _)| t).collect::<Vec<_>>();

        assert_eq!(result[0], Ty::Fn);
        assert_eq!(result[1], Ty::Data);
        assert_eq!(result[2], Ty::Colon);
        assert_eq!(result[3], Ty::ThinArrow);
        assert_eq!(result[4], Ty::ThickArrow);
        assert_eq!(result[5], Ty::Question);
        assert_eq!(result[6], Ty::Equals);
        assert_eq!(result[7], Ty::Identifier("_"));
        assert_eq!(result[8], Ty::LeftBrace);
        assert_eq!(result[9], Ty::RightBrace);
        assert_eq!(result[10], Ty::Comma);
        assert_eq!(result[11], Ty::Label("label"));
        assert_eq!(result[12], Ty::DataOffsetReference("dataref"));
        assert_eq!(result[13], Ty::DecLiteral(U256::from(123)));
        assert_eq!(result[14], Ty::HexLiteral(vec![0xff].into()));
    }

    #[test]
    fn test_identifiers() {
        let input = "add sstore callvalue x y123 _test";
        let result = lex(input).unwrap().into_iter().map(|(t, _)| t).collect::<Vec<_>>();

        assert_eq!(result[0], Ty::Identifier("add"));
        assert_eq!(result[1], Ty::Identifier("sstore"));
        assert_eq!(result[2], Ty::Identifier("callvalue"));
        assert_eq!(result[3], Ty::Identifier("x"));
        assert_eq!(result[4], Ty::Identifier("y123"));
        assert_eq!(result[5], Ty::Identifier("_test"));
    }

    #[test]
    fn test_hex_literals() {
        let input = "0x00 0xdead 0xBEEF 0x123456789abcdef 0xcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc";
        let result = lex(input).unwrap().into_iter().map(|(t, _)| t).collect::<Vec<_>>();

        assert_eq!(result[0], Token::HexLiteral(vec![0x00].into()));
        assert_eq!(result[1], Token::HexLiteral(vec![0xde, 0xad].into()));
        assert_eq!(result[2], Token::HexLiteral(vec![0xbe, 0xef].into()));
        assert_eq!(result[3], Token::HexLiteral(hex::decode("0123456789abcdef").unwrap().into()));
        assert_eq!(
            result[4],
            Token::HexLiteral(
                hex::decode("cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")
                    .unwrap()
                    .into()
            )
        );
    }

    #[test]
    fn test_comments() {
        let input = r#"
            fn main // single line comment
            /* multi
               line
               comment */ data
        "#;
        let result = lex(input).unwrap().into_iter().map(|(t, _)| t).collect::<Vec<_>>();

        // Should have newline, fn, main, newline, data
        assert_eq!(result.len(), 6, "{:?}", result);
        assert_eq!(result[0], Token::Newline);
        assert_eq!(result[1], Token::Fn);
        assert_eq!(result[2], Token::Identifier("main"));
        assert_eq!(result[3], Token::Newline);
        assert_eq!(result[4], Token::Data);
        assert_eq!(result[5], Token::Newline);
    }
}
