use crate::parser::lexer::LexerItem;

use super::lexer::{Lexer, Token};
use alloy_primitives::{
    U256, hex,
    ruint::{BaseConvertError, ParseError as UintParseError},
};
use bumpalo::{Bump, collections::Vec as BVec};
use logos::skip;
use std::ops::Range;

const DEFAULT_FUNCTIONS_CAPACITY: usize = 16;
const DEFAULT_DATA_SEGMENTS_CAPACITY: usize = 32;
const DEFAULT_BASIC_BLOCKS_CAPACITY: usize = 16;
const DEFAULT_BB_INPUTS_CAPACITY: usize = 4;
const DEFAULT_BB_OUTPUTS_CAPACITY: usize = 4;

type Box<'ast, T> = &'ast mut T;

struct Spanned<T> {
    span: Range<u32>,
    inner: T,
}

impl<T> Spanned<T> {
    fn new(inner: T, span: Range<u32>) -> Self {
        Self { inner, span }
    }
}

struct Ast<'ast, 'src> {
    ast_arena: &'ast Bump,
    functions: BVec<'ast, Function<'ast, 'src>>,
    data_segments: BVec<'ast, DataSegment<'ast, 'src>>,
}

struct DataSegment<'ast, 'src> {
    name: Spanned<&'src str>,
    data: Box<'ast, [u8]>,
}

struct Function<'ast, 'src> {
    name: Spanned<&'src str>,
    basic_blocks: BVec<'ast, BasicBlock<'ast, 'src>>,
}

struct BasicBlock<'ast, 'src> {
    name: Spanned<&'src str>,
    inputs: BVec<'ast, Spanned<&'src str>>,
    outputs: BVec<'ast, Spanned<&'src str>>,
    stmts: BVec<'ast, Statement<'ast, 'src>>,
    control_flow: Option<ControlFlow<'ast, 'src>>,
}

struct Statement<'ast, 'src> {
    assigns: BVec<'ast, Spanned<&'src str>>,
    op: Spanned<&'src str>,
    params: BVec<'ast, ParamExpr<'ast, 'src>>,
}

enum ParamExpr<'ast, 'src> {
    NameRef(Spanned<&'src str>),
    FuncRef(Spanned<&'src str>),
    DataRef(Spanned<&'src str>),
    Num(Box<'ast, Spanned<U256>>),
}

enum ControlFlow<'ast, 'src> {
    UnconditionalTo(Spanned<&'src str>),
    Conditional {
        condition: Spanned<&'src str>,
        non_zero_to: Spanned<&'src str>,
        zero_to: Spanned<&'src str>,
    },
    Switch(Box<'ast, Switch<'ast, 'src>>),
    InternalReturn,
}

struct Switch<'ast, 'src> {
    value_ref: Spanned<&'src str>,
    default: Option<Spanned<&'src str>>,
    cases: BVec<'ast, Case<'src>>,
}

struct Case<'src> {
    match_value: U256,
    to: Spanned<&'src str>,
}

struct ParseError {
    reason: String,
    span: Range<u32>,
}

type Result<T> = core::result::Result<T, ParseError>;

fn map_lexical_error<'src>(item: LexerItem<'src>) -> Result<(Token, &'src str, Range<u32>)> {
    let (tok, slice, span) = item;
    let tok = tok.map_err(|_| ParseError {
        reason: format!("Lexical error, unexpected string: {:?}", slice),
        span: span.clone(),
    })?;

    Ok((tok, slice, span))
}

fn parse_ast<'src, 'ast>(
    ast_arena: &'ast Bump,
    lexer: &mut Lexer<'src>,
) -> Result<Ast<'ast, 'src>> {
    let mut ast = Ast {
        ast_arena,
        functions: BVec::with_capacity_in(DEFAULT_FUNCTIONS_CAPACITY, ast_arena),
        data_segments: BVec::with_capacity_in(DEFAULT_DATA_SEGMENTS_CAPACITY, ast_arena),
    };

    while let Some(next_token) = lexer.next_skip_newline() {
        match map_lexical_error(next_token)? {
            (Token::Fn, _, _) => {
                ast.functions.push(parse_function(ast_arena, lexer)?);
            }
            (Token::Data, _, _) => {
                ast.data_segments.push(parse_data_segment(ast_arena, lexer)?);
            }
            (tok, _, span) => {
                return Err(ParseError {
                    reason: format!(
                        "Unexpected token {tok:?}, expected \"fn <fn_name>: ...basic_blocks\" or \"data <data_name> ...bytes\""
                    ),
                    span,
                });
            }
        }
    }

    Ok(ast)
}

fn match_token<'src>(
    next_item: Option<LexerItem<'src>>,
    expected: Token,
) -> Result<(&'src str, Range<u32>)> {
    let next_item = next_item.ok_or_else(|| ParseError {
        reason: format!("Unexpected EOF, expected <{:?}>", expected),
        span: 0..0,
    })?;
    let (token, slice, span) = map_lexical_error(next_item)?;
    if token != expected {
        return Err(ParseError {
            reason: format!("Expected <{:?}>, Got token {:?} {:?}", expected, token, slice),
            span,
        });
    }
    Ok((slice, span))
}

fn parse_function<'ast, 'src>(
    ast_arena: &'ast Bump,
    lexer: &mut Lexer<'src>,
) -> Result<Function<'ast, 'src>> {
    let (slice, span) = match_token(lexer.next_skip_newline(), Token::Identifier)?;
    let name = Spanned::new(slice, span);
    match_token(lexer.next_skip_newline(), Token::Colon)?;

    let mut basic_blocks = BVec::with_capacity_in(DEFAULT_BASIC_BLOCKS_CAPACITY, ast_arena);

    while let Some(item) = lexer.peek_skip_newline() {
        match item {
            (Ok(Token::Fn | Token::Data), _, _) => {
                break;
            }
            _ => basic_blocks.push(parse_basic_block(ast_arena, lexer)?),
        }
    }

    Ok(Function { name, basic_blocks })
}

fn parse_data_segment<'ast, 'src>(
    ast_arena: &'ast Bump,
    lexer: &mut Lexer<'src>,
) -> Result<DataSegment<'ast, 'src>> {
    let (slice, span) = match_token(lexer.next_skip_newline(), Token::Identifier)?;
    let name = Spanned::new(slice, span);

    let next_item = lexer.next_skip_newline().ok_or_else(|| ParseError {
        reason: "Expected hex or decimal literal for data segment".to_string(),
        span: 0..0,
    })?;
    let (token, slice, span) = map_lexical_error(next_item)?;

    let data = match token {
        Token::HexLiteral => {
            // Remove 0x prefix and parse hex string
            let hex_str = slice.strip_prefix("0x").expect("Hex literal without leading 0x");
            if hex_str.len() % 2 != 0 {
                return Err(ParseError {
                    reason: format!("Hex literal {:?} has uneven digits", slice),
                    span,
                });
            }
            let bytes = ast_arena.alloc_slice_fill_default(hex_str.len() / 2);
            hex::decode_to_slice(hex_str, bytes).expect("lexer gave invalid hex literal");
            bytes
        }
        Token::DecLiteral => {
            // Parse as U256 and convert to big-endian bytes
            let value = slice.parse::<U256>().map_err(|e| match e {
                UintParseError::BaseConvertError(BaseConvertError::Overflow) => ParseError {
                    reason: format!("Decimal value doesn't fit in 32-bytes, use hex."),
                    span,
                },
                _ => unreachable!("Lexer gave invalid decimal literal {e:?}"),
            })?;
            ast_arena.alloc_with(|| value.to_be_bytes::<32>())
        }
        _ => {
            return Err(ParseError {
                reason: format!("Expected hex or decimal literal, got {:?}", token),
                span,
            });
        }
    };

    Ok(DataSegment { name, data })
}

fn parse_literal<'src>(token: Token, slice: &'src str, span: &Range<u32>) -> Result<U256> {
    match token {
        Token::HexLiteral => {
            let hex_str = slice.strip_prefix("0x").unwrap_or(slice);
            U256::from_str_radix(hex_str, 16).map_err(|e| ParseError {
                reason: format!("Invalid hex literal: {}", e),
                span: span.clone(),
            })
        }
        Token::DecLiteral => slice.parse::<U256>().map_err(|e| ParseError {
            reason: format!("Invalid decimal literal: {}", e),
            span: span.clone(),
        }),
        _ => Err(ParseError {
            reason: format!("Expected literal, got {:?}", token),
            span: span.clone(),
        }),
    }
}

fn parse_statement<'ast, 'src>(
    ast_arena: &'ast Bump,
    lexer: &mut Lexer<'src>,
) -> Result<Statement<'ast, 'src>> {
    let mut assigns = BVec::new_in(ast_arena);

    // Peek to check if we have assignments (pattern: ident = ...)
    // or if it's just an operation with no outputs
    let first_item = lexer
        .next_skip_newline()
        .ok_or_else(|| ParseError { reason: "Expected statement".to_string(), span: 0..0 })?;

    let (first_token, first_slice, first_span) = map_lexical_error(first_item.clone())?;

    // Check if first token is an identifier
    let op_name_item = if first_token == Token::Identifier {
        // Peek next token to see if it's '='
        if let Some((Ok(Token::Equals), _, _)) = lexer.peek() {
            // This is an assignment: ident = ...
            lexer.next(); // consume the '='
            assigns.push(Spanned::new(first_slice, first_span));

            // Continue collecting more assigns if present
            loop {
                if let Some((Ok(Token::Identifier), _, _)) = lexer.peek() {
                    let peek_ahead = lexer.clone();
                    let mut peek_ahead = peek_ahead;
                    peek_ahead.next(); // skip identifier
                    if let Some((Ok(Token::Equals), _, _)) = peek_ahead.peek() {
                        // Another assignment
                        let (_, slice, span) = map_lexical_error(lexer.next().unwrap())?;
                        assigns.push(Spanned::new(slice, span));
                        lexer.next(); // consume '='
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Now get the operation name
            lexer.next_skip_newline().ok_or_else(|| ParseError {
                reason: "Expected operation name after '='".to_string(),
                span: 0..0,
            })?
        } else {
            // No '=', so this identifier is the operation name
            first_item
        }
    } else {
        return Err(ParseError {
            reason: format!("Expected identifier for statement, got {:?}", first_token),
            span: first_span,
        });
    };

    let (op_token, op_slice, op_span) = map_lexical_error(op_name_item)?;
    let op = match op_token {
        Token::Identifier => Spanned::new(op_slice, op_span),
        _ => {
            return Err(ParseError {
                reason: format!("Expected operation name, got {:?}", op_token),
                span: op_span,
            });
        }
    };

    // Parse parameters
    let mut params = BVec::new_in(ast_arena);
    loop {
        let peek = lexer.peek();
        match peek {
            Some((Ok(Token::Identifier), _, _)) => {
                let (_, slice, span) = map_lexical_error(lexer.next().unwrap())?;
                params.push(ParamExpr::NameRef(Spanned::new(slice, span)));
            }
            Some((Ok(Token::Label), _, _)) => {
                let (_, slice, span) = map_lexical_error(lexer.next().unwrap())?;
                // Strip @ prefix
                let label = slice.strip_prefix('@').unwrap_or(slice);
                params.push(ParamExpr::FuncRef(Spanned::new(label, span)));
            }
            Some((Ok(Token::DataRef), _, _)) => {
                let (_, slice, span) = map_lexical_error(lexer.next().unwrap())?;
                // Strip . prefix
                let data_ref = slice.strip_prefix('.').unwrap_or(slice);
                params.push(ParamExpr::DataRef(Spanned::new(data_ref, span)));
            }
            Some((Ok(Token::HexLiteral), _, _)) | Some((Ok(Token::DecLiteral), _, _)) => {
                let (token, slice, span) = map_lexical_error(lexer.next().unwrap())?;
                let value = parse_literal(token, slice, &span)?;
                params.push(ParamExpr::Num(ast_arena.alloc(Spanned::new(value, span))));
            }
            _ => break,
        }
    }

    Ok(Statement { assigns, op, params })
}

fn parse_control_flow<'ast, 'src>(
    ast_arena: &'ast Bump,
    lexer: &mut Lexer<'src>,
) -> Result<ControlFlow<'ast, 'src>> {
    let first_item = lexer
        .next_skip_newline()
        .ok_or_else(|| ParseError { reason: "Expected control flow".to_string(), span: 0..0 })?;

    let (first_token, _first_slice, first_span) = map_lexical_error(first_item)?;

    match first_token {
        Token::ThickArrow => {
            // Could be: => @label or => var ? @label1 : @label2
            let next_item = lexer.next_skip_newline().ok_or_else(|| ParseError {
                reason: "Expected label or identifier after '=>'".to_string(),
                span: 0..0,
            })?;
            let (next_token, next_slice, next_span) = map_lexical_error(next_item)?;

            match next_token {
                Token::Label => {
                    // Unconditional: => @label
                    let label = next_slice.strip_prefix('@').unwrap_or(next_slice);
                    Ok(ControlFlow::UnconditionalTo(Spanned::new(label, next_span)))
                }
                Token::Identifier => {
                    // Conditional: => var ? @label1 : @label2
                    let condition = Spanned::new(next_slice, next_span);

                    match_token(lexer.next_skip_newline(), Token::Question)?;

                    let (non_zero_slice, non_zero_span) =
                        match_token(lexer.next_skip_newline(), Token::Label)?;
                    let non_zero_to = Spanned::new(
                        non_zero_slice.strip_prefix('@').unwrap_or(non_zero_slice),
                        non_zero_span,
                    );

                    match_token(lexer.next_skip_newline(), Token::Colon)?;

                    let (zero_slice, zero_span) =
                        match_token(lexer.next_skip_newline(), Token::Label)?;
                    let zero_to =
                        Spanned::new(zero_slice.strip_prefix('@').unwrap_or(zero_slice), zero_span);

                    Ok(ControlFlow::Conditional { condition, non_zero_to, zero_to })
                }
                _ => Err(ParseError {
                    reason: format!(
                        "Expected label or identifier after '=>', got {:?}",
                        next_token
                    ),
                    span: next_span,
                }),
            }
        }
        Token::Switch => {
            // switch var { cases* default? }
            let (value_slice, value_span) =
                match_token(lexer.next_skip_newline(), Token::Identifier)?;
            let value_ref = Spanned::new(value_slice, value_span);

            match_token(lexer.next_skip_newline(), Token::LeftBrace)?;

            let mut cases = BVec::new_in(ast_arena);
            let mut default = None;

            loop {
                let peek = lexer.peek_skip_newline();
                match peek {
                    Some((Ok(Token::RightBrace), _, _)) => {
                        lexer.next_skip_newline(); // consume }
                        break;
                    }
                    Some((Ok(Token::Default), _, _)) => {
                        lexer.next_skip_newline(); // consume 'default'
                        match_token(lexer.next_skip_newline(), Token::ThickArrow)?;
                        let (label_slice, label_span) =
                            match_token(lexer.next_skip_newline(), Token::Label)?;
                        let label = label_slice.strip_prefix('@').unwrap_or(label_slice);
                        default = Some(Spanned::new(label, label_span));
                    }
                    Some((Ok(Token::HexLiteral), _, _)) | Some((Ok(Token::DecLiteral), _, _)) => {
                        let (token, slice, span) =
                            map_lexical_error(lexer.next_skip_newline().unwrap())?;
                        let match_value = parse_literal(token, slice, &span)?;

                        match_token(lexer.next_skip_newline(), Token::ThickArrow)?;

                        let (label_slice, label_span) =
                            match_token(lexer.next_skip_newline(), Token::Label)?;
                        let label = label_slice.strip_prefix('@').unwrap_or(label_slice);
                        let to = Spanned::new(label, label_span);

                        cases.push(Case { match_value, to });
                    }
                    _ => {
                        return Err(ParseError {
                            reason: "Expected case literal, 'default', or '}' in switch"
                                .to_string(),
                            span: 0..0,
                        });
                    }
                }
            }

            Ok(ControlFlow::Switch(ast_arena.alloc(Switch { value_ref, default, cases })))
        }
        Token::InternalReturn => {
            // iret (no arguments)
            Ok(ControlFlow::InternalReturn)
        }
        _ => Err(ParseError {
            reason: format!("Expected control flow (=>, switch, iret), got {:?}", first_token),
            span: first_span,
        }),
    }
}

fn parse_basic_block<'ast, 'src>(
    ast_arena: &'ast Bump,
    lexer: &mut Lexer<'src>,
) -> Result<BasicBlock<'ast, 'src>> {
    let (slice, span) = match_token(lexer.next_skip_newline(), Token::Identifier)?;
    let name = Spanned::new(slice, span);

    let mut inputs = BVec::with_capacity_in(DEFAULT_BB_INPUTS_CAPACITY, ast_arena);

    let bb_io_pattern = "(inputs...) (\"->\" outputs...)? \"{\"";
    let skip_outputs = loop {
        let Some(item) = lexer.next() else {
            return Err(ParseError {
                reason: format!("Unexpected EOF, expected {}", bb_io_pattern),
                span: 0..0,
            });
        };
        let (token, slice, span) = map_lexical_error(item)?;
        match token {
            Token::Identifier => {
                inputs.push(Spanned::new(slice, span));
            }
            Token::ThinArrow => {
                break false;
            }
            Token::LeftBrace => {
                break true;
            }
            _ => {
                return Err(ParseError {
                    reason: format!("Unexpected token {:?}, expected {}", token, bb_io_pattern),
                    span,
                });
            }
        }
    };

    // Check for outputs (-> ident*)
    let mut outputs = BVec::with_capacity_in(DEFAULT_BB_OUTPUTS_CAPACITY, ast_arena);
    if !skip_outputs {}
    if let Some((Ok(Token::ThinArrow), _, _)) = lexer.peek_skip_newline() {
        lexer.next_skip_newline(); // consume ->

        loop {
            let peek = lexer.peek_skip_newline();
            match peek {
                Some((Ok(Token::Identifier), _, _)) => {
                    let (_, slice, span) = map_lexical_error(lexer.next_skip_newline().unwrap())?;
                    outputs.push(Spanned::new(slice, span));
                }
                _ => break,
            }
        }
    }

    match_token(lexer.next_skip_newline(), Token::LeftBrace)?;

    // Parse statements
    let mut stmts = BVec::new_in(ast_arena);
    let mut control_flow = None;

    loop {
        let peek = lexer.peek_skip_newline();
        match peek {
            Some((Ok(Token::RightBrace), _, _)) => {
                lexer.next_skip_newline(); // consume }
                break;
            }
            Some((Ok(Token::ThickArrow), _, _))
            | Some((Ok(Token::Switch), _, _))
            | Some((Ok(Token::InternalReturn), _, _)) => {
                // This is control flow
                control_flow = Some(parse_control_flow(ast_arena, lexer)?);
                // After control flow, expect }
                match_token(lexer.next_skip_newline(), Token::RightBrace)?;
                break;
            }
            Some(_) => {
                // Parse a statement
                stmts.push(parse_statement(ast_arena, lexer)?);
            }
            None => {
                return Err(ParseError {
                    reason: "Unexpected EOF while parsing basic block".to_string(),
                    span: 0..0,
                });
            }
        }
    }

    Ok(BasicBlock { name, inputs, outputs, stmts, control_flow })
}
