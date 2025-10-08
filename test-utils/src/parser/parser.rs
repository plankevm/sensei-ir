use super::lexer::{Lexer, Token};
use alloy_primitives::{
    U256, hex,
    ruint::{BaseConvertError, ParseError as UintParseError},
};
use bumpalo::{Bump, collections::Vec as BVec};
use chumsky::{extra, input::IterInput, prelude::*};
use std::ops::Range;

const START_STMT_OUTPUTS_CAPACITY: usize = 1;
const STMT_OUTPUTS_FIRST_RESIZE_CAPACITY: usize = 4;
const DEFAULT_STMT_PARAMS_CAPACITY: usize = 4;
const DEFAULT_BB_STMTS_CAPACITY: usize = 16;

type Span = Range<usize>;
type Box<'a, T> = &'a mut T;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self { inner, span }
    }
}

// AST types
pub struct Ast<'ast, 'src> {
    pub ast_arena: &'ast Bump,
    pub functions: BVec<'ast, Function<'ast, 'src>>,
    pub data_segments: BVec<'ast, DataSegment<'ast, 'src>>,
}

pub struct DataSegment<'ast, 'src> {
    pub name: Spanned<&'src str>,
    pub data: Spanned<Box<'ast, [u8]>>,
}

pub struct Function<'ast, 'src> {
    pub name: Spanned<&'src str>,
    pub basic_blocks: BVec<'ast, BasicBlock<'ast, 'src>>,
}

pub struct BasicBlock<'ast, 'src> {
    pub name: Spanned<&'src str>,
    pub inputs: BVec<'ast, Spanned<&'src str>>,
    pub outputs: BVec<'ast, Spanned<&'src str>>,
    pub stmts: BVec<'ast, Statement<'ast, 'src>>,
    pub control_flow: Option<ControlFlow<'ast, 'src>>,
}

pub struct Statement<'ast, 'src> {
    pub assigns: BVec<'ast, Spanned<&'src str>>,
    pub op: Spanned<&'src str>,
    pub params: BVec<'ast, ParamExpr<'ast, 'src>>,
}

pub enum ParamExpr<'ast, 'src> {
    NameRef(Spanned<&'src str>),
    FuncRef(Spanned<&'src str>),
    DataRef(Spanned<&'src str>),
    Num(Box<'ast, Spanned<U256>>),
}

pub enum ControlFlow<'ast, 'src> {
    UnconditionalTo(Spanned<&'src str>),
    Conditional {
        condition: Spanned<&'src str>,
        non_zero_to: Spanned<&'src str>,
        zero_to: Spanned<&'src str>,
    },
    Switch(Box<'ast, Switch<'ast, 'src>>),
    InternalReturn,
}

pub struct Switch<'ast, 'src> {
    pub value_ref: Spanned<&'src str>,
    pub default: Option<Spanned<&'src str>>,
    pub cases: BVec<'ast, Case<'src>>,
}

pub struct Case<'src> {
    pub match_value: U256,
    pub to: Spanned<&'src str>,
}

type ParserError<'src> = extra::Err<Rich<'src, Token, Span>>;

pub fn parse<'ast, 'src>(
    source: &'src str,
    arena: &'ast Bump,
) -> Result<Ast<'ast, 'src>, Vec<Rich<'src, Token, Span>>> {
    let token_iter = Lexer::new(source);
    let eoi = source.len()..source.len();
    let input = IterInput::new(token_iter, eoi);

    parser(source, arena).parse(input).into_result()
}

fn parser<'ast, 'src: 'ast>(
    source: &'src str,
    arena: &'ast Bump,
) -> impl Parser<'src, IterInput<Lexer<'src>, Span>, Ast<'ast, 'src>, ParserError<'src>> + Clone {
    // Token selectors with slice recovery - all produce Spanned values
    let ident = select! { Token::Identifier => () }
        .map_with(|_, e| Spanned::new(&source[e.span()], e.span()));

    let label = select! { Token::Label => () }.map_with(|_, e| {
        let s: &str = &source[e.span()];
        Spanned::new(s.strip_prefix('@').expect("invalid label"), e.span())
    });

    let data_ref = select! { Token::DataRef => () }.map_with(|_, e| {
        let s: &str = &source[e.span()];
        Spanned::new(s.strip_prefix('.').expect("invalid data ref"), e.span())
    });

    let dec_literal_as_u256 = select! { Token::DecLiteral => () }.map_with(|_, e| {
        let s: &str = &source[e.span()];
        s.parse::<U256>()
    });

    let hex_as_u256 = select! { Token::HexLiteral => () }.map_with(|_, e| {
        let s: &str = &source[e.span()];
        let hex_str = s.strip_prefix("0x").unwrap_or(s);
        U256::from_str_radix(hex_str, 16)
    });

    let u256_value = dec_literal_as_u256.or(hex_as_u256).try_map_with(|v, e| {
        let s = &source[e.span()];
        let value = v.map_err(|err| match err {
            UintParseError::BaseConvertError(BaseConvertError::Overflow) => {
                Rich::custom(e.span(), format!("Literal {:?} doesn't fit into 256-bits", s))
            }
            _ => unreachable!("U256 failed to parse {:?} (err: {})", s, err),
        })?;
        Ok(Spanned::new(value, e.span()))
    });

    // Statement parsers - all require explicit mnemonics now
    let statement = {
        let assigns = empty()
            .map(|_| BVec::with_capacity_in(START_STMT_OUTPUTS_CAPACITY, arena))
            .foldl(ident.clone().repeated(), |mut assigns, assign| {
                if assigns.len() == START_STMT_OUTPUTS_CAPACITY {
                    assigns.reserve(STMT_OUTPUTS_FIRST_RESIZE_CAPACITY);
                }
                assigns.push(assign);
                assigns
            });
        let mnemonic = ident.clone();
        let params =
            empty().map(|_| BVec::with_capacity_in(DEFAULT_STMT_PARAMS_CAPACITY, arena)).foldl(
                choice((
                    ident.clone().map(ParamExpr::NameRef),
                    label.clone().map(ParamExpr::FuncRef),
                    data_ref.clone().map(ParamExpr::DataRef),
                    u256_value.clone().map(|v| ParamExpr::Num(arena.alloc(v))),
                ))
                .repeated(),
                |mut exprs, expr| {
                    exprs.push(expr);
                    exprs
                },
            );

        assigns
            .then_ignore(just(Token::Equals))
            .then(mnemonic)
            .then(params)
            .map(|((assigns, op), params)| Statement { assigns, op, params })
    };

    // Control flow parsers
    let branch = just(Token::ThickArrow)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::Question))
        .then(label.clone())
        .then_ignore(just(Token::Colon))
        .then(label.clone())
        .map(|((condition, non_zero_to), zero_to)| ControlFlow::Conditional {
            condition,
            non_zero_to,
            zero_to,
        });

    let continue_to =
        just(Token::ThickArrow).ignore_then(label.clone()).map(ControlFlow::UnconditionalTo);

    let iret = just(Token::InternalReturn).map(|_| ControlFlow::InternalReturn);

    let switch = just(Token::Switch)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::LeftBrace))
        .then(
            choice((
                just(Token::Default)
                    .ignore_then(just(Token::ThickArrow))
                    .ignore_then(label.clone())
                    .map(|l| (None, l)),
                u256_value
                    .clone()
                    .then_ignore(just(Token::ThickArrow))
                    .then(label.clone())
                    .map(|(v, l)| (Some(v), l)),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .map(move |v| {
                let mut bv = BVec::with_capacity_in(v.len(), arena);
                bv.extend(v);
                bv
            }),
        )
        .then_ignore(just(Token::RightBrace))
        .map(|(value_ref, cases_data)| {
            let mut cases = BVec::new_in(arena);
            let mut default = None;

            for (match_val, to) in cases_data {
                match match_val {
                    Some(v) => cases.push(Case { match_value: v.inner, to }),
                    None => default = Some(to),
                }
            }

            ControlFlow::Switch(arena.alloc(Switch { value_ref, default, cases }))
        });

    let control_flow = choice((iret, switch, branch, continue_to));

    // Basic block parser
    let basic_block = ident
        .clone()
        .then(ident.clone().repeated().collect::<Vec<_>>().map(move |v| {
            let mut bv = BVec::with_capacity_in(v.len(), arena);
            bv.extend(v);
            bv
        }))
        .then(
            just(Token::ThinArrow)
                .ignore_then(ident.clone().repeated().collect::<Vec<_>>().map(move |v| {
                    let mut bv = BVec::with_capacity_in(v.len(), arena);
                    bv.extend(v);
                    bv
                }))
                .or_not()
                .map(|o| o.unwrap_or_else(|| BVec::new_in(arena))),
        )
        .then_ignore(just(Token::LeftBrace))
        .then_ignore(just(Token::Newline).ignored().repeated())
        .then(
            statement
                .clone()
                .separated_by(just(Token::Newline).ignored().repeated().at_least(1))
                .collect::<Vec<_>>()
                .then_ignore(just(Token::Newline).ignored().repeated())
                .then(control_flow.clone().or_not())
                .then_ignore(just(Token::Newline).ignored().repeated()),
        )
        .then_ignore(just(Token::RightBrace))
        .then_ignore(just(Token::Newline).ignored().repeated())
        .map(|(((name, inputs), outputs), (stmts, control_flow))| BasicBlock {
            name,
            inputs,
            outputs,
            stmts: BVec::from_iter_in(stmts.into_iter(), arena),
            control_flow,
        });

    // Function definition
    let function = just(Token::Fn)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::Colon))
        .then_ignore(just(Token::Newline).ignored().or_not())
        .then(basic_block.clone().repeated().collect::<Vec<_>>().map(move |v| {
            let mut bv = BVec::with_capacity_in(v.len(), arena);
            bv.extend(v);
            bv
        }))
        .map(|(name, basic_blocks)| Function { name, basic_blocks });

    // Data definition
    let data_def = just(Token::Data)
        .ignore_then(ident.clone())
        .then(select! { Token::HexLiteral => () }.try_map_with(|_, e| {
            let s: &str = &source[e.span()];
            let hex_str = s.strip_prefix("0x").expect("invalid hex literal");
            if hex_str.len() % 2 != 0 {
                return Err(Rich::custom(
                    e.span(),
                    "Data definition with odd hex nibbles is ambiguous",
                ));
            }
            let bytes = arena.alloc_slice_fill_default(hex_str.len() / 2);
            hex::decode_to_slice(hex_str, bytes).expect("hex not decoded despite validation");
            Ok(Spanned::new(bytes, e.span()))
        }))
        .then_ignore(just(Token::Newline).ignored().or_not())
        .map(|(name, data)| DataSegment { name, data });

    // Top-level program
    just(Token::Newline)
        .ignored()
        .repeated()
        .ignore_then(
            choice((function.map(|f| (Some(f), None)), data_def.map(|d| (None, Some(d)))))
                .repeated()
                .collect::<Vec<_>>()
                .map(|defs| {
                    let mut functions = BVec::new_in(arena);
                    let mut data_segments = BVec::new_in(arena);
                    for (func, data) in defs {
                        if let Some(f) = func {
                            functions.push(f);
                        }
                        if let Some(d) = data {
                            data_segments.push(d);
                        }
                    }
                    (functions, data_segments)
                }),
        )
        .then_ignore(end())
        .map(|(functions, data_segments)| Ast { ast_arena: arena, functions, data_segments })
}
