use crate::parser::lexer::{Token, lex};
use alloy_primitives::U256;
use chumsky::extra;
use chumsky::prelude::*;
use eth_ir_data::operation::InternalCall;
use eth_ir_data::operation::OneInOneOut;
use eth_ir_data::operation::SetDataOffset;
use eth_ir_data::operation::SetLargeConst;
use eth_ir_data::operation::SetSmallConst;
use eth_ir_data::operation::TwoInOneOut;
use eth_ir_data::{self as ir, *};
use std::borrow::Cow;
use std::ops::Range;

// AST types for the IR
#[derive(Debug, Clone, PartialEq)]
pub struct Program<'src> {
    pub definitions: Vec<Definition<'src>>,
}

impl TryInto<ir::EthIRProgram> for &Program<'_> {
    type Error = Cow<'static, str>;

    fn try_into(self) -> Result<ir::EthIRProgram, Self::Error> {
        let mut data_defs = vec![];
        let mut data_bytes = index_vec![];
        let mut top_level_func_defs = index_vec![];

        for def in &self.definitions {
            match def {
                Definition::Data(data) => {
                    let start = data_bytes.len_idx();
                    data_bytes.extend_from_slice(IndexSlice::new(&data.value));
                    let end = data_bytes.len_idx();
                    if let Some((dup_name, _)) =
                        data_defs.iter().find(|&&(other_name, _)| other_name == data.name)
                    {
                        return Err(Cow::Owned(format!("Duplicate data name {:?}", dup_name)));
                    }

                    data_defs.push((data.name, start..end));
                }
                Definition::Function(func) => {
                    if top_level_func_defs
                        .position(|&(other_name, _)| other_name == func.name)
                        .is_some()
                    {
                        return Err(Cow::Owned(format!("Duplicate data name {:?}", func.name)));
                    }
                    top_level_func_defs.push((func.name, func.output_count));
                }
            }
        }

        let mut functions = index_vec![];
        let mut basic_blocks = index_vec![];
        let mut locals_arena = index_vec![];
        let mut operations = index_vec![];
        let mut large_consts = index_vec![];

        for def in &self.definitions {
            let Definition::Function(func) = def else {
                continue;
            };

            let mut basic_block_names = IndexLinearSet::new();

            for bb in &func.blocks {
                basic_block_names.add(bb.name).map_err(|_| {
                    format!("Duplicate basic block name {:?} in function {:?}", bb.name, func.name)
                })?;
            }

            for bb in &func.blocks {
                let mut locals = IndexLinearSet::with_capacity(10);
                for input in bb.inputs {
                    locals.add(input).map_err(|_| {
                        format!("Duplicate local def {:?} in {}/{}", input, func.name, bb.name,)
                    })?;
                }

                let input_count = locals.len_idx();
                let ops_start = operations.len_idx();
                for stmt in &bb.body {
                    operations.push(
                        convert_statement(
                            stmt,
                            &top_level_func_defs,
                            &data_defs,
                            &mut locals,
                            &mut locals_arena,
                            &mut large_consts,
                        )
                        .map_err(|msg| format!("Error in {}/{}: {msg}", func.name, bb.name))?,
                    );
                }
            }
        }

        // TODO: Check function 0

        Ok(EthIRProgram {
            entry: FunctionId::new(0),
            functions,
            basic_blocks,
            operations,

            locals: locals_arena,
            data_bytes,
            large_consts,

            cases: index_vec![],
        })
    }
}

fn convert_statement<'src>(
    stmt: &Statement<'src>,
    top_level_func_defs: &IndexVec<FunctionId, (&'src str, u32)>,
    data_defs: &[(&'src str, Range<DataOffset>)],
    locals: &mut IndexLinearSet<LocalId, &'src str>,
    locals_arena: &mut IndexVec<LocalIndex, LocalId>,
    large_consts: &mut IndexVec<LargeConstId, U256>,
) -> Result<Operation, String> {
    let op = match stmt {
        Statement::InternalCall { outputs, function: fn_name, args } => {
            let function = top_level_func_defs
                .position(|(fn_name, _)| fn_name == fn_name)
                .ok_or_else(|| format!("icall to undefined function {}", fn_name))?;
            let output_count = top_level_func_defs[function].1;
            if output_count as usize != outputs.len() {
                Err(format!(
                    "icall outputs {} but function {fn_name} has {output_count} outputs",
                    outputs.len(),
                ))?
            }

            let args_start = locals_arena.len_idx();
            for &arg in args {
                let local = locals
                    .position(|&name| name == arg)
                    .ok_or_else(|| format!("icall arg {arg} undefined"))?;
                locals_arena.push(local);
            }
            let outputs_start = locals_arena.len_idx();
            for &out in outputs {
                let Ok(local) = locals.add(out) else {
                    return Err(format!("Duplicate local def {out:?}"));
                };

                locals_arena.push(local);
            }

            Operation::InternalCall(InternalCall { function, args_start, outputs_start })
        }
        &Statement::SetLocal { to_local, from_local } => {
            let arg1 = locals
                .find(from_local)
                .ok_or_else(|| format!("icall arg {from_local} undefined"))?;

            let Ok(result) = locals.add(to_local) else {
                return Err(format!("Duplicate local def {to_local:?}"));
            };
            Operation::LocalSet(OneInOneOut { result, arg1 })
        }
        &Statement::SetConst { to_local, value } => {
            let Ok(local) = locals.add(to_local) else {
                return Err(format!("Duplicate local def {to_local:?}"));
            };
            match u64::try_from(value) {
                Ok(value) => Operation::LocalSetSmallConst(SetSmallConst { local, value }),
                Err(_) => {
                    let cid = large_consts.len_idx();
                    large_consts.push(value);
                    Operation::LocalSetLargeConst(SetLargeConst { local, cid })
                }
            }
        }
        &Statement::SetDataOffset { to_local, data_ref } => {
            let (_, value) = data_defs
                .iter()
                .find(|&(name, _)| name == &data_ref)
                .cloned()
                .ok_or_else(|| format!("No data {data_ref:?}"))?;
            let local = locals.len_idx();
            let Ok(local) = locals.add(to_local) else {
                return Err(format!("Duplicate local def {to_local:?}"));
            };

            Operation::LocalSetDataOffset(SetDataOffset { local, value })
        }
        Statement::OpInvoke { to_local, op_name, args } => {
            use Operation as Op;
            let mut two_in_one_out = || -> Result<_, String> {
                if args.len() != 2 {
                    return Err(format!("{op_name} has 2 inputs, found: {}", args.len()));
                }
                let out_name =
                    to_local.ok_or_else(|| format!("{op_name} must have an output, found none"))?;

                let arg1 = locals
                    .find(args[0])
                    .ok_or_else(|| format!("local {:?} not defined", args[0]))?;
                let arg2 = locals
                    .find(args[1])
                    .ok_or_else(|| format!("local {:?} not defined", args[1]))?;

                let Ok(result) = locals.add(out_name) else {
                    return Err(format!("duplicate local def {}", out_name));
                };

                Ok(TwoInOneOut { result, arg1, arg2 })
            };

            match *op_name {
                "add" => Op::Add(two_in_one_out()?),
                _ => todo!(),
            }
        }
    };
    Ok(op)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition<'src> {
    Function(FunctionDef<'src>),
    Data(DataDef<'src>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef<'src> {
    pub name: &'src str,
    pub output_count: u32,
    pub blocks: Vec<BasicBlock<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock<'src> {
    pub name: &'src str,
    pub inputs: Vec<&'src str>,
    pub outputs: Vec<&'src str>,
    pub body: Vec<Statement<'src>>,
    pub control: Option<ControlFlow<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'src> {
    OpInvoke { to_local: Option<&'src str>, op_name: &'src str, args: Vec<&'src str> },
    InternalCall { outputs: Vec<&'src str>, function: &'src str, args: Vec<&'src str> },
    SetLocal { to_local: &'src str, from_local: &'src str },
    SetConst { to_local: &'src str, value: U256 },
    SetDataOffset { to_local: &'src str, data_ref: &'src str },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlow<'src> {
    Branch { condition: &'src str, non_zero_target: &'src str, zero_target: &'src str },
    Continue { target: &'src str },
    InternalReturn { value: &'src str },
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataDef<'src> {
    pub name: &'src str,
    pub value: Box<[u8]>,
}

// Parser implementation
pub fn parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    &'tokens [(Token<'src>, Range<usize>)],
    Program<'src>,
    extra::Err<Rich<'tokens, (Token<'src>, Range<usize>)>>,
> + 'tokens {
    let ident = select! { (Token::Identifier(s), _) => s };
    let label = select! { (Token::Label(s), _) => s };
    let data_ref = select! { (Token::DataOffsetReference(s), _) => s };
    let dec_literal = select! { (Token::DecLiteral(x), _) => x };
    let hex_literal = select! { (Token::HexLiteral(bytes), _) => bytes };

    let newline = select! { (Token::Newline, _) => () }.ignored();
    let equals = select! { (Token::Equals, _) => () };
    let colon = select! { (Token::Colon, _) => () };
    let question = select! { (Token::Question, _) => () };
    let thick_arrow = select! { (Token::ThickArrow, _) => () };
    let thin_arrow = select! { (Token::ThinArrow, _) => () };
    let left_brace = select! { (Token::LeftBrace, _) => () };
    let right_brace = select! { (Token::RightBrace, _) => () };
    let fn_tok = select! { (Token::Fn, _) => () };
    let data_tok = select! { (Token::Data, _) => () };
    let icall_tok = select! { (Token::Identifier(s), _) if s == "icall" => () };
    let iret_tok = select! { (Token::Identifier(s), _) if s == "iret" => () };

    // Statement parsers
    let set_local = ident
        .then_ignore(equals.clone())
        .then(ident)
        .map(|(to, from)| Statement::SetLocal { to_local: to, from_local: from });

    let const_value = dec_literal.or(hex_literal.map(|b| U256::from_be_slice(&b)));
    let set_const = ident
        .then_ignore(equals.clone())
        .then(const_value)
        .map(|(to, value)| Statement::SetConst { to_local: to, value });

    let set_data_offset = ident
        .then_ignore(equals.clone())
        .then(data_ref)
        .map(|(to, data)| Statement::SetDataOffset { to_local: to, data_ref: data });

    let op_invoke_with_assign = ident
        .then_ignore(equals.clone())
        .then(ident)
        .then(ident.repeated().collect::<Vec<_>>())
        .map(|((to_local, op_name), args)| Statement::OpInvoke {
            to_local: Some(to_local),
            op_name,
            args,
        });

    let op_invoke_no_assign = ident
        .then(ident.repeated().collect::<Vec<_>>())
        .map(|(op_name, args)| Statement::OpInvoke { to_local: None, op_name, args });

    let icall = ident
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then_ignore(equals.clone())
        .or_not()
        .then_ignore(icall_tok)
        .then(label)
        .then(ident.repeated().collect::<Vec<_>>())
        .then_ignore(colon.clone().or_not())
        .map(|((outputs, function), args)| Statement::InternalCall {
            outputs: outputs.unwrap_or_default(),
            function,
            args,
        });

    let stmt = choice((
        icall,
        op_invoke_with_assign,
        set_const,
        set_data_offset,
        set_local,
        op_invoke_no_assign,
    ));

    // Control flow parsers
    let branch = thick_arrow
        .clone()
        .ignore_then(ident)
        .then_ignore(question)
        .then(label)
        .then_ignore(colon.clone())
        .then(label)
        .map(|((condition, non_zero), zero)| ControlFlow::Branch {
            condition,
            non_zero_target: non_zero,
            zero_target: zero,
        });

    let continue_to = thick_arrow.ignore_then(label).map(|target| ControlFlow::Continue { target });

    let iret = iret_tok.ignore_then(ident).map(|value| ControlFlow::InternalReturn { value });

    let control_flow = choice((iret, branch, continue_to));

    // Basic block parser
    let bb_head = ident
        .then(ident.repeated().collect::<Vec<_>>())
        .then(thin_arrow.ignore_then(ident.repeated().collect::<Vec<_>>()).or_not())
        .map(|((name, inputs), outputs)| (name, inputs, outputs.unwrap_or_default()));

    let bb_body = stmt
        .separated_by(newline.clone().repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .then(control_flow.then_ignore(newline.clone().or_not()).or_not());

    let basic_block = bb_head
        .then_ignore(left_brace)
        .then_ignore(newline.clone().repeated())
        .then(bb_body)
        .then_ignore(right_brace)
        .then_ignore(newline.clone().repeated())
        .map(|((name, inputs, outputs), (body, control))| BasicBlock {
            name,
            inputs,
            outputs,
            body,
            control,
        });

    // Function definition parser
    let fn_def = fn_tok
        .ignore_then(ident)
        .then(
            dec_literal
                .clone()
                .try_map(|x, _| Ok(x.try_into().expect("Output count doesn't fit into u32"))),
        )
        .then_ignore(colon.clone())
        .then_ignore(newline.clone().or_not())
        .then(basic_block.repeated().collect::<Vec<_>>())
        .map(|((name, output_count), blocks)| {
            Definition::Function(FunctionDef { name, output_count, blocks })
        });

    // Data definition parser
    let bytes_literal = dec_literal.map(|x| x.to_be_bytes_vec().into_boxed_slice()).or(hex_literal);
    let data_def = data_tok
        .ignore_then(ident)
        .then(bytes_literal)
        .then_ignore(newline.clone().or_not())
        .map(|(name, value)| Definition::Data(DataDef { name, value }));

    // Program parser
    let definition = fn_def.or(data_def);

    newline
        .repeated()
        .ignore_then(definition.repeated().collect::<Vec<_>>())
        .then_ignore(end())
        .map(|definitions| Program { definitions })
}

pub fn parse<'tokens, 'src: 'tokens>(
    tokens: &'tokens [(Token<'src>, Range<usize>)],
) -> Program<'src> {
    let (out, errors) = parser().parse(tokens).into_output_errors();
    if !errors.is_empty() {
        panic!("{errors:?}")
    }
    out.unwrap()
}

pub fn parse_e2e<'src>(source: &'src str) -> Program<'src> {
    let spanned_tokens = lex(source).expect("lexing failed");
    parse(&spanned_tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::lex;

    #[test]
    fn test_simple_function() {
        let input = r#"
            fn main 1:
                entry a b {
                    x = add a b
                    => @done
                }
                done x {
                    iret x
                }
        "#;
        let tokens = lex(input).expect("lexing failed");
        let program = parse(&tokens);

        assert_eq!(program.definitions.len(), 1);
        match &program.definitions[0] {
            Definition::Function(f) => {
                assert_eq!(f.name, "main");
                assert_eq!(f.output_count, 1);
                assert_eq!(f.blocks.len(), 2);
                assert_eq!(f.blocks[0].name, "entry");
                assert_eq!(f.blocks[0].inputs, vec!["a", "b"]);
                assert!(f.blocks[0].outputs.is_empty());
                assert_eq!(f.blocks[0].control, Some(ControlFlow::Continue { target: "done" }));
                assert_eq!(f.blocks[0].body.len(), 1);
                assert_eq!(
                    f.blocks[0].body,
                    vec![Statement::OpInvoke {
                        to_local: Some("x"),
                        op_name: "add",
                        args: vec!["a", "b"]
                    }]
                );
                assert_eq!(f.blocks[1].name, "done");
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_data_definition() {
        let input = "data mydata 0xabcd1234";
        let tokens = lex(input).expect("lexing failed");
        let program = parse(&tokens);

        assert_eq!(program.definitions.len(), 1);
        let Definition::Data(data_def) = &program.definitions[0] else {
            panic!("expected data definition")
        };
        assert_eq!(&data_def.value[..], &[0xab, 0xcd, 0x12, 0x34]);
    }

    #[test]
    fn test_minimal_function() {
        let input = "fn main 1:\nentry {\nstop\n}";
        let tokens = lex(input).expect("lexing failed");
        let program = parse(&tokens);

        assert_eq!(program.definitions.len(), 1);
    }

    #[test]
    fn test_internal_call() {
        let input = r#"
fn main 2:
    entry a b {
        x y = icall @other a b:
        iret x
    }
"#;
        let tokens = lex(input).expect("lexing failed");
        let program = parse(&tokens);

        let func = match &program.definitions[0] {
            Definition::Function(f) => f,
            _ => panic!("expected function"),
        };

        match &func.blocks[0].body[0] {
            Statement::InternalCall { outputs, function, args } => {
                assert_eq!(outputs, &vec!["x", "y"]);
                assert_eq!(*function, "other");
                assert_eq!(args, &vec!["a", "b"]);
            }
            _ => panic!("expected internal call"),
        }
    }

    #[test]
    fn test_full_example() {
        // Test with simpler input first
        let input = r#"
            fn main 1:
                entry a b c -> a y {
                    x = add a b
                    => @done
                }
                done a y {
                    stop
                }
            "#;

        let program = parse_e2e(input);
        assert_eq!(program.definitions.len(), 1);
    }
}
