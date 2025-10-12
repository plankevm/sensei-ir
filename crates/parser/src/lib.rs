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

    @1 $0 $1 {
        $2 = add $0 $1
        $3 = const 0x0
        $4 = const 0x20
        return $3 $4
    }
"#;

        assert_parse_format(input, expected, EmitConfig::default());
    }

    #[test]
    fn test_arithmetic_operations() {
        let input = r#"
fn main:
    entry x y {
        a = add x y
        b = sub x y
        c = mul x y
        d = div x y
        e = mod x y
        f = exp x y
        g = lt x y
        h = gt x y
        i = eq x y
        j = and x y
        k = or x y
        l = xor x y
        m = not x
        n = iszero x
        stop
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

    @1 $0 $1 {
        $2 = add $0 $1
        $3 = sub $0 $1
        $4 = mul $0 $1
        $5 = div $0 $1
        $6 = mod $0 $1
        $7 = exp $0 $1
        $8 = lt $0 $1
        $9 = gt $0 $1
        $10 = eq $0 $1
        $11 = and $0 $1
        $12 = or $0 $1
        $13 = xor $0 $1
        $14 = not $0
        $15 = iszero $0
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_control_flow_branch() {
        let input = r#"
fn main:
    entry x {
        => x ? @nonzero : @zero
    }
    nonzero {
        a = const 0x1
        stop
    }
    zero {
        b = const 0x0
        stop
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

    @1 $0 {
        => $0 ? @2 : @3
    }

    @2 {
        $1 = const 0x1
        stop
    }

    @3 {
        $2 = const 0x0
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_control_flow_continue() {
        let input = r#"
fn main:
    entry x y {
        a = add x y
        => @exit
    }
    exit {
        stop
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

    @1 $0 $1 {
        $2 = add $0 $1
        => @2
    }

    @2 {
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_data_segments() {
        let input = r#"
fn main:
    entry {
        x = data_offset .mydata
        y = data_offset .short
        z = data_offset .long
        stop
    }

data mydata 0x48656c6c6f20576f726c6421
data short 0xff
data long 0x0123456789abcdef0123456789abcdef
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
        $0 = data_offset .0
        $1 = data_offset .1
        $2 = data_offset .2
        stop
    }


data .0 0x48656c6c6f20576f726c6421
data .1 0xff
data .2 0x0123456789abcdef0123456789abcdef
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_internal_calls1() {
        let input = r#"
fn main:
    entry a b {
        x y = icall @add_fn a b
        stop
    }

fn add_fn:
    entry x y -> sum_out diff_out {
        sum_out = add x y
        diff_out = sub x y
        iret
    }
"#;

        let expected = r#"
Functions:
    fn @0 -> entry @0  (outputs: 0)
    fn @1 -> entry @1  (outputs: 2)
    fn @2 -> entry @2  (outputs: 0)

Basic Blocks:
    @0 {
        stop
    }

    @1 $0 $1 -> $2 $3 {
        $2 = add $0 $1
        $3 = sub $0 $1
        iret
    }

    @2 $4 $5 {
        $6 $7 = icall $4 $5
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_memory_operations() {
        let input = r#"
fn main:
    entry {
        size = const 32
        ptr = malloc size
        value = const 0xdeadbeef
        mstore ptr value 32
        loaded = mload ptr 32
        new_ptr = malloc size
        mcopy new_ptr ptr size
        stop
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
        $0 = const 0x20
        $1 = malloc $0
        $2 = const 0xdeadbeef
        mstore 32 $1 $2
        $3 = mload 32 $1
        $4 = malloc $0
        mcopy $4 $1 $0
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_storage_operations() {
        let input = r#"
fn main:
    entry {
        key = const 0x1
        value = const 0xabcd
        sstore key value
        loaded = sload key
        tstore key value
        tloaded = tload key
        stop
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
        $0 = const 0x1
        $1 = const 0xabcd
        sstore $0 $1
        $2 = sload $0
        tstore $0 $1
        $3 = tload $0
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_environment_operations() {
        let input = r#"
fn main:
    entry {
        addr = address
        bal = balance addr
        orig = origin
        call = caller
        val = callvalue
        coin = coinbase
        time = timestamp
        num = number
        diff = difficulty
        limit = gaslimit
        chain = chainid
        g = gas
        stop
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
        $0 = address
        $1 = balance $0
        $2 = origin
        $3 = caller
        $4 = callvalue
        $5 = coinbase
        $6 = timestamp
        $7 = number
        $8 = difficulty
        $9 = gaslimit
        $10 = chainid
        $11 = gas
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_log_operations() {
        let input = r#"
fn main:
    entry {
        offset = const 0x0
        size = const 0x20
        topic1 = const 0xaaaa
        topic2 = const 0xbbbb
        topic3 = const 0xcccc
        topic4 = const 0xdddd
        log0 offset size
        log1 offset size topic1
        log2 offset size topic1 topic2
        log3 offset size topic1 topic2 topic3
        log4 offset size topic1 topic2 topic3 topic4
        stop
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
        $2 = const 0xaaaa
        $3 = const 0xbbbb
        $4 = const 0xcccc
        $5 = const 0xdddd
        log0 $0 $1
        log1 $0 $1 $2
        log2 $0 $1 $2 $3
        log3 $0 $1 $2 $3 $4
        log4 $0 $1 $2 $3 $4 $5
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_call_operations() {
        let input = r#"
fn main:
    entry {
        gas = const 0x5000
        addr = large_const 0x1234567890123456789012345678901234567890
        value = const 0x100
        argsOffset = const 0x0
        argsSize = const 0x20
        retOffset = const 0x20
        retSize = const 0x20
        success = call gas addr value argsOffset argsSize retOffset retSize
        success2 = delegatecall gas addr argsOffset argsSize retOffset retSize
        success3 = staticcall gas addr argsOffset argsSize retOffset retSize
        stop
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
        $0 = const 0x5000
        $1 = large_const 0x1234567890123456789012345678901234567890
        $2 = const 0x100
        $3 = const 0x0
        $4 = const 0x20
        $5 = const 0x20
        $6 = const 0x20
        $7 = call $0 $1 $2 $3 $4 $5 $6
        $8 = delegatecall $0 $1 $3 $4 $5 $6
        $9 = staticcall $0 $1 $3 $4 $5 $6
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_create_operations() {
        let input = r#"
fn main:
    entry {
        value = const 0x0
        offset = const 0x0
        size = const 0x100
        salt = const 0xdeadbeef
        addr = create value offset size
        addr2 = create2 value offset size salt
        stop
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
        $1 = const 0x0
        $2 = const 0x100
        $3 = const 0xdeadbeef
        $4 = create $0 $1 $2
        $5 = create2 $0 $1 $2 $3
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_terminating_operations() {
        let input = r#"
fn main:
    entry cond {
        => cond ? @return_path : @revert_path
    }
    return_path {
        ret_offset = const 0x0
        ret_size = const 0x20
        return ret_offset ret_size
    }
    revert_path {
        rev_offset = const 0x0
        rev_size = const 0x40
        revert rev_offset rev_size
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

    @1 $0 {
        => $0 ? @2 : @3
    }

    @2 {
        $1 = const 0x0
        $2 = const 0x20
        return $1 $2
    }

    @3 {
        $3 = const 0x0
        $4 = const 0x40
        revert $3 $4
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_large_constants() {
        let input = r#"
fn main:
    entry {
        small = const 0xff
        medium = const 0xffff
        large = large_const 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
        max = large_const 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        stop
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
        $0 = const 0xff
        $1 = const 0xffff
        $2 = large_const 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
        $3 = large_const 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_complex_example1() {
        let input = r#"
fn main:
    entry a b {
        sum = add a b
        zero = const 0x0
        is_zero = eq sum zero
        => is_zero ? @error : @success
    }
    error {
        msg = data_offset .error_msg
        len = const 0x1b
        revert msg len
    }
    success {
        key = const 0x0
        value = const 0x1
        sstore key value
        stop
    }

data error_msg 0x4572726f723a20496e76616c696420696e707574
"#;

        let expected = r#"
Functions:
    fn @0 -> entry @0  (outputs: 0)
    fn @1 -> entry @1  (outputs: 0)

Basic Blocks:
    @0 {
        stop
    }

    @1 $0 $1 {
        $2 = add $0 $1
        $3 = const 0x0
        $4 = eq $2 $3
        => $4 ? @2 : @3
    }

    @2 {
        $5 = data_offset .0
        $6 = const 0x1b
        revert $5 $6
    }

    @3 {
        $7 = const 0x0
        $8 = const 0x1
        sstore $7 $8
        stop
    }


data .0 0x4572726f723a20496e76616c696420696e707574
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_simple_function2() {
        let input = r#"
fn main:
    entry a b {
        c = add a b
        two = const 0x2
        d = mul c two
        stop
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

    @1 $0 $1 {
        $2 = add $0 $1
        $3 = const 0x2
        $4 = mul $2 $3
        stop
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_control_flow() {
        let input = r#"
fn main:
    entry value threshold {
        cmp = lt value threshold
        => cmp ? @below : @above
    }
    below -> below_result {
        below_result = const 0
        => @done
    }
    above val -> above_result {
        above_result = copy val
        => @done
    }
    done -> final_result {
        final_result = or below_result above_result
        iret
    }
"#;

        let expected = r#"
Functions:
    fn @0 -> entry @0  (outputs: 0)
    fn @1 -> entry @1  (outputs: 1)

Basic Blocks:
    @0 {
        stop
    }

    @1 $0 $1 {
        $2 = lt $0 $1
        => $2 ? @2 : @3
    }

    @2 -> $3 {
        $3 = const 0x0
        => @4
    }

    @3 $4 -> $5 {
        $5 = copy $4
        => @4
    }

    @4 -> $6 {
        $6 = or $3 $5
        iret
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_data_and_memory() {
        let input = r#"
fn main:
    entry {
        hello_ptr = data_offset .hello
        world_ptr = data_offset .world
        size = const 0x40
        mem_ptr = malloc size
        zero = const 0x0
        len = const 0x20
        calldatacopy mem_ptr zero len
        value = const 0xdeadbeef
        mstore mem_ptr value 32
        loaded = mload mem_ptr 32
        stop
    }

data hello 0x48656c6c6f
data world 0x576f726c64
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
        $0 = data_offset .0
        $1 = data_offset .1
        $2 = const 0x40
        $3 = malloc $2
        $4 = const 0x0
        $5 = const 0x20
        calldatacopy $3 $4 $5
        $6 = const 0xdeadbeef
        mstore 32 $3 $6
        $7 = mload 32 $3
        stop
    }


data .0 0x48656c6c6f
data .1 0x576f726c64
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_internal_calls2() {
        let input = r#"
fn main:
    entry a b c -> final_result {
        sum_main diff_main = icall @arithmetic a b
        final_result = icall @select c sum_main diff_main
        => @ret
    }
    ret {
        iret
    }

fn arithmetic:
    entry x y -> sum_out diff_out {
        sum_out = add x y
        diff_out = sub x y
        iret
    }

fn select:
    entry flag option1 option2 {
        => flag ? @take_first : @take_second
    }
    take_first -> choose_first {
        choose_first = copy option1
        => @ret_first
    }
    take_second -> choose_second {
        choose_second = copy option2
        => @ret_second
    }
    ret_first -> select_result_first {
        select_result_first = copy choose_first
        iret
    }
    ret_second -> select_result_second {
        select_result_second = copy choose_second
        iret
    }
"#;

        let expected = r#"
Functions:
    fn @0 -> entry @0  (outputs: 0)
    fn @1 -> entry @1  (outputs: 2)
    fn @2 -> entry @2  (outputs: 1)
    fn @3 -> entry @7  (outputs: 0)

Basic Blocks:
    @0 {
        stop
    }

    @1 $0 $1 -> $2 $3 {
        $2 = add $0 $1
        $3 = sub $0 $1
        iret
    }

    @2 $4 $5 $6 {
        => $4 ? @3 : @4
    }

    @3 -> $7 {
        $7 = copy $5
        => @5
    }

    @4 -> $8 {
        $8 = copy $6
        => @6
    }

    @5 -> $9 {
        $9 = copy $7
        iret
    }

    @6 -> $10 {
        $10 = copy $8
        iret
    }

    @7 $11 $12 $13 -> $16 {
        $14 $15 = icall $11 $12
        $16 = icall $13 $14 $15
        => @8
    }

    @8 {
        iret
    }
"#;

        assert_parse_format(input, expected);
    }

    #[test]
    fn test_complex_example2() {
        let input = r#"
fn main:
    entry amount {
        caller_addr = caller
        bal = balance caller_addr
        sufficient = gt bal amount
        => sufficient ? @process : @insufficient_funds
    }
    insufficient_funds {
        err_ptr_insufficient = data_offset .error_insufficient_balance
        err_len_insufficient = const 0x15
        revert err_ptr_insufficient err_len_insufficient
    }
    process -> net_amount fee {
        hundred = const 0x64
        fee = div amount hundred
        net_amount = sub amount fee
        total = add net_amount fee
        is_overflow = lt total amount
        => is_overflow ? @overflow_error : @execute
    }
    overflow_error {
        err_ptr_overflow = data_offset .error_overflow
        err_len_overflow = const 0x8
        revert err_ptr_overflow err_len_overflow
    }
    execute -> success {
        net_key = const 0x0
        fee_key = const 0x1
        sstore net_key net_amount
        sstore fee_key fee
        zero = const 0x0
        log_len = const 0x40
        log2 zero log_len net_amount fee
        success = const 0x1
        => @done
    }
    done -> final_success {
        final_success = copy success
        iret
    }

data error_insufficient_balance 0x496e73756666696369656e742062616c616e6365
data error_overflow 0x4f766572666c6f77
"#;

        let expected = r#"
Functions:
    fn @0 -> entry @0  (outputs: 0)
    fn @1 -> entry @1  (outputs: 1)

Basic Blocks:
    @0 {
        stop
    }

    @1 $0 {
        $1 = caller
        $2 = balance $1
        $3 = gt $2 $0
        => $3 ? @3 : @2
    }

    @2 {
        $4 = data_offset .0
        $5 = const 0x15
        revert $4 $5
    }

    @3 -> $8 $7 {
        $6 = const 0x64
        $7 = div $0 $6
        $8 = sub $0 $7
        $9 = add $8 $7
        $10 = lt $9 $0
        => $10 ? @4 : @5
    }

    @4 {
        $11 = data_offset .1
        $12 = const 0x8
        revert $11 $12
    }

    @5 -> $17 {
        $13 = const 0x0
        $14 = const 0x1
        sstore $13 $8
        sstore $14 $7
        $15 = const 0x0
        $16 = const 0x40
        log2 $15 $16 $8 $7
        $17 = const 0x1
        => @6
    }

    @6 -> $18 {
        $18 = copy $17
        iret
    }


data .0 0x496e73756666696369656e742062616c616e6365
data .1 0x4f766572666c6f77
"#;

        assert_parse_format(input, expected);
    }
}
