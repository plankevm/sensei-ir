use super::parsing::parse_e2e;
use eth_ir_data::EthIRProgram;
use std::borrow::Cow;

/// Helper function to parse IR and convert to EthIRProgram, then format using Display
fn parse_and_format(input: &str) -> Result<String, Cow<'static, str>> {
    let ast = parse_e2e(input);
    let ir: EthIRProgram = (&ast).try_into()?;
    Ok(format!("{}", ir))
}

/// Helper function to assert that parsed and formatted input matches expected output
fn assert_parse_format(input: &str, expected: &str) {
    let result = parse_and_format(input).expect("Failed to parse and format");
    let actual = result.trim();
    let expected = expected.trim();

    if actual != expected {
        eprintln!("=== Input ===\n{}\n", input.trim());
        eprintln!("=== Expected ===\n{}\n", expected);
        eprintln!("=== Actual ===\n{}\n", actual);
        eprintln!("=== Diff ===");
        for (i, (expected_line, actual_line)) in expected.lines().zip(actual.lines()).enumerate() {
            if expected_line != actual_line {
                eprintln!("Line {}: - {}", i + 1, expected_line);
                eprintln!("Line {}: + {}", i + 1, actual_line);
            }
        }
        // Also show missing lines
        let expected_lines = expected.lines().count();
        let actual_lines = actual.lines().count();
        if expected_lines != actual_lines {
            eprintln!(
                "Line count mismatch: expected {} lines, got {} lines",
                expected_lines, actual_lines
            );
        }
        panic!("Parse format mismatch");
    }
}

#[test]
fn test_simple_function1() {
    let input = r#"
fn main 0:
    entry a b {
        c = add a b
        zero = 0
        size = 32
        return zero size
    }
"#;

    let expected = r#"
fn @0 0:
    @0 $0 $1 {
        $2 = add $0 $1
        $3 = 0x0
        $4 = 0x20
        return $3 $4
    }
"#;

    assert_parse_format(input, expected);
}

#[test]
fn test_arithmetic_operations() {
    let input = r#"
fn main 0:
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
fn @0 0:
    @0 $0 $1 {
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
fn main 0:
    entry x {
        => x ? @nonzero : @zero
    }
    nonzero {
        a = 0x1
        stop
    }
    zero {
        b = 0x0
        stop
    }
"#;

    let expected = r#"
fn @0 0:
    @0 $0 {
        => $0 ? @1 : @2
    }

    @1 {
        $0 = 0x1
        stop
    }

    @2 {
        $0 = 0x0
        stop
    }
"#;

    assert_parse_format(input, expected);
}

#[test]
fn test_control_flow_continue() {
    let input = r#"
fn main 0:
    entry x y {
        a = add x y
        => @exit
    }
    exit {
        stop
    }
"#;

    let expected = r#"
fn @0 0:
    @0 $0 $1 {
        $2 = add $0 $1
        => @1
    }

    @1 {
        stop
    }
"#;

    assert_parse_format(input, expected);
}

#[test]
fn test_data_segments() {
    let input = r#"
fn main 0:
    entry {
        x = .mydata
        y = .short
        z = .long
        stop
    }

data mydata 0x48656c6c6f20576f726c6421
data short 0xff
data long 0x0123456789abcdef0123456789abcdef
"#;

    let expected = r#"
fn @0 0:
    @0 {
        $0 = .0
        $1 = .1
        $2 = .2
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
fn main 0:
    entry a b {
        x y = icall @add_fn a b
        stop
    }

fn add_fn 2:
    entry x y {
        sum = add x y
        stop
    }
"#;

    let expected = r#"
fn @0 0:
    @0 $0 $1 {
        $2 $3 = icall @1 $0 $1
        stop
    }

fn @1 2:
    @1 $0 $1 {
        $2 = add $0 $1
        stop
    }
"#;

    assert_parse_format(input, expected);
}

#[test]
fn test_memory_operations() {
    let input = r#"
fn main 0:
    entry {
        size = 32
        ptr = malloc size
        value = 0xdeadbeef
        mstore32 ptr value
        loaded = mload32 ptr
        new_ptr = malloc size
        mcopy new_ptr ptr size
        stop
    }
"#;

    let expected = r#"
fn @0 0:
    @0 {
        $0 = 0x20
        $1 = malloc $0
        $2 = 0xdeadbeef
        mstore32 $1 $2
        $3 = mload32 $1
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
fn main 0:
    entry {
        key = 0x1
        value = 0xabcd
        sstore key value
        loaded = sload key
        tstore key value
        tloaded = tload key
        stop
    }
"#;

    let expected = r#"
fn @0 0:
    @0 {
        $0 = 0x1
        $1 = 0xabcd
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
fn main 0:
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
fn @0 0:
    @0 {
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
fn main 0:
    entry {
        offset = 0x0
        size = 0x20
        topic1 = 0xaaaa
        topic2 = 0xbbbb
        topic3 = 0xcccc
        topic4 = 0xdddd
        log0 offset size
        log1 offset size topic1
        log2 offset size topic1 topic2
        log3 offset size topic1 topic2 topic3
        log4 offset size topic1 topic2 topic3 topic4
        stop
    }
"#;

    let expected = r#"
fn @0 0:
    @0 {
        $0 = 0x0
        $1 = 0x20
        $2 = 0xaaaa
        $3 = 0xbbbb
        $4 = 0xcccc
        $5 = 0xdddd
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
fn main 0:
    entry {
        gas = 0x5000
        addr = 0x1234567890123456789012345678901234567890
        value = 0x100
        argsOffset = 0x0
        argsSize = 0x20
        retOffset = 0x20
        retSize = 0x20
        success = call gas addr value argsOffset argsSize retOffset retSize
        success2 = delegatecall gas addr argsOffset argsSize retOffset retSize
        success3 = staticcall gas addr argsOffset argsSize retOffset retSize
        stop
    }
"#;

    let expected = r#"
fn @0 0:
    @0 {
        $0 = 0x5000
        $1 = 0x1234567890123456789012345678901234567890
        $2 = 0x100
        $3 = 0x0
        $4 = 0x20
        $5 = 0x20
        $6 = 0x20
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
fn main 0:
    entry {
        value = 0x0
        offset = 0x0
        size = 0x100
        salt = 0xdeadbeef
        addr = create value offset size
        addr2 = create2 value offset size salt
        stop
    }
"#;

    let expected = r#"
fn @0 0:
    @0 {
        $0 = 0x0
        $1 = 0x0
        $2 = 0x100
        $3 = 0xdeadbeef
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
fn main 0:
    entry cond {
        => cond ? @return_path : @revert_path
    }
    return_path {
        offset = 0x0
        size = 0x20
        return offset size
    }
    revert_path {
        offset = 0x0
        size = 0x40
        revert offset size
    }
"#;

    let expected = r#"
fn @0 0:
    @0 $0 {
        => $0 ? @1 : @2
    }

    @1 {
        $0 = 0x0
        $1 = 0x20
        return $0 $1
    }

    @2 {
        $0 = 0x0
        $1 = 0x40
        revert $0 $1
    }
"#;

    assert_parse_format(input, expected);
}

#[test]
fn test_large_constants() {
    let input = r#"
fn main 0:
    entry {
        small = 0xff
        medium = 0xffff
        large = 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
        max = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        stop
    }
"#;

    let expected = r#"
fn @0 0:
    @0 {
        $0 = 0xff
        $1 = 0xffff
        $2 = 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
        $3 = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        stop
    }
"#;

    assert_parse_format(input, expected);
}

#[test]
fn test_complex_example1() {
    let input = r#"
fn main 0:
    entry a b {
        sum = add a b
        zero = 0x0
        is_zero = eq sum zero
        => is_zero ? @error : @success
    }
    error {
        msg = .error_msg
        len = 0x1b
        revert msg len
    }
    success {
        key = 0x0
        value = 0x1
        sstore key value
        stop
    }

data error_msg 0x4572726f723a20496e76616c696420696e707574
"#;

    let expected = r#"
fn @0 0:
    @0 $0 $1 {
        $2 = add $0 $1
        $3 = 0x0
        $4 = eq $2 $3
        => $4 ? @1 : @2
    }

    @1 {
        $0 = .0
        $1 = 0x1b
        revert $0 $1
    }

    @2 {
        $0 = 0x0
        $1 = 0x1
        sstore $0 $1
        stop
    }


data .0 0x4572726f723a20496e76616c696420696e707574
"#;

    assert_parse_format(input, expected);
}

#[test]
fn test_simple_function2() {
    let input = r#"
fn main 0:
    entry a b {
        c = add a b
        two = 0x2
        d = mul c two
        stop
    }
"#;
    let result = parse_and_format(input).expect("Failed to parse and format");

    let expected = r#"
fn @0 0:
    @0 $0 $1 {
        $2 = add $0 $1
        $3 = 0x2
        $4 = mul $2 $3
        stop
    }
"#;

    assert_eq!(result.trim(), expected.trim());
}

#[test]
fn test_control_flow() {
    let input = r#"
fn main 1:
    entry value threshold {
        cmp = lt value threshold
        => cmp ? @below : @above
    }
    below -> r1 {
        r1 = 0
        => @done
    }
    above val -> r2 {
        r2 = val
        => @done
    }
    done res1 res2 {
        final_result = or res1 res2
        iret final_result
    }
    "#;
    let result = parse_and_format(input).expect("Failed to parse and format");

    let expected = r#"
fn @0 1:
    @0 $0 $1 {
        $2 = lt $0 $1
        => $2 ? @1 : @2
    }

    @1 -> $0 {
        $0 = 0x0
        => @3
    }

    @2 $0 -> $1 {
        $1 = $0
        => @3
    }

    @3 $0 $1 {
        $2 = or $0 $1
        iret $2
    }
"#;

    assert_eq!(result.trim(), expected.trim());
}

#[test]
fn test_data_and_memory() {
    let input = r#"
data hello 0x48656c6c6f
data world 0x576f726c64
fn main 0:
    entry {
        hello_ptr = .hello
        world_ptr = .world
        size = 0x40
        mem_ptr = malloc size
        zero = 0x0
        len = 0x20
        calldatacopy mem_ptr zero len
        value = 0xDEADBEEF
        mstore mem_ptr value
        loaded = mload mem_ptr
        stop
    }
    "#;
    let result = parse_and_format(input).expect("Failed to parse and format");

    let expected = r#"
fn @0 0:
    @0 {
        $0 = .0
        $1 = .1
        $2 = 0x40
        $3 = malloc $2
        $4 = 0x0
        $5 = 0x20
        calldatacopy $3 $4 $5
        $6 = 0xdeadbeef
        mstore32 $3 $6
        $7 = mload32 $3
        stop
    }


data .0 0x48656c6c6f
data .1 0x576f726c64
"#;

    assert_eq!(result.trim(), expected.trim());
}

#[test]
fn test_internal_calls2() {
    let input = r#"
fn main 1:
    entry a b c -> result {
        sum diff = icall @arithmetic a b
        result = icall @select c sum diff
        => @ret
    }
    ret result {
        final = result
        iret final
    }

fn arithmetic 2:
    entry x y -> sum diff {
        sum = add x y
        diff = sub x y
        => @ret
    }
    ret sum diff {
        first = sum
        iret first
    }

fn select 1:
    entry flag option1 option2 {
        => flag ? @take_first : @take_second
    }
    take_first option1 -> result1 {
        result1 = option1
        => @ret
    }
    take_second option2 -> result2 {
        result2 = option2
        => @ret
    }
    ret result1 result2 {
        final_result = or result1 result2
        iret final_result
    }
    "#;
    let result = parse_and_format(input).expect("Failed to parse and format");

    let expected = r#"
fn @0 1:
    @0 $0 $1 $2 -> $5 {
        $3 $4 = icall @1 $0 $1
        $5 = icall @2 $2 $3 $4
        => @1
    }

    @1 $0 {
        $1 = $0
        iret $1
    }

fn @1 2:
    @2 $0 $1 -> $2 $3 {
        $2 = add $0 $1
        $3 = sub $0 $1
        => @3
    }

    @3 $0 $1 {
        $2 = $0
        iret $2
    }

fn @2 1:
    @4 $0 $1 $2 {
        => $0 ? @5 : @6
    }

    @5 $0 -> $1 {
        $1 = $0
        => @7
    }

    @6 $0 -> $1 {
        $1 = $0
        => @7
    }

    @7 $0 $1 {
        $2 = or $0 $1
        iret $2
    }
"#;

    assert_eq!(result.trim(), expected.trim());
}

#[test]
fn test_complex_example2() {
    let input = r#"
fn main 1:
    entry amount {
        caller_addr = caller
        bal = balance caller_addr
        sufficient = gt bal amount
        => sufficient ? @process : @insufficient_funds
    }
    insufficient_funds {
        err_ptr = .error_insufficient_balance
        err_len = 0x15
        revert err_ptr err_len
    }
    process amount -> net_amount fee {
        hundred = 0x64
        fee = div amount hundred
        net_amount = sub amount fee
        total = add net_amount fee
        is_overflow = lt total amount
        => is_overflow ? @overflow_error : @execute
    }
    overflow_error {
        err_ptr = .error_overflow
        err_len = 0x8
        revert err_ptr err_len
    }
    execute net_amount fee -> success {
        net_key = 0x0
        fee_key = 0x1
        sstore net_key net_amount
        sstore fee_key fee
        zero = 0x0
        log_len = 0x40
        log2 zero log_len net_amount fee
        success = 0x1
        => @done
    }
    done success {
        result = success
        iret result
    }

data error_insufficient_balance 0x496e73756666696369656e742062616c616e6365
data error_overflow 0x4f766572666c6f77
    "#;
    let result = parse_and_format(input).expect("Failed to parse and format");

    let expected = r#"
fn @0 1:
    @0 $0 {
        $1 = caller
        $2 = balance $1
        $3 = gt $2 $0
        => $3 ? @2 : @1
    }

    @1 {
        $0 = .0
        $1 = 0x15
        revert $0 $1
    }

    @2 $0 -> $3 $2 {
        $1 = 0x64
        $2 = div $0 $1
        $3 = sub $0 $2
        $4 = add $3 $2
        $5 = lt $4 $0
        => $5 ? @3 : @4
    }

    @3 {
        $0 = .1
        $1 = 0x8
        revert $0 $1
    }

    @4 $0 $1 -> $6 {
        $2 = 0x0
        $3 = 0x1
        sstore $2 $0
        sstore $3 $1
        $4 = 0x0
        $5 = 0x40
        log2 $4 $5 $0 $1
        $6 = 0x1
        => @5
    }

    @5 $0 {
        $1 = $0
        iret $1
    }


data .0 0x496e73756666696369656e742062616c616e6365
data .1 0x4f766572666c6f77
"#;

    assert_eq!(result.trim(), expected.trim());
}
