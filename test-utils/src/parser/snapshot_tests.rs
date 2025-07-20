use super::parsing::parse_e2e;
use eth_ir_data::EthIRProgram;
use insta::assert_snapshot;
use std::borrow::Cow;

/// Helper function to parse IR and convert to EthIRProgram, then format using Display
fn parse_and_format(input: &str) -> Result<String, Cow<'static, str>> {
    let ast = parse_e2e(input);
    let ir: EthIRProgram = (&ast).try_into()?;
    Ok(format!("{}", ir))
}

#[test]
fn test_simple_function() {
    let input = r#"
fn main 0:
    entry a b {
        c = add a b
        zero = 0
        size = 32
        return zero size
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
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

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
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

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_control_flow_continue() {
    let input = r#"
fn main 0:
    entry a b {
        sum = add a b
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_data_segments() {
    let input = r#"
fn main 0:
    entry {
        a = .greeting
        b = .small
        c = .large
        stop
    }

data greeting 0x48656c6c6f20576f726c6421
data small 0xFF
data large 0x0123456789ABCDEF0123456789ABCDEF
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_internal_calls() {
    let input = r#"
fn main 0:
    entry a b {
        x y = icall @helper a b
        stop
    }

fn helper 2:
    entry p q {
        sum = add p q
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_memory_operations() {
    let input = r#"
fn main 0:
    entry {
        size = 32
        ptr = malloc size
        value = 0xDEADBEEF
        mstore ptr value
        loaded = mload ptr
        ptr2 = malloc size
        mcopy ptr2 ptr size
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_storage_operations() {
    let input = r#"
fn main 0:
    entry {
        key = 1
        value = 0xABCD
        sstore key value
        loaded = sload key
        tstore key value
        tloaded = tload key
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_environment_operations() {
    let input = r#"
fn main 0:
    entry {
        addr = address
        bal = balance addr
        orig = origin
        clr = caller
        val = callvalue
        cb = coinbase
        ts = timestamp
        num = number
        diff = difficulty
        gl = gaslimit
        chain = chainid
        g = gas
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_log_operations() {
    let input = r#"
fn main 0:
    entry {
        offset = 0
        length = 32
        topic1 = 0xAAAA
        topic2 = 0xBBBB
        topic3 = 0xCCCC
        topic4 = 0xDDDD
        log0 offset length
        log1 offset length topic1
        log2 offset length topic1 topic2
        log3 offset length topic1 topic2 topic3
        log4 offset length topic1 topic2 topic3 topic4
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_call_operations() {
    let input = r#"
fn main 0:
    entry {
        gas = 0x5000
        addr = 0x1234567890123456789012345678901234567890
        value = 0x100
        argsOffset = 0
        argsLength = 32
        retOffset = 32
        retLength = 32
        success1 = call gas addr value argsOffset argsLength retOffset retLength
        success2 = delegatecall gas addr argsOffset argsLength retOffset retLength
        success3 = staticcall gas addr argsOffset argsLength retOffset retLength
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_create_operations() {
    let input = r#"
fn main 0:
    entry {
        value = 0
        offset = 0
        length = 256
        salt = 0xDEADBEEF
        addr1 = create value offset length
        addr2 = create2 value offset length salt
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_terminating_operations() {
    let input = r#"
fn main 0:
    entry x {
        => x ? @do_return : @do_revert
    }
    do_return {
        offset1 = 0
        length1 = 32
        return offset1 length1
    }
    do_revert {
        offset2 = 0
        length2 = 64
        revert offset2 length2
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_large_constants() {
    let input = r#"
fn main 0:
    entry {
        small1 = 0xFF
        small2 = 0xFFFF
        large1 = 0x1234567890ABCDEF1234567890ABCDEF1234567890ABCDEF1234567890ABCDEF
        large2 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_complex_example() {
    let input = r#"
data error_msg 0x4572726f723a20496e76616c696420696e707574
fn main 0:
    entry a b {
        sum = add a b
        zero = 0
        is_zero = eq sum zero
        => is_zero ? @error : @success
    }
    error {
        msg_ptr = .error_msg
        msg_len = 25
        revert msg_ptr msg_len
    }
    success {
        key = 0
        value = 1
        sstore key value
        stop
    }
"#;

    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

// Tests for fixture files
#[test]
fn test_fixture_simple_function() {
    let input = include_str!("../../fixtures/simple_function.ir");
    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_fixture_control_flow() {
    let input = include_str!("../../fixtures/control_flow.ir");
    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_fixture_data_and_memory() {
    let input = include_str!("../../fixtures/data_and_memory.ir");
    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_fixture_internal_calls() {
    let input = include_str!("../../fixtures/internal_calls.ir");
    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}

#[test]
fn test_fixture_complex_example() {
    let input = include_str!("../../fixtures/complex_example.ir");
    let result = parse_and_format(input).expect("Failed to parse and format");
    assert_snapshot!(result);
}
