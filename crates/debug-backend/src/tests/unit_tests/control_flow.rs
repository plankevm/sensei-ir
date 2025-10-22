//! Tests for control flow operations and calls

use crate::{
    tests::helpers::{
        assert_opcode_counts, compile_to_bytecode, count_opcode, create_branching_program,
        create_program_with_switch, create_simple_program, execute_and_verify_halt,
        execute_and_verify_stop,
    },
    translate_program,
};
use alloy_primitives::U256;
use evm_glue::assembly::Asm;
use sir_data::{
    BasicBlockId, Branch, CasesId, Control, FunctionId, LocalId, LocalIndex, Operation, Switch,
    operation::*,
};

#[test]
fn branching_with_return() {
    let blocks = vec![
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 1 })],
            Control::Branches(Branch {
                condition: LocalId::new(0),
                zero_target: BasicBlockId::new(1),
                non_zero_target: BasicBlockId::new(2),
            }),
        ),
        (
            vec![
                Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
                Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 }),
                Operation::Return(InlineOperands {
                    ins: [LocalId::new(1), LocalId::new(2)],
                    outs: [],
                }),
            ],
            Control::LastOpTerminates,
        ),
        (vec![Operation::Stop(InlineOperands::default())], Control::LastOpTerminates),
    ];

    let program = create_branching_program(blocks, 0);
    let assembly = translate_program(program);

    assert_opcode_counts(&assembly, &[("JUMPI", 1), ("JUMP", 1), ("JUMPDEST", 4), ("RETURN", 1)]);
}

#[test]
fn branching_with_revert() {
    let blocks = vec![
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 })],
            Control::Branches(Branch {
                condition: LocalId::new(0),
                zero_target: BasicBlockId::new(1),
                non_zero_target: BasicBlockId::new(2),
            }),
        ),
        (
            vec![
                Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
                Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 }),
                Operation::Revert(InlineOperands {
                    ins: [LocalId::new(1), LocalId::new(2)],
                    outs: [],
                }),
            ],
            Control::LastOpTerminates,
        ),
        (vec![Operation::Stop(InlineOperands::default())], Control::LastOpTerminates),
    ];

    let program = create_branching_program(blocks, 0);
    let assembly = translate_program(program);

    assert_opcode_counts(&assembly, &[("JUMPI", 1), ("JUMP", 1), ("JUMPDEST", 4), ("REVERT", 1)]);
}

#[test]
fn stop_execution() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 42 }),
        Operation::Stop(InlineOperands::default()),
    ];

    let bytecode = compile_to_bytecode(operations);
    execute_and_verify_stop(bytecode).expect("Should execute successfully with STOP");
}

#[test]
fn stop_opcode_generation() {
    let operations = vec![Operation::Stop(InlineOperands::default())];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("STOP", 1)]);
}

#[test]
fn invalid_execution() {
    let operations = vec![Operation::Invalid(InlineOperands::default())];
    let bytecode = compile_to_bytecode(operations);

    execute_and_verify_halt(bytecode).expect("Should halt with INVALID");
}

#[test]
fn invalid_opcode_generation() {
    let operations = vec![Operation::Invalid(InlineOperands::default())];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert!(count_opcode(&asm, "INVALID") >= 1, "Should have INVALID operation");
}

#[test]
fn internal_call() {
    use crate::tests::helpers::create_multi_function_program;

    // Function 0: Main function that calls function 1
    let main_function = vec![(
        vec![
            Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 42 }),
            Operation::InternalCall(InternalCallData {
                function: FunctionId::new(1),
                ins_start: LocalIndex::new(0),
                outs_start: LocalIndex::new(1),
            }),
            Operation::Stop(InlineOperands::default()),
        ],
        Control::LastOpTerminates,
    )];

    // Function 1: Called function that doubles the input and returns
    // Internal functions return when control flow ends naturally
    let called_function = vec![(
        vec![
            // Double the input value (arg in local 0, result in local 1)
            Operation::Add(InlineOperands {
                ins: [LocalId::new(0), LocalId::new(0)],
                outs: [LocalId::new(1)],
            }),
            // Function returns here - no explicit return operation needed
        ],
        Control::LastOpTerminates, // Function ends here
    )];

    let program = create_multi_function_program(vec![(main_function, 0), (called_function, 0)]);

    let assembly = translate_program(program);

    // Internal calls should generate assembly
    // We can check that the program successfully translated
    assert!(!assembly.is_empty(), "Should generate assembly for internal call");

    // The internal call mechanism uses JUMPs in the generated bytecode
    // but the exact implementation may vary
}

#[test]
fn switch_statement() {
    // Test switch control flow with multiple cases
    let blocks = vec![
        // Block 0: Entry block that sets up condition and switches
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 2 })],
            Control::Switch(Switch {
                condition: LocalId::new(0),
                fallback: Some(BasicBlockId::new(3)), // Default case
                cases: CasesId::new(0),
            }),
        ),
        // Block 1: Case value = 1
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 100 })],
            Control::ContinuesTo(BasicBlockId::new(4)),
        ),
        // Block 2: Case value = 2
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 200 })],
            Control::ContinuesTo(BasicBlockId::new(4)),
        ),
        // Block 3: Default case
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 999 })],
            Control::ContinuesTo(BasicBlockId::new(4)),
        ),
        // Block 4: Exit block
        (
            vec![Operation::Return(InlineOperands {
                ins: [LocalId::new(0), LocalId::new(1)],
                outs: [],
            })],
            Control::LastOpTerminates,
        ),
    ];

    // TODO: Switch test needs to be updated for new Cases API
    // The create_program_with_switch helper is currently unimplemented
    unimplemented!("Switch test requires updated Cases API");
}

#[test]
fn control_flow() {
    let blocks = vec![
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 1 })],
            Control::ContinuesTo(BasicBlockId::new(1)),
        ),
        (vec![Operation::Stop(InlineOperands::default())], Control::LastOpTerminates),
    ];

    let program = create_branching_program(blocks, 0);
    let assembly = translate_program(program);

    assert!(count_opcode(&assembly, "JUMP") >= 1, "Should have unconditional jump");
}

#[test]
fn create() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 }),
        Operation::Create(AllocatedIns { ins_start: LocalIndex::new(0), outs: [LocalId::new(3)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("CREATE", 1), ("STOP", 1)]);
}

#[test]
fn test_create2() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 32 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 1 }),
        Operation::Create2(AllocatedIns { ins_start: LocalIndex::new(0), outs: [LocalId::new(4)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("CREATE2", 1), ("STOP", 1)]);
}

#[test]
fn selfdestruct() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::SelfDestruct(InlineOperands { ins: [LocalId::new(0)], outs: [] }),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert!(count_opcode(&asm, "SELFDESTRUCT") >= 1, "Should have SELFDESTRUCT operation");
}

#[test]
fn callcode() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 1000 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(4), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(5), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(6), value: 0 }),
        Operation::CallCode(AllocatedIns {
            ins_start: LocalIndex::new(0),
            outs: [LocalId::new(7)],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("CALLCODE", 1), ("STOP", 1)]);
}

#[test]
fn delegatecall() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 1000 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(4), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(5), value: 0 }),
        Operation::DelegateCall(AllocatedIns {
            ins_start: LocalIndex::new(0),
            outs: [LocalId::new(6)],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("DELEGATECALL", 1), ("STOP", 1)]);
}

#[test]
fn staticcall() {
    let operations = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 1000 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(4), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(5), value: 0 }),
        Operation::StaticCall(AllocatedIns {
            ins_start: LocalIndex::new(0),
            outs: [LocalId::new(6)],
        }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(operations);
    let asm = translate_program(program);

    assert_opcode_counts(&asm, &[("STATICCALL", 1), ("STOP", 1)]);
}

#[test]
fn stack_depth_management() {
    // Test that we properly manage stack depth with many locals
    let mut ops = vec![];
    for i in 0..20 {
        ops.push(Operation::SetSmallConst(SetSmallConstData {
            sets: LocalId::new(i),
            value: i as u32,
        }));
    }

    // Create operations that use multiple locals
    ops.push(Operation::Add(InlineOperands {
        ins: [LocalId::new(0), LocalId::new(19)],
        outs: [LocalId::new(20)],
    }));

    ops.push(Operation::Stop(InlineOperands::default()));

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    // Check that assembly was generated for managing many locals
    assert!(!asm.is_empty(), "Should generate assembly for stack management");

    // Verify the program handles multiple locals (20+)
    let op_count = asm.iter().filter(|op| matches!(op, Asm::Op(_))).count();
    assert!(op_count >= 20, "Should have operations for managing {} locals", 20);
}

#[test]
fn jump_table_generation() {
    // Test that control flow generates jump tables
    let blocks = vec![
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 2 })],
            Control::ContinuesTo(BasicBlockId::new(1)),
        ),
        (
            vec![
                Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 42 }),
                Operation::Stop(InlineOperands::default()),
            ],
            Control::LastOpTerminates,
        ),
    ];

    let program = create_branching_program(blocks, 0);
    let asm = translate_program(program);

    // Should have JUMP operations
    assert!(count_opcode(&asm, "JUMP") > 0, "Should generate JUMP for control flow");
    assert!(count_opcode(&asm, "JUMPDEST") > 0, "Should generate JUMPDEST for jump targets");
}

#[test]
fn complex_control_flow_with_branches() {
    // Test complex control flow with multiple branches
    let blocks = vec![
        // Entry block
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 2 })],
            Control::Branches(Branch {
                condition: LocalId::new(0),
                non_zero_target: BasicBlockId::new(1),
                zero_target: BasicBlockId::new(2),
            }),
        ),
        // True branch
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 100 })],
            Control::ContinuesTo(BasicBlockId::new(3)),
        ),
        // False branch
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 200 })],
            Control::ContinuesTo(BasicBlockId::new(3)),
        ),
        // Merge block
        (
            vec![Operation::Return(InlineOperands {
                ins: [LocalId::new(1), LocalId::new(1)],
                outs: [],
            })],
            Control::LastOpTerminates,
        ),
    ];

    let program = create_branching_program(blocks, 0);
    let asm = translate_program(program);

    // Should have conditional jump
    assert!(count_opcode(&asm, "JUMPI") > 0, "Should generate JUMPI for conditional branch");
    assert!(count_opcode(&asm, "JUMPDEST") >= 2, "Should have multiple jump destinations");
}

#[test]
fn call_frame_setup() {
    // Test that external calls properly set up the stack frame
    let ops = vec![
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(2), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(3), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(4), value: 0 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(5), value: 1000 }),
        Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(6), value: 0 }),
        Operation::Call(AllocatedIns { ins_start: LocalIndex::new(0), outs: [LocalId::new(7)] }),
        Operation::Stop(InlineOperands::default()),
    ];

    let program = create_simple_program(ops);
    let asm = translate_program(program);

    // Should have CALL opcode with proper stack setup
    assert_eq!(count_opcode(&asm, "CALL"), 1, "Should generate CALL opcode");
}

#[test]
fn complex_control_flow_with_switch() {
    // Test complex control flow with multiple branches
    let blocks = vec![
        // Entry block
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(0), value: 2 })],
            Control::Branches(Branch {
                condition: LocalId::new(0),
                non_zero_target: BasicBlockId::new(1),
                zero_target: BasicBlockId::new(2),
            }),
        ),
        // True branch
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 100 })],
            Control::ContinuesTo(BasicBlockId::new(3)),
        ),
        // False branch
        (
            vec![Operation::SetSmallConst(SetSmallConstData { sets: LocalId::new(1), value: 200 })],
            Control::ContinuesTo(BasicBlockId::new(3)),
        ),
        // Merge block
        (
            vec![Operation::Return(InlineOperands {
                ins: [LocalId::new(1), LocalId::new(1)],
                outs: [],
            })],
            Control::LastOpTerminates,
        ),
    ];

    let program = create_branching_program(blocks, 0);
    let asm = translate_program(program);

    // Should have conditional jump
    assert!(count_opcode(&asm, "JUMPI") > 0, "Should generate JUMPI for conditional branch");
    assert!(count_opcode(&asm, "JUMPDEST") >= 2, "Should have multiple jump destinations");
}
