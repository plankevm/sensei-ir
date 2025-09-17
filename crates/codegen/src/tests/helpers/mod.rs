pub mod builders;
pub mod constants;
pub mod macros;

// Re-export commonly used items for convenience
pub use builders::{
    EvmBuilder, OperationTestBuilder, TestProgram, assert_opcode_counts, assert_opcode_sequence,
    build_binary_op_with_return, compile_to_bytecode, count_opcode, create_branching_program,
    create_multi_function_program, create_ops_with_return, create_program_with_data,
    create_program_with_switch, create_return_for_local, create_simple_program,
    execute_and_get_result, execute_and_get_result_with_calldata, execute_and_verify_halt,
    execute_and_verify_result, execute_and_verify_revert, execute_and_verify_stop,
    execute_blocks_and_extract, execute_bytecode_raw, execute_operations_with_return,
    execute_storage_operations, ir_to_bytecode, local_memory_offset, memory_load, memory_store,
    set_locals, set_zero_locals, storage_load, storage_store, with_return,
};
pub use constants::*;
