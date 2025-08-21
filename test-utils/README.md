# Test Utils

This module contains parsing utilities and snapshot tests for the Ethereum IR compiler.

## Snapshot Testing

We use [insta](https://insta.rs/) for snapshot testing the IR parser and display output. Snapshot tests ensure that the parser correctly handles various IR constructs and that the Display implementation produces consistent, readable output.

### Running Snapshot Tests

To run all snapshot tests:
```bash
cargo test -p test-utils snapshot_tests::
```

To run a specific snapshot test:
```bash
cargo test -p test-utils snapshot_tests::test_simple_function
```

### Updating Snapshots

When you make changes to the parser or Display implementation, snapshots may need to be updated.

1. Run the tests to generate new snapshots:
   ```bash
   cargo test -p test-utils snapshot_tests::
   ```

2. Review the changes using cargo-insta (install with `cargo install cargo-insta`):
   ```bash
   cargo insta review
   ```

3. Accept all changes:
   ```bash
   cargo insta accept
   ```

### Test Structure

Snapshot tests are located in `src/parser/snapshot_tests.rs`. Each test:
1. Parses IR syntax from a string
2. Converts the AST to an `EthIRProgram`
3. Uses the Display implementation to generate output
4. Compares against a stored snapshot

### IR Syntax Notes

The parser has specific requirements:
- `iret` is control flow, not a statement. It must appear at the end of a basic block after at least one statement
- Locals are scoped to basic blocks in the functional IR design
- Basic blocks can have input locals (e.g., `block_name input1 input2 {`) which create new locals in that block's scope
- Basic blocks can have output locals (e.g., `block_name -> output1 output2 {`) which must be defined in that block
- Operations can only take identifiers as arguments, not literals. To use a literal, first assign it to a local
- Hex literals should use the `0x` prefix
- Decimal literals can be used for small values
- Data definitions must appear before functions without blank lines between them

### Parser Limitations

Due to current parser implementation:
- `iret` cannot be the only content in a block - add a dummy assignment like `result = value` before `iret result`
- Literal values cannot be used directly as operation arguments - assign them to locals first
- Functions can only return a single value through `iret`, even if they declare multiple outputs

### Example IR

```ir
data greeting 0x48656c6c6f
fn main 1:
    entry a b {
        c = add a b
        => @done
    }
    done c {
        iret c
    }
```

### Fixture Files

Test fixtures are located in the `fixtures/` directory. These contain more complex IR examples that test various features:
- `simple_function.ir` - Basic arithmetic operations
- `control_flow.ir` - Branching and control flow
- `data_and_memory.ir` - Data segments and memory operations
- `internal_calls.ir` - Function calls within the IR
- `complex_example.ir` - Comprehensive example with multiple features