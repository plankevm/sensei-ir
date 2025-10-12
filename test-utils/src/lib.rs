/// Asserts that two strings are equal, showing a detailed diff if they differ.
///
/// # Arguments
/// * `actual` - The actual string value
/// * `expected` - The expected string value
/// * `context_name` - Name to use in the panic message (e.g., "Parse format", "IR display")
/// * `additional_context` - Optional additional context to display (e.g., input that generated the
///   actual value)
pub fn assert_strings_with_diff(
    actual: &str,
    expected: &str,
    context_name: &str,
    additional_context: Option<(&str, &str)>,
) {
    let actual = actual.trim();
    let expected = expected.trim();

    if actual != expected {
        if let Some((context_label, context_value)) = additional_context {
            eprintln!("=== {} ===\n{}\n", context_label, context_value.trim());
        }
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

        panic!("{} mismatch", context_name);
    }
}
