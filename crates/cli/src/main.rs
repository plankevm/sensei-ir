use clap::Parser;
use sir_debug_backend::translate_program;
use sir_parser::{EmitConfig, parse_or_panic};
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
};

#[derive(Parser)]
#[command(name = "sir")]
#[command(about = "Sensei IR to EVM bytecode compiler", long_about = None)]
#[command(version)]
struct Cli {
    /// Input file (use '-' or omit for stdin)
    input: Option<PathBuf>,

    /// Compile only init function (no main)
    #[arg(long)]
    init_only: bool,

    /// Override init function name
    #[arg(long, default_value = "init")]
    init_name: String,

    /// Override main function name
    #[arg(long, default_value = "main")]
    main_name: String,

    /// Use maximized assembly mode (default: minimized)
    #[arg(long)]
    maximized: bool,
}

fn read_input(input: Option<PathBuf>) -> String {
    let use_stdin = match &input {
        None => true,
        Some(path) => path.to_str() == Some("-"),
    };

    if use_stdin {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer).expect("failed to read from stdin");
        buffer
    } else {
        let path = input.unwrap();
        fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read file '{}': {}", path.display(), e))
    }
}

fn main() {
    let cli = Cli::parse();

    // Read input source
    let source = read_input(cli.input);

    // Build emit configuration
    let config = if cli.init_only {
        EmitConfig::init_only_with_name(&cli.init_name)
    } else {
        EmitConfig::with_names(&cli.init_name, &cli.main_name)
    };

    // Parse IR to EthIRProgram
    let program = parse_or_panic(&source, config);

    // Translate to assembly
    let asm = translate_program(program);

    // Assemble to bytecode
    let bytecode = if cli.maximized {
        let (_, bytecode) =
            evm_glue::assemble_maximized(&asm, true).expect("failed to assemble bytecode");
        bytecode
    } else {
        let (_, bytecode) =
            evm_glue::assemble_minimized(&asm, true).expect("failed to assemble bytecode");
        bytecode
    };

    // Format and print output
    print!("0x");
    for byte in bytecode {
        print!("{:02x}", byte);
    }
    println!();
}
