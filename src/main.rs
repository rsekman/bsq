use std::error::Error;
use std::fs::File;

use clap::Parser as ArgParser;

use bsq::program::compile;

#[derive(ArgParser)]
#[command(name = "bsq", version = "0.1", about)]
struct CliOptions {
    #[arg(help = "Program to execute.")]
    program: String,
    #[arg(help = "Input files to process. If empty, read from stdin.")]
    input: Vec<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = CliOptions::parse();

    let program = compile(&args.program)?;

    if args.input.is_empty() {
        let stdin = std::io::stdin();
        let mut stdin = stdin.lock();
        let _ = program.run(&mut stdin)?;
    } else {
        for path in args.input {
            let mut f = File::open(path)?;
            let _ = program.run(&mut f)?;
        }
    }
    return Ok(());
}
