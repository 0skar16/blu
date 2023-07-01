use std::path::PathBuf;

use anyhow::Result;
use blu::parser::parse_blu;
use clap::{Parser, Subcommand, Args};
#[derive(Parser, Debug)]
#[command(author, version, about="A compiler for the Blu language", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand,Debug)]
enum Commands {
    Compile(CompileArgs),
}

#[derive(Args,Debug)]
struct CompileArgs {
    input: PathBuf,
    #[arg(long,short)]
    output: String,
}
fn main() -> Result<()> {
    //unsafe { backtrace_on_stack_overflow::enable() };
    let cli = Cli::try_parse()?;
    match cli.command {
        Commands::Compile(CompileArgs { input, output }) => {
            let code = std::fs::read_to_string(input)?;
            let ast = parse_blu(&code)?;
            dbg!(ast);
        },
    }
    Ok(())
}
