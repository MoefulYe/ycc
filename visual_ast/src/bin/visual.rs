use clap::Parser;
use miette::IntoDiagnostic;
use miette::NamedSource;
use parser::parse;
use std::{
    fs::OpenOptions,
    io::{Read, Write},
    path::PathBuf,
};
use visual_ast::visualize;

#[derive(Parser, Debug)]
#[command(
    name = "ycc ast visualizer",
    version = "0.1.0",
    author = "ashenye",
    about = "ycc ast visualizer"
)]
struct Cli {
    input: PathBuf,
    #[arg(short, long)]
    output: Option<PathBuf>,
}

fn main() -> miette::Result<()> {
    let args = Cli::parse();
    let Cli { input, output } = args;
    let output = output.unwrap_or_else(|| "output.svg".into());
    let mut code = String::new();
    OpenOptions::new()
        .read(true)
        .open(&input)
        .expect("failed to open input file")
        .read_to_string(&mut code)
        .expect("failed to read input file");
    match parse(&code) {
        Ok(ast) => {
            let svg = visualize(&ast).into_diagnostic()?;
            crate::OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(&output)
                .expect("failed to open output file")
                .write_all(svg.as_bytes())
                .expect("failed to write output file");
        }
        Err(err) => {
            return Err(
                err.with_source_code(NamedSource::new(input.to_str().unwrap_or("unknown"), code))
            );
        }
    };
    Ok(())
}
