use clap::Parser;
use graphviz_rust::{
    cmd::Format,
    exec,
    printer::{DotPrinter, PrinterContext},
};
use miette::NamedSource;
use parser::parse;
use std::{
    fs::OpenOptions,
    io::{Read, Write},
    path::PathBuf,
};
use visual_ast::drawer::draw;

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

fn main() {
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
            let g = draw(&ast);
            let svg = exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])
                .expect("failed to get svg");
            OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(&output)
                .expect("failed to open output file")
                .write_all(svg.as_bytes())
                .expect("failed to write output file");
        }
        Err(err) => {
            let err =
                err.with_source_code(NamedSource::new(input.to_str().unwrap_or("unknown"), code));
            eprintln!("{err:?}");
        }
    };
}
