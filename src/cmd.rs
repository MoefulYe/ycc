use codegen::compiler::Compiler;
use inkwell::context::Context;
use miette::{IntoDiagnostic, NamedSource};
use parser::parse;
use std::{
    fs::{File, OpenOptions},
    io::{read_to_string, stdout, Write},
    path::PathBuf,
};

use clap::{Parser, ValueEnum};

#[derive(Debug, PartialEq, Eq, ValueEnum, Clone, Copy)]
pub enum FileType {
    Svg,
    LlvmIr,
    Asm,
    Obj,
}

#[derive(Parser)]
#[command(
    name = "ycc",
    version = "0.1.0",
    author = "ashenye",
    about = "Ycc is Yet another miniC Compiler"
)]
pub struct Cmd {
    input: PathBuf,
    #[arg(
        short,
        long,
        help = "where the output file to place, default print at stdout"
    )]
    output: Option<PathBuf>,
    #[arg(
        short = 't',
        long = "type",
        value_enum,
        help = "what type file to output",
        default_value_t = FileType::LlvmIr
    )]
    type_: FileType,
}

impl Cmd {
    pub fn exec(self) -> miette::Result<()> {
        let code = read_to_string(File::open(&self.input).into_diagnostic()?).into_diagnostic()?;
        let input = self.input.to_str().unwrap_or("unknown");
        let ast = match parse(&code) {
            Ok(ok) => ok,
            Err(err) => return Err(err.with_source_code(NamedSource::new(input, code))),
        };
        let mut output: Box<dyn Write> = match self.output {
            Some(output) => Box::new(
                OpenOptions::new()
                    .write(true)
                    .truncate(true)
                    .create(true)
                    .open(output)
                    .into_diagnostic()?,
            ),
            None => Box::new(stdout()),
        };

        match self.type_ {
            FileType::Svg => {
                let svg = visual_ast::visualize(&ast).into_diagnostic()?;
                output.write_all(svg.as_bytes()).into_diagnostic()?;
                Ok(())
            }
            ty => {
                let ctx = &Context::create();
                let mut compiler = Compiler::new(ctx, input);
                if let Err(err) = compiler.codegen(&ast) {
                    return Err(err.with_source_code(NamedSource::new(input, code)));
                }
                compiler.optimize();
                match ty {
                    FileType::LlvmIr => compiler.export_llvm_ir(&mut output),
                    FileType::Asm => compiler.export_asm(&mut output),
                    FileType::Obj => compiler.export_obj(&mut output),
                    _ => unreachable!(),
                }
            }
        }
    }
}
