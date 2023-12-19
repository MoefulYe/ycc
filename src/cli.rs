use codegen::compiler::{self, Compiler};
use inkwell::context::Context;
use miette::{IntoDiagnostic, NamedSource};
use parser::parse;
use std::{
    fs::{File, OpenOptions},
    io::{Read, Write},
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

impl FileType {
    fn ext(self) -> &'static str {
        match self {
            FileType::Svg => "svg",
            FileType::LlvmIr => "ll",
            FileType::Asm => "asm",
            FileType::Obj => "o",
        }
    }
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
    #[arg(short, long, help = "where the output file to place")]
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
        let mut code = String::new();
        File::open(&self.input)
            .into_diagnostic()?
            .read_to_string(&mut code)
            .into_diagnostic()?;
        let input = self.input.to_str().unwrap_or("unknown");
        let ast = match parse(&code) {
            Ok(ok) => ok,
            Err(err) => return Err(err.with_source_code(NamedSource::new(input, code))),
        };
        let output = match self.output {
            Some(path) => path,
            None => {
                let mut path = PathBuf::from(input);
                path.set_extension(self.type_.ext());
                path
            }
        };
        match self.type_ {
            FileType::Svg => {
                let svg = visual_ast::visualize(&ast).into_diagnostic()?;
                OpenOptions::new()
                    .write(true)
                    .truncate(true)
                    .create(true)
                    .open(output)
                    .into_diagnostic()?
                    .write_all(svg.as_bytes())
                    .into_diagnostic()
            }
            ty => {
                let ctx = &Context::create();
                let mut compiler = Compiler::new(ctx, input);
                if let Err(err) = compiler.codegen(&ast) {
                    return Err(err.with_source_code(NamedSource::new(input, code)));
                }
                compiler.optimize();
                match ty {
                    FileType::LlvmIr => compiler.export_llvm_ir(output),
                    FileType::Asm => compiler.export_asm(output),
                    FileType::Obj => compiler.export_obj(output),
                    _ => unreachable!(),
                }
            }
        }
    }
}
