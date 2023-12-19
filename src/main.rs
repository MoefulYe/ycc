mod cli;
use clap::Parser;
use cli::Cmd;

fn main() -> miette::Result<()> {
    //     let code = r##"
    //
    // int main() {
    //     int c;
    //     while(c < 9) {
    //         c = c + 1;
    //     }
    //     return c;
    //  }
    //             "##;
    //     let ast = parse(code).map_err(|err| err.with_source_code(NamedSource::new("test.c", code)))?;
    //     let ctx = Context::create();
    //     let mut compiler = compiler::Compiler::new(&ctx, "test.c");
    //     compiler
    //         .codegen(&ast)
    //         .map_err(|err| err.with_source_code(NamedSource::new("test.c", code)))?;
    //     compiler.optimize();
    //     println!("{}", compiler.module.print_to_string().to_string());
    //     Ok(())
    Cmd::parse().exec()
}
