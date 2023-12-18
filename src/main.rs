use codegen::compiler;
use inkwell::context::Context;
use miette::{NamedSource, Result};
use parser::parse;

fn main() -> Result<()> {
    let code = r##"

    const int a = 1;
int main() { 
    int b = 2;
    int c = a + b;
    return c;
 }
            "##;
    let ast = parse(code).map_err(|err| err.with_source_code(NamedSource::new("test.c", code)))?;
    let ctx = Context::create();
    let mut compiler = compiler::Compiler::new(&ctx, "test.c");
    compiler
        .codegen(&ast)
        .map_err(|err| err.with_source_code(NamedSource::new("test.c", code)))?;
    compiler.optimize();
    println!("{}", compiler.module.print_to_string().to_string());
    Ok(())
}
