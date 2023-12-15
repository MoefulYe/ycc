mod compiler;
mod error;
mod gen;
pub mod scopes;
pub mod ty;
mod val;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use inkwell::context::Context;
    use miette::{ErrReport, NamedSource};
    use parser::parse;

    use crate::compiler::Compiler;

    use super::*;

    #[test]
    fn it_works() {
        let code = r##"
            int add(int a, float b, bool c, int d[2]) {
                return 1;
            }
              "##;
        let ast = parse(code).unwrap();
        let ctx = Context::create();
        let mut compiler = Compiler::new(&ctx, "test");
        let err: miette::Report = compiler.codegen(&ast).unwrap_err().into();
        println!("{:?}", err.with_source_code(NamedSource::new("test", code)));
    }
}
