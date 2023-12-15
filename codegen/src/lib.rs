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
    use parser::parse;

    use crate::compiler::Compiler;

    use super::*;

    #[test]
    fn it_works() {
        let ast = parse(
            r##"
            int add(int a, float b[]);
              "##,
        )
        .unwrap();
        let ctx = Context::create();
        let mut compiler = Compiler::new(&ctx, "test");
        compiler.codegen(&ast).unwrap();
        let s = compiler.module.print_to_string().to_string();
        println!("{s}");
    }
}
