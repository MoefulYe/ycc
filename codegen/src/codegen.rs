use crate::compiler::Compiler;

pub trait CodeGen {
    type Out;
    fn codegen<'ctx, 'input>(&self, codegener: Compiler<'ctx, 'input>) -> Self::Out;
}
