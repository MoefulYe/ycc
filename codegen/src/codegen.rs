use crate::compiler::CodeGener;

pub trait CodeGen {
    type Out;
    fn codegen<'ctx>(&self, codegener: CodeGener<'ctx>) -> Self::Out;
}
