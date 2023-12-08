use ast::{ConstDecl, Module, Sourced};

use crate::{compiler::Compiler, error::Result, value::TryIntoLLVMValue};

pub trait CodeGen<'input, 'ctx> {
    type Out;
    fn codegen(&self, compiler: &mut Compiler<'ctx, 'input>) -> Result<Self::Out>;
}

impl<'input, 'ctx> CodeGen<'input, 'ctx> for Module<'input> {
    type Out = ();

    fn codegen(&self, compiler: &mut Compiler<'ctx, 'input>) -> Result<Self::Out> {
        Ok(())
    }
}

impl<'input, 'ctx> CodeGen<'input, 'ctx> for Sourced<ConstDecl<'input>> {
    type Out = ();

    fn codegen(&self, compiler: &mut Compiler<'ctx, 'input>) -> Result<Self::Out> {
        let (loc, stmt) = self;
        let ConstDecl { ty, ident, init } = stmt;
        let llvm_val = init.llvm_value(ty, compiler)?;
        compiler.new_symbol(ident.1, llvm_val.into());
        Ok(())
    }
}
