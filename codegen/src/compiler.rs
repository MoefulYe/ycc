use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{FloatType, IntType, VoidType},
};

use crate::scope::SymbolScope;

pub struct CodeGener<'ctx> {
    //TODO
    ctx: Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    symbol_table: SymbolScope<'ctx, 'ctx>,
}

impl<'ctx> CodeGener<'ctx> {
    #[inline]
    pub fn bool_type(&'ctx self) -> IntType<'ctx> {
        self.ctx.bool_type()
    }

    #[inline]
    pub fn i32_type(&'ctx self) -> IntType<'ctx> {
        self.ctx.i32_type()
    }

    #[inline]
    pub fn f32_type(&'ctx self) -> FloatType<'ctx> {
        self.ctx.f32_type()
    }

    #[inline]
    pub fn void_type(&'ctx self) -> VoidType<'ctx> {
        self.ctx.void_type()
    }
}
