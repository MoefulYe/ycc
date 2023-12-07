use std::ops::{Deref, DerefMut};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{FloatType, IntType, VoidType},
};

use crate::scope::Symbols;

pub struct Compiler<'ctx, 'input>
where
    'ctx: 'input,
{
    ctx: Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    symbols: Symbols<'input, 'ctx>,
}

impl<'ctx, 'input> Compiler<'ctx, 'input> {
    pub fn new(module_name: &str) -> Self {
        let ctx = Context::create();
        let module = ctx.create_module(module_name);
        let builder = ctx.create_builder();
        Self {
            ctx,
            module,
            builder,
            symbols: Symbols::new(),
        }
    }

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

pub struct ScopedGuard<'a, 'input> {
    compiler: &'a mut Compiler<'a, 'input>,
}

impl<'a, 'input> Drop for ScopedGuard<'a, 'input> {
    fn drop(&mut self) {
        println!("droped");
    }
}

impl<'a, 'input> Deref for ScopedGuard<'a, 'input> {
    type Target = Compiler<'a, 'input>;

    fn deref(&self) -> &Self::Target {
        self.compiler
    }
}

impl<'a, 'input> DerefMut for ScopedGuard<'a, 'input> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.compiler
    }
}

impl<'a, 'input> ScopedGuard<'a, 'input> {
    pub fn new(compiler: &'a mut Compiler<'a, 'input>) -> Self {
        Self { compiler }
    }
}
