use std::ops::{Deref, DerefMut};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{FloatType, IntType, VoidType},
};

use crate::scope::Scopes;

pub struct Compiler<'ctx, 'input>
where
    'ctx: 'input,
{
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    scopes: Scopes<'input, 'ctx>,
}

impl<'ctx, 'input> Compiler<'ctx, 'input> {
    pub fn new(ctx: &'ctx Context, mod_name: &str) -> Self {
        Self {
            ctx,
            module: ctx.create_module(mod_name),
            builder: ctx.create_builder(),
            scopes: Scopes::new(),
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
