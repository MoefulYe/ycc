use std::ops::{Deref, DerefMut};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{FloatType, IntType, VoidType},
};

use crate::scopes::{Scopes, Symbol};

pub struct Compiler<'ctx, 'input> {
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

    #[inline]
    pub fn ctx(&'ctx self) -> &Context {
        &self.ctx
    }

    #[inline]
    pub fn module(&'ctx self) -> &Module<'ctx> {
        &self.module
    }

    #[inline]
    pub fn builder(&'ctx self) -> &Builder<'ctx> {
        &self.builder
    }

    pub fn scope(&mut self) {
        self.scopes.enter()
    }

    pub fn unscope(&mut self) {
        self.scopes.enter()
    }

    pub fn symbol(&self, symbol: &str) -> Option<Symbol<'ctx>> {
        self.scopes.find(symbol)
    }

    pub fn new_symbol(&mut self, symbol: &'input str, val: Symbol<'ctx>) -> Result<(), ()> {
        self.scopes.insert(symbol, val)
    }
}
