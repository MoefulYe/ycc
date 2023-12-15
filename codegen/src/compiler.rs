use crate::error::Result;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{FloatType, IntType, VoidType},
};
use std::ops::{Deref, DerefMut};

use crate::{
    gen::CodeGen,
    scopes::{Scopes, Symbol},
};

pub struct Compiler<'ast, 'ctx> {
    pub ctx: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub scopes: Scopes<'ast, 'ctx>,
}

impl<'ast, 'ctx> Compiler<'ast, 'ctx> {
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
    pub fn ctx(&'ctx self) -> &'ctx Context {
        self.ctx
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

    pub fn new_symbol(
        &mut self,
        symbol: &'ast str,
        val: impl Into<Symbol<'ctx>>,
    ) -> Result<(), ()> {
        self.scopes.insert(symbol, val)
    }

    pub fn codegen(&mut self, ast: &'ast ast::Module<'ast>) -> Result<()> {
        ast.codegen(self)
    }
}

pub struct ScopedGuard<'compiler, 'ast, 'ctx>(&'compiler mut Compiler<'ast, 'ctx>);

impl<'compiler, 'ast, 'ctx> Drop for ScopedGuard<'compiler, 'ast, 'ctx> {
    fn drop(&mut self) {
        self.scopes.exit();
    }
}

impl<'compiler, 'ast, 'ctx> DerefMut for ScopedGuard<'compiler, 'ast, 'ctx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl<'compiler, 'ast, 'ctx> Deref for ScopedGuard<'compiler, 'ast, 'ctx> {
    type Target = Compiler<'ast, 'ctx>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'compiler, 'ast, 'ctx> ScopedGuard<'compiler, 'ast, 'ctx> {
    fn new(compiler: &'compiler mut Compiler<'ast, 'ctx>) -> Self {
        compiler.scopes.enter();
        Self(compiler)
    }
}
