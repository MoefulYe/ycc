use crate::error::Result;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    types::{FloatType, IntType, VoidType},
    OptimizationLevel,
};
use miette::{miette, IntoDiagnostic};
use std::{
    io::Write,
    ops::{Deref, DerefMut},
};

use crate::{
    gen::CodeGen,
    scopes::{Scopes, Symbol},
};

pub(crate) struct Loop<'ctx> {
    pub head: BasicBlock<'ctx>,
    pub after: BasicBlock<'ctx>,
}

pub struct Compiler<'ast, 'ctx> {
    pub ctx: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub(crate) scopes: Scopes<'ast, 'ctx>,
    pub(crate) loops: Vec<Loop<'ctx>>,
    pub(crate) current_fn: Option<&'ast str>,
}

impl<'ast, 'ctx> Compiler<'ast, 'ctx> {
    pub fn new(ctx: &'ctx Context, mod_name: &str) -> Self {
        Self {
            ctx,
            module: ctx.create_module(mod_name),
            builder: ctx.create_builder(),
            scopes: Scopes::new(),
            loops: vec![],
            current_fn: None,
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

    pub fn codegen(&mut self, ast: &'ast ast::Module<'ast>) -> Result<(), miette::Report> {
        ast.codegen(self).map_err(|err| err.into())
    }

    pub(crate) fn guard<'guard>(&'guard mut self) -> ScopedGuard<'guard, 'ast, 'ctx> {
        ScopedGuard::new(self)
    }

    pub(crate) fn guard_loop<'guard>(
        &'guard mut self,
        head: BasicBlock<'ctx>,
        after: BasicBlock<'ctx>,
    ) -> LoopGuard<'guard, 'ast, 'ctx> {
        LoopGuard::new(self, head, after)
    }

    pub fn optimize(&self) -> bool {
        let manager = PassManager::create(());
        manager.add_instruction_combining_pass();
        manager.add_reassociate_pass();
        manager.add_gvn_pass();
        manager.add_cfg_simplification_pass();
        manager.add_basic_alias_analysis_pass();
        manager.add_promote_memory_to_register_pass();
        manager.add_function_inlining_pass();
        manager.add_global_dce_pass();
        manager.add_constant_merge_pass();
        manager.run_on(&self.module)
    }

    pub fn export_llvm_ir(&self, output: &mut dyn Write) -> miette::Result<()> {
        let content = self.module.to_string();
        output.write_all(content.as_bytes()).into_diagnostic()
    }
    pub fn export_asm(&self, output: &mut dyn Write) -> miette::Result<()> {
        Target::initialize_riscv(&InitializationConfig::default());
        let triple = TargetTriple::create("riscv64-unknown-linux-gnu");
        let target = Target::from_triple(&triple).map_err(|err| miette!("{err}"))?;
        let machine = target
            .create_target_machine(
                &triple,
                "",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| miette!("unknown error"))?;
        let content = machine
            .write_to_memory_buffer(&self.module, FileType::Assembly)
            .map_err(|err| miette!("{err}"))?;
        output.write_all(content.as_slice()).into_diagnostic()
    }
    pub fn export_obj(&self, output: &mut dyn Write) -> miette::Result<()> {
        Target::initialize_riscv(&InitializationConfig::default());
        let triple = TargetTriple::create("riscv64-unknown-linux-gnu");
        let target = Target::from_triple(&triple).map_err(|err| miette!("{err}"))?;
        let machine = target
            .create_target_machine(
                &triple,
                "",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| miette!("unknown err"))?;
        let content = machine
            .write_to_memory_buffer(&self.module, FileType::Object)
            .map_err(|err| miette!("{err}"))?;
        output.write_all(content.as_slice()).into_diagnostic()
    }

    pub(crate) fn no_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
    }
}

pub(crate) struct ScopedGuard<'compiler, 'ast, 'ctx>(&'compiler mut Compiler<'ast, 'ctx>);

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
    pub fn new(compiler: &'compiler mut Compiler<'ast, 'ctx>) -> Self {
        compiler.scopes.enter();
        Self(compiler)
    }
}

pub(crate) struct LoopGuard<'compiler, 'ast, 'ctx>(&'compiler mut Compiler<'ast, 'ctx>);

impl<'compiler, 'ast, 'ctx> LoopGuard<'compiler, 'ast, 'ctx> {
    pub fn new(
        compiler: &'compiler mut Compiler<'ast, 'ctx>,
        head: BasicBlock<'ctx>,
        after: BasicBlock<'ctx>,
    ) -> Self {
        compiler.loops.push(Loop { head, after });
        LoopGuard(compiler)
    }
}

impl<'compiler, 'ast, 'ctx> Drop for LoopGuard<'compiler, 'ast, 'ctx> {
    fn drop(&mut self) {
        self.loops.pop().expect("unreachable");
    }
}

impl<'compiler, 'ast, 'ctx> Deref for LoopGuard<'compiler, 'ast, 'ctx> {
    type Target = Compiler<'ast, 'ctx>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'compiler, 'ast, 'ctx> DerefMut for LoopGuard<'compiler, 'ast, 'ctx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}
