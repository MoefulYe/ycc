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
    fs,
    io::Write,
    ops::{Deref, DerefMut},
    path::PathBuf,
};

use crate::{
    gen::CodeGen,
    scopes::{Scopes, Symbol},
};

pub struct Loop<'ctx> {
    pub head: BasicBlock<'ctx>,
    pub after: BasicBlock<'ctx>,
}

pub struct Compiler<'ast, 'ctx> {
    pub ctx: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub scopes: Scopes<'ast, 'ctx>,
    pub loops: Vec<Loop<'ctx>>,
}

impl<'ast, 'ctx> Compiler<'ast, 'ctx> {
    pub fn new(ctx: &'ctx Context, mod_name: &str) -> Self {
        Self {
            ctx,
            module: ctx.create_module(mod_name),
            builder: ctx.create_builder(),
            scopes: Scopes::new(),
            loops: vec![],
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

    pub fn guard<'guard>(&'guard mut self) -> ScopedGuard<'guard, 'ast, 'ctx> {
        ScopedGuard::new(self)
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

    pub fn export_llvm_ir(&self, path: PathBuf) -> miette::Result<()> {
        fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(path)
            .into_diagnostic()?
            .write_all(self.module.to_string().as_bytes())
            .into_diagnostic()
    }
    pub fn export_asm(&self, path: PathBuf) -> miette::Result<()> {
        Target::initialize_x86(&InitializationConfig::default());
        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        let target = Target::from_triple(&triple).map_err(|err| miette!("{err}"))?;
        let machine = target
            .create_target_machine(
                &triple,
                TargetMachine::get_host_cpu_name()
                    .to_str()
                    .unwrap_or_default(),
                TargetMachine::get_host_cpu_features()
                    .to_str()
                    .unwrap_or_default(),
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| miette!("unknown error"))?;
        machine
            .write_to_file(&self.module, FileType::Assembly, &path)
            .map_err(|err| miette!("{err}"))
    }
    pub fn export_obj(&self, path: PathBuf) -> miette::Result<()> {
        Target::initialize_x86(&InitializationConfig::default());
        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        let target = Target::from_triple(&triple).map_err(|err| miette!("{err}"))?;
        let machine = target
            .create_target_machine(
                &triple,
                TargetMachine::get_host_cpu_name()
                    .to_str()
                    .unwrap_or_default(),
                TargetMachine::get_host_cpu_features()
                    .to_str()
                    .unwrap_or_default(),
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| miette!("unknown err"))?;
        machine
            .write_to_file(&self.module, FileType::Object, &path)
            .map_err(|err| miette!("{err}"))
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
    pub fn new(compiler: &'compiler mut Compiler<'ast, 'ctx>) -> Self {
        compiler.scopes.enter();
        Self(compiler)
    }
}
