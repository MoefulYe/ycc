use crate::{
    compiler::Compiler,
    error::{CodeGenError, Result},
    ty::TryIntoLLVMType,
    value::TryIntoLLVMValue,
};
use ast::{ConstDecl, FuncDef, FuncProto, GlobalVarDecl, Module, Sourced, Unit};
use inkwell::{values::BasicValue, AddressSpace};

pub trait CodeGen<'ast, 'ctx> {
    type Out;

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out>;
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<Module<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let Module(units) = &self.1;
        for unit in units {
            unit.codegen(compiler)?
        }
        Ok(())
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<Unit<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        match &self.1 {
            Unit::ConstDecl(unit) => unit.codegen(compiler),
            Unit::GlobalVarDecl(unit) => unit.codegen(compiler),
            Unit::FuncDecl(unit) => unit.codegen(compiler),
            Unit::FuncDef(unit) => unit.codegen(compiler),
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<ConstDecl<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let (_, this) = self;
        let ConstDecl {
            ty,
            ident: (loc, ident),
            init,
        } = this;
        let val = match init.llvm_value(ty, compiler.ctx)? {
            inkwell::values::BasicValueEnum::ArrayValue(val) => {
                let ty = val.get_type();
                let arr = compiler
                    .module
                    .add_global(ty, Some(AddressSpace::default()), ident);
                arr.set_constant(true);
                arr.set_initializer(&val);
                arr.as_basic_value_enum()
            }
            val => val,
        };
        compiler
            .new_symbol(ident, val)
            .map_err(|_| CodeGenError::DuplicateIdentifier {
                loc: loc.to_owned(),
                ident: ident.to_string(),
            })
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<GlobalVarDecl<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let (_, this) = self;
        let GlobalVarDecl {
            ty,
            ident: (loc, ident),
            init,
        } = this;
        match init {
            Some(init) => {
                let val = init.llvm_value(ty, compiler.ctx)?;
                let global = compiler.module.add_global(val.get_type(), None, ident);
                Ok(())
            }
            None => {
                let ty = ty.llvm_type(compiler.ctx)?;
                let global = compiler.module.add_global(ty, None, ident);
                compiler
                    .scopes
                    .insert(ident, global.as_basic_value_enum())
                    .map_err(|_| CodeGenError::DuplicateIdentifier {
                        loc: loc.to_owned(),
                        ident: ident.to_string(),
                    })
            }
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<FuncProto<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        todo!()
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<FuncDef<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        todo!()
    }
}
