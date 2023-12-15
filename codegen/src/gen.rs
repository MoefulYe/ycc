use crate::{
    compiler::{Compiler, ScopedGuard},
    error::{CodeGenError, Result},
    ty::{TryIntoFuncType, TryIntoLLVMType},
    val::TryIntoLLVMValue,
};
use ast::{ConstDecl, FuncDef, FuncProto, GlobalVarDecl, Module, Sourced, Unit};
use inkwell::{
    values::{BasicValue, FunctionValue},
    AddressSpace,
};

pub trait CodeGen<'ast, 'ctx> {
    type Out;

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out>;
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Module<'ast> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let Module(units) = self;
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
            Unit::FuncDecl(unit) => unit.codegen(compiler).map(|_| ()),
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
                let ty = val.get_type();
                let global = compiler.module.add_global(ty, None, ident);
                global.set_initializer(&val);
                compiler
                    .scopes
                    .insert(ident, (global.as_pointer_value(), ty, ty))
                    .map_err(|_| CodeGenError::DuplicateIdentifier {
                        loc: loc.to_owned(),
                        ident: ident.to_string(),
                    })
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
    type Out = FunctionValue<'ctx>;

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        match compiler.module.get_function(self.1.ident.1) {
            Some(_) => Err(CodeGenError::RedefinedFunction {
                loc: self.1.ident.0,
                ident: self.1.ident.1.to_string(),
            }),
            None => {
                let ident = self.1.ident.1;
                let fn_type = self.fn_type(compiler.ctx)?;
                Ok(compiler.module.add_function(ident, fn_type, None))
            }
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<FuncDef<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let (_, this) = self;
        let FuncDef { proto, body } = this;
        let ident = proto.1.ident.1;
        let func = match compiler.module.get_function(ident) {
            Some(func) => {
                if func.get_type() != proto.fn_type(compiler.ctx)? {
                    return Err(CodeGenError::ConflictingDeclaration {
                        loc: proto.1.ident.0,
                        ident: ident.to_owned(),
                    });
                } else {
                    func
                }
            }
            None => {
                let fn_type = proto.fn_type(compiler.ctx)?;
                compiler.module.add_function(ident, fn_type, None)
            }
        };
        let compiler = ScopedGuard::new(compiler);
        let entry = compiler.ctx.append_basic_block(func, "entry");
        compiler.builder.position_at_end(entry);
        Ok(())
    }
}
