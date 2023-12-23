use crate::{
    compiler::Compiler,
    error::{CodeGenError, Result},
    scopes::Symbol::*,
    ty::{ndim_arr_of, GetElemType, TryIntoFuncType, TryIntoLLVMType},
    util::prefix,
    val::TryIntoLLVMValue,
};
use ast::{
    AssignStmt, Binary, Block, ConstDecl, Expr, FuncDef, FuncProto, GlobalVarDecl, IfStmt, LValue,
    Literal, Module, PrimExpr, Sourced, Stmt, Unary, UnaryOp, Unit, VarDecl, WhileStmt,
};
use inkwell::{
    basic_block::BasicBlock,
    types::BasicType,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue},
    AddressSpace, FloatPredicate, IntPredicate,
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
        match init.llvm_value(ty, compiler.ctx)? {
            inkwell::values::BasicValueEnum::ArrayValue(val) => {
                let elem_ty = ty.elem_type(compiler.ctx)?;
                let pointee_ty = val.get_type().as_basic_type_enum();
                let arr = match compiler.current_fn {
                    Some(func) => compiler.module.add_global(
                        pointee_ty,
                        Some(AddressSpace::default()),
                        &prefix(func, ident),
                    ),
                    None => {
                        compiler
                            .module
                            .add_global(pointee_ty, Some(AddressSpace::default()), ident)
                    }
                };
                let ptr = arr.as_pointer_value();
                arr.set_constant(true);
                arr.set_initializer(&val);
                compiler
                    .new_symbol(
                        ident,
                        ArrImmut {
                            ptr,
                            elem_ty,
                            pointee_ty,
                        },
                    )
                    .map_err(|_| CodeGenError::DuplicateIdentifier {
                        loc: loc.to_owned(),
                        ident: ident.to_string(),
                    })
            }
            val => compiler.new_symbol(ident, Const(val)).map_err(|_| {
                CodeGenError::DuplicateIdentifier {
                    loc: loc.to_owned(),
                    ident: ident.to_string(),
                }
            }),
        }
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
                let pointee_ty = val.get_type();
                let global =
                    compiler
                        .module
                        .add_global(pointee_ty, Some(AddressSpace::default()), ident);
                global.set_initializer(&val);
                let symbol = if pointee_ty.is_array_type() {
                    ArrMut {
                        ptr: global.as_pointer_value(),
                        elem_ty: ty.elem_type(compiler.ctx)?,
                        pointee_ty,
                    }
                } else {
                    PrimMut {
                        ptr: global.as_pointer_value(),
                        pointee_ty,
                    }
                };
                compiler.scopes.insert(ident, symbol).map_err(|_| {
                    CodeGenError::DuplicateIdentifier {
                        loc: loc.to_owned(),
                        ident: ident.to_string(),
                    }
                })
            }
            None => {
                let pointee_ty = ty.llvm_type(compiler.ctx)?;
                let global =
                    compiler
                        .module
                        .add_global(pointee_ty, Some(AddressSpace::default()), ident);
                let symbol = if pointee_ty.is_array_type() {
                    ArrMut {
                        ptr: global.as_pointer_value(),
                        elem_ty: ty.elem_type(compiler.ctx)?,
                        pointee_ty,
                    }
                } else {
                    PrimMut {
                        ptr: global.as_pointer_value(),
                        pointee_ty,
                    }
                };
                compiler.scopes.insert(ident, symbol).map_err(|_| {
                    CodeGenError::DuplicateIdentifier {
                        loc: loc.to_owned(),
                        ident: ident.to_string(),
                    }
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
        let (func_loc, this) = self;
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
        compiler.current_fn = Some(ident);
        let compiler = &mut compiler.guard();
        let entry = compiler.ctx.append_basic_block(func, "entry");
        compiler.builder.position_at_end(entry);

        for (idx, (_, param)) in proto.1.params.1.iter().enumerate() {
            let (ident_loc, ident) = &param.ident;
            let ty = match &param.ty.1 {
                ast::Type::Prim(prim) => prim.llvm_type(compiler.ctx)?.as_basic_type_enum(),
                ast::Type::Array(prim, _) => prim
                    .llvm_type(compiler.ctx)?
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            };
            let ptr = compiler.builder.build_alloca(ty, ident);
            let val = func.get_nth_param(idx as u32).unwrap();
            val.set_name(ident);
            compiler.builder.build_store(ptr, val);
            let pointee_ty = param.ty.llvm_type(compiler.ctx)?;
            let symbol = if pointee_ty.is_array_type() {
                ArrMut {
                    ptr: match val {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => unreachable!(),
                    },
                    elem_ty: param.ty.elem_type(compiler.ctx)?,
                    pointee_ty,
                }
            } else {
                PrimMut { ptr, pointee_ty }
            };
            compiler.scopes.insert(ident, symbol).map_err(|_| {
                CodeGenError::DuplicateIdentifier {
                    loc: ident_loc.to_owned(),
                    ident: ident.to_string(),
                }
            })?;
        }

        body.codegen(&mut compiler.guard())?;
        if compiler.no_terminator() && func.get_type().get_return_type().is_none() {
            compiler.builder.build_return(None);
        }
        compiler.current_fn = None;
        if !func.verify(true) {
            Err(CodeGenError::VerifyFunction {
                loc: func_loc.to_owned(),
                ident: ident.to_string(),
            })
        } else {
            Ok(())
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<Block<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        for stmt in &self.1 {
            stmt.codegen(compiler)?;
        }
        Ok(())
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<Stmt<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        match &self.1 {
            Stmt::ConstDecl(stmt) => stmt.codegen(compiler),
            Stmt::VarDecl(stmt) => stmt.codegen(compiler),
            Stmt::Assign(stmt) => stmt.codegen(compiler),
            Stmt::If(stmt) => {
                let (any_no_term, exit_block) = stmt.codegen(compiler)?;
                if !any_no_term {
                    exit_block.remove_from_function().unwrap();
                }
                compiler.builder.position_at_end(exit_block);
                Ok(())
            }
            Stmt::While(stmt) => stmt.codegen(compiler),
            Stmt::Break((loc, _)) => {
                let after = compiler.loops.last().map(|l| l.after).ok_or_else(|| {
                    CodeGenError::BreakOutsideLoop {
                        loc: loc.to_owned(),
                    }
                })?;
                compiler.builder.build_unconditional_branch(after);
                Ok(())
            }
            Stmt::Continue((loc, _)) => {
                let head = compiler.loops.last().map(|l| l.head).ok_or_else(|| {
                    CodeGenError::ContinueOutsideLoop {
                        loc: loc.to_owned(),
                    }
                })?;
                compiler.builder.build_unconditional_branch(head);
                Ok(())
            }
            Stmt::Return(stmt) => match &stmt.1 {
                Some(expr) => {
                    let ret = expr.codegen(compiler)?;
                    compiler.builder.build_return(Some(&ret));
                    Ok(())
                }
                None => {
                    compiler.builder.build_return(None);
                    Ok(())
                }
            },
            Stmt::Expr(stmt) => stmt.codegen(compiler).map(|_| ()),
            Stmt::Block(block) => block.codegen(&mut compiler.guard()),
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<VarDecl<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let (loc, this) = self;
        let VarDecl { ty, ident, init } = this;
        match &ty.1 {
            ast::Type::Prim(ty) => {
                let pointee_ty = ty.llvm_type(compiler.ctx)?;
                let ptr = compiler.builder.build_alloca(pointee_ty, ident.1);
                let symbol = PrimMut { ptr, pointee_ty };
                compiler.scopes.insert(ident.1, symbol).map_err(|_| {
                    CodeGenError::DuplicateIdentifier {
                        loc: ident.0,
                        ident: ident.1.to_string(),
                    }
                })?;
                if let Some(init) = init {
                    let val = init.codegen(compiler)?;
                    compiler.builder.build_store(ptr, val);
                }
                Ok(())
            }
            ast::Type::Array(prim, (_, dims)) => {
                let elem_ty = prim.llvm_type(compiler.ctx)?;
                let pointee_ty = ndim_arr_of(elem_ty, dims).as_basic_type_enum();
                let ptr = compiler.builder.build_alloca(pointee_ty, ident.1);
                if let Some((init_loc, init)) = init {
                    let src = compiler.module.add_global(
                        pointee_ty,
                        Some(AddressSpace::default()),
                        &prefix(unsafe { compiler.current_fn.unwrap_unchecked() }, ident.1),
                    );
                    let init = if let Expr::Prim((_, PrimExpr::Literal(lit))) = init {
                        lit.llvm_value(ty, compiler.ctx)?
                    } else {
                        return Err(CodeGenError::IllegalArrayInitializer {
                            loc: init_loc.to_owned(),
                        });
                    };
                    src.set_linkage(inkwell::module::Linkage::Private);
                    src.set_unnamed_addr(true);
                    src.set_visibility(inkwell::GlobalVisibility::Hidden);
                    src.set_constant(true);
                    src.set_initializer(&init);
                    compiler
                        .builder
                        .build_memcpy(
                            ptr,
                            4,
                            src.as_pointer_value(),
                            4,
                            pointee_ty.size_of().unwrap(),
                        )
                        .map_err(|err| CodeGenError::Unknown {
                            loc: loc.to_owned(),
                            msg: err.to_string(),
                        })?;
                }
                let symbol = ArrMut {
                    ptr,
                    elem_ty,
                    pointee_ty,
                };
                compiler.scopes.insert(ident.1, symbol).map_err(|_| {
                    CodeGenError::DuplicateIdentifier {
                        loc: ident.0,
                        ident: ident.1.to_string(),
                    }
                })
            }
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<AssignStmt<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let (_, this) = self;
        let AssignStmt { lhs, rhs } = this;
        match &lhs.1 {
            LValue::Ident(ident) => {
                let ptr = match compiler.scopes.find(ident.1).ok_or_else(|| {
                    CodeGenError::UnresolvedIdentifier {
                        loc: ident.0,
                        ident: ident.1.to_string(),
                    }
                })? {
                    Const(_) => {
                        return Err(CodeGenError::ConstantAsLeftValue {
                            loc: ident.0,
                            ident: ident.1.to_string(),
                        })
                    }
                    PrimMut { ptr, .. } => ptr,
                    ArrMut { .. } => {
                        return Err(CodeGenError::ArrayIsNotAssignable { loc: ident.0 })
                    }
                    ArrImmut { .. } => {
                        return Err(CodeGenError::ImmutableAsLeftValue {
                            loc: ident.0,
                            ident: ident.1.to_string(),
                        })
                    }
                };
                let val = rhs.codegen(compiler)?;
                compiler.builder.build_store(ptr, val);
                Ok(())
            }
            LValue::Idx(ident, idxs) => {
                let (ptr, _, pointee_ty) = match compiler.scopes.find(ident.1).ok_or_else(|| {
                    CodeGenError::UnresolvedIdentifier {
                        loc: ident.0,
                        ident: ident.1.to_string(),
                    }
                })? {
                    Const(_) => {
                        return Err(CodeGenError::ConstantAsLeftValue {
                            loc: ident.0,
                            ident: ident.1.to_string(),
                        })
                    }
                    PrimMut { pointee_ty, .. } => {
                        return Err(CodeGenError::TypeCannotBeIndexed {
                            loc: ident.0,
                            ty: pointee_ty.to_string(),
                        })
                    }
                    ArrMut {
                        ptr,
                        elem_ty,
                        pointee_ty,
                    } => (ptr, elem_ty, pointee_ty),
                    ArrImmut { .. } => {
                        return Err(CodeGenError::ImmutableAsLeftValue {
                            loc: ident.0,
                            ident: ident.1.to_string(),
                        })
                    }
                };
                let idxs = idxs.iter().try_fold(
                    vec![compiler.ctx.i32_type().const_zero()],
                    |mut acc, expr| match expr.codegen(compiler)? {
                        BasicValueEnum::IntValue(val) => {
                            acc.push(val);
                            Ok(acc)
                        }
                        val => Err(CodeGenError::ArrayIndexMustBeInteger {
                            loc: expr.0,
                            ty: val.get_type().to_string(),
                        }),
                    },
                )?;
                let elem = unsafe { compiler.builder.build_gep(pointee_ty, ptr, &idxs, "gep") };
                let rhs = rhs.codegen(compiler)?;
                compiler.builder.build_store(elem, rhs);
                Ok(())
            }
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<IfStmt<'ast>> {
    // 存在任意分支块不存在return
    type Out = (bool, BasicBlock<'ctx>);

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let func = compiler
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        match &self.1 {
            IfStmt::If(cond, then) => {
                let cond = match cond.codegen(compiler)? {
                    BasicValueEnum::IntValue(val) => {
                        if val.get_type().get_bit_width() == 1 {
                            val
                        } else {
                            return Err(CodeGenError::TypeMismatch {
                                expected: "\"i1\"".to_string(),
                                found: "\"i32\"".to_string(),
                                found_loc: cond.0,
                            });
                        }
                    }
                    val => {
                        return Err(CodeGenError::TypeMismatch {
                            expected: "\"i1\"".to_owned(),
                            found: val.get_type().to_string(),
                            found_loc: cond.0,
                        })
                    }
                };
                let then_block = compiler.ctx.append_basic_block(func, "then");
                let exit_block = compiler.ctx.append_basic_block(func, "exit");
                compiler
                    .builder
                    .build_conditional_branch(cond, then_block, exit_block);
                compiler.builder.position_at_end(then_block);
                then.codegen(&mut compiler.guard())?;
                if compiler.no_terminator() {
                    compiler.builder.build_unconditional_branch(exit_block);
                }
                Ok((true, exit_block))
            }
            IfStmt::IfElse(cond, then, else_) => {
                let cond = match cond.codegen(compiler)? {
                    BasicValueEnum::IntValue(val) => {
                        if val.get_type().get_bit_width() == 1 {
                            val
                        } else {
                            return Err(CodeGenError::TypeMismatch {
                                expected: "\"i1\"".to_string(),
                                found: "\"i32\"".to_string(),
                                found_loc: cond.0,
                            });
                        }
                    }
                    val => {
                        return Err(CodeGenError::TypeMismatch {
                            expected: "\"i1\"".to_string(),
                            found: val.get_type().to_string(),
                            found_loc: cond.0,
                        })
                    }
                };
                let func = compiler
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let then_block = compiler.ctx.append_basic_block(func, "then");
                let else_block = compiler.ctx.append_basic_block(func, "else");
                let exit_block = compiler.ctx.append_basic_block(func, "exit");
                let mut any_no_term = false;
                compiler
                    .builder
                    .build_conditional_branch(cond, then_block, else_block);
                compiler.builder.position_at_end(then_block);
                then.codegen(&mut compiler.guard())?;
                if compiler.no_terminator() {
                    compiler.builder.build_unconditional_branch(exit_block);
                    any_no_term = true;
                }
                compiler.builder.position_at_end(else_block);
                else_.codegen(&mut compiler.guard())?;
                if compiler.no_terminator() {
                    compiler.builder.build_unconditional_branch(exit_block);
                    any_no_term = true;
                }
                Ok((any_no_term, exit_block))
            }
            IfStmt::IfElseIf(cond, then, else_) => {
                let func = compiler
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let cond = match cond.codegen(compiler)? {
                    BasicValueEnum::IntValue(val) => {
                        if val.get_type().get_bit_width() == 1 {
                            val
                        } else {
                            return Err(CodeGenError::TypeMismatch {
                                expected: "\"i1\"".to_string(),
                                found: "\"i32\"".to_string(),
                                found_loc: cond.0,
                            });
                        }
                    }
                    val => {
                        return Err(CodeGenError::TypeMismatch {
                            expected: "\"i1\"".to_string(),
                            found: val.get_type().to_string(),
                            found_loc: cond.0,
                        })
                    }
                };
                let then_block = compiler.ctx.append_basic_block(func, "then");
                let else_block = compiler.ctx.append_basic_block(func, "else");
                compiler
                    .builder
                    .build_conditional_branch(cond, then_block, else_block);
                compiler.builder.position_at_end(else_block);
                let (mut any_no_term, exit_block) = else_.codegen(compiler)?;
                compiler.builder.position_at_end(then_block);
                then.codegen(&mut compiler.guard())?;
                if compiler.no_terminator() {
                    compiler.builder.build_unconditional_branch(exit_block);
                    any_no_term = true;
                }
                Ok((any_no_term, exit_block))
            }
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<WhileStmt<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let func = compiler
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let head = compiler.ctx.append_basic_block(func, "head");
        let body = compiler.ctx.append_basic_block(func, "loop");
        let after = compiler.ctx.append_basic_block(func, "after");
        let compiler = &mut compiler.guard_loop(head, after);
        compiler.builder.build_unconditional_branch(head);
        compiler.builder.position_at_end(head);
        let cond = match self.1.cond.codegen(compiler)? {
            BasicValueEnum::IntValue(val) => {
                if val.get_type().get_bit_width() == 1 {
                    val
                } else {
                    return Err(CodeGenError::TypeMismatch {
                        expected: "\"i1\"".to_owned(),
                        found: "\"i32\"".to_owned(),
                        found_loc: self.1.cond.0,
                    });
                }
            }
            val => {
                return Err(CodeGenError::TypeMismatch {
                    expected: "\"i1\"".to_owned(),
                    found: val.get_type().to_string(),
                    found_loc: self.1.cond.0,
                })
            }
        };
        compiler.builder.build_conditional_branch(cond, body, after);
        compiler.builder.position_at_end(body);
        self.1.body.codegen(&mut compiler.guard())?;
        if compiler.no_terminator() {
            compiler.builder.build_unconditional_branch(head);
        }
        compiler.builder.position_at_end(after);
        Ok(())
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<Expr<'ast>> {
    type Out = BasicValueEnum<'ctx>;

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let (_, this) = self;
        match this {
            Expr::Prim(expr) => expr.codegen(compiler),
            Expr::Unary(expr) => expr.codegen(compiler),
            Expr::Binary(expr) => expr.codegen(compiler),
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<PrimExpr<'ast>> {
    type Out = BasicValueEnum<'ctx>;

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        match &self.1 {
            PrimExpr::LValue(lval) => lval.codegen(compiler),
            PrimExpr::Literal(lit) => lit.codegen(compiler),
            PrimExpr::Paren(inner) => inner.codegen(compiler),
            PrimExpr::Call(name, args) => {
                let func = compiler.module.get_function(name.1).ok_or_else(|| {
                    CodeGenError::UnresolvedFunction {
                        loc: name.0,
                        ident: name.1.to_owned(),
                    }
                })?;
                let args = args.1.iter().try_fold(vec![], |mut acc, arg| {
                    let arg = arg.codegen(compiler)?;
                    acc.push(BasicMetadataValueEnum::from(arg));
                    Ok(acc)
                })?;
                Ok(compiler
                    .builder
                    .build_call(func, &args, "call")
                    .try_as_basic_value()
                    .left()
                    .unwrap_or_else(|| compiler.ctx.i32_type().const_zero().as_basic_value_enum()))
            }
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<LValue<'ast>> {
    type Out = BasicValueEnum<'ctx>;

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        match &self.1 {
            LValue::Ident((loc, ident)) => {
                let symbol = compiler.scopes.find(ident).ok_or_else(|| {
                    CodeGenError::UnresolvedIdentifier {
                        loc: loc.to_owned(),
                        ident: ident.to_string(),
                    }
                })?;
                match symbol {
                    Const(val) => Ok(val),
                    PrimMut { ptr, pointee_ty } => {
                        Ok(compiler.builder.build_load(pointee_ty, ptr, "load"))
                    }
                    ArrMut { ptr, .. } => Ok(ptr.as_basic_value_enum()),
                    ArrImmut { ptr, .. } => Ok(ptr.as_basic_value_enum()),
                }
            }
            LValue::Idx(ident, idxs) => {
                let symbol = compiler.scopes.find(ident.1).ok_or_else(|| {
                    CodeGenError::UnresolvedIdentifier {
                        loc: ident.0,
                        ident: ident.1.to_string(),
                    }
                })?;
                match symbol {
                    Const(_) => Err(CodeGenError::ConstantCannotBeIndexed {
                        loc: ident.0,
                        ident: ident.1.to_string(),
                    }),
                    PrimMut { pointee_ty, .. } => Err(CodeGenError::TypeCannotBeIndexed {
                        loc: ident.0,
                        ty: pointee_ty.to_string(),
                    }),
                    ArrMut {
                        ptr,
                        elem_ty,
                        pointee_ty,
                    }
                    | ArrImmut {
                        ptr,
                        elem_ty,
                        pointee_ty,
                    } => {
                        let idxs = idxs.iter().try_fold(
                            vec![compiler.ctx.i32_type().const_zero()],
                            |mut acc, expr| match expr.codegen(compiler)? {
                                BasicValueEnum::IntValue(val) => {
                                    acc.push(val);
                                    Ok(acc)
                                }
                                val => Err(CodeGenError::ArrayIndexMustBeInteger {
                                    loc: expr.0,
                                    ty: val.get_type().to_string(),
                                }),
                            },
                        )?;
                        let elem =
                            unsafe { compiler.builder.build_gep(pointee_ty, ptr, &idxs, "gep") };
                        Ok(compiler.builder.build_load(elem_ty, elem, "load"))
                    }
                }
            }
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<Literal> {
    type Out = BasicValueEnum<'ctx>;

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        match &self.1 {
            Literal::Bool(lit) => Ok(compiler
                .ctx
                .bool_type()
                .const_int(*lit as u64, false)
                .as_basic_value_enum()),
            Literal::Int(lit) => Ok(compiler
                .ctx
                .i32_type()
                .const_int(*lit as u64, false)
                .as_basic_value_enum()),
            Literal::Float(lit) => Ok(compiler
                .ctx
                .f32_type()
                .const_float(*lit as f64)
                .as_basic_value_enum()),
            Literal::List(_) => Err(CodeGenError::Unimplemented { loc: self.0 }),
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<Unary<'ast>> {
    type Out = BasicValueEnum<'ctx>;

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let (
            _,
            Unary {
                op: (op_loc, op),
                rhs,
            },
        ) = self;
        match op {
            UnaryOp::Pos => match rhs.codegen(compiler)? {
                BasicValueEnum::IntValue(val) => Ok(val.as_basic_value_enum()),
                BasicValueEnum::FloatValue(val) => Ok(val.as_basic_value_enum()),
                val => Err(CodeGenError::IllegalUnaryOperator {
                    loc: op_loc.to_owned(),
                    op: op.to_string(),
                    ty: val.get_type().to_string(),
                }),
            },
            UnaryOp::Neg => match rhs.codegen(compiler)? {
                BasicValueEnum::IntValue(val) => Ok(compiler
                    .builder
                    .build_int_neg(val, "int_neg")
                    .as_basic_value_enum()),
                BasicValueEnum::FloatValue(val) => Ok(compiler
                    .builder
                    .build_float_neg(val, "float_neg")
                    .as_basic_value_enum()),
                val => Err(CodeGenError::IllegalUnaryOperator {
                    loc: op_loc.to_owned(),
                    op: op.to_string(),
                    ty: val.get_type().to_string(),
                }),
            },
            UnaryOp::Not => match rhs.codegen(compiler)? {
                BasicValueEnum::IntValue(val) => {
                    if val.get_type().get_bit_width() == 1 {
                        Ok(compiler
                            .builder
                            .build_not(val, "bool_not")
                            .as_basic_value_enum())
                    } else {
                        Err(CodeGenError::IllegalUnaryOperator {
                            loc: op_loc.to_owned(),
                            op: op.to_string(),
                            ty: "\"i32\"".to_owned(),
                        })
                    }
                }
                val => Err(CodeGenError::IllegalUnaryOperator {
                    loc: op_loc.to_owned(),
                    op: op.to_string(),
                    ty: val.get_type().to_string(),
                }),
            },
            UnaryOp::BitNot => match rhs.codegen(compiler)? {
                BasicValueEnum::IntValue(val) => Ok(compiler
                    .builder
                    .build_not(val, "int_bnot")
                    .as_basic_value_enum()),
                val => Err(CodeGenError::IllegalUnaryOperator {
                    loc: op_loc.to_owned(),
                    op: op.to_string(),
                    ty: val.get_type().to_string(),
                }),
            },
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<Binary<'ast>> {
    type Out = BasicValueEnum<'ctx>;

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let (
            _,
            Binary {
                op: (op_loc, op),
                lhs,
                rhs,
            },
        ) = self;
        let lhs_loc = lhs.0;
        let rhs_loc = rhs.0;
        match (lhs.codegen(compiler)?, rhs.codegen(compiler)?) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => match op {
                ast::BinaryOp::LogicalOr => {
                    if lhs.get_type().get_bit_width() == 1 && rhs.get_type().get_bit_width() == 1 {
                        Ok(compiler
                            .builder
                            .build_or(lhs, rhs, "bool_or")
                            .as_basic_value_enum())
                    } else {
                        Err(CodeGenError::IllegalBinaryOperator {
                            loc_lhs: lhs_loc.to_owned(),
                            loc_rhs: rhs_loc.to_owned(),
                            loc_op: op_loc.to_owned(),
                            op: op.to_string(),
                            lhs: "\"i32\"".to_owned(),
                            rhs: "\"i32\"".to_owned(),
                        })
                    }
                }
                ast::BinaryOp::LogicalAnd => {
                    if lhs.get_type().get_bit_width() == 1 && rhs.get_type().get_bit_width() == 1 {
                        Ok(compiler
                            .builder
                            .build_and(lhs, rhs, "bool_and")
                            .as_basic_value_enum())
                    } else {
                        Err(CodeGenError::IllegalBinaryOperator {
                            loc_lhs: lhs_loc.to_owned(),
                            loc_rhs: rhs_loc.to_owned(),
                            loc_op: op_loc.to_owned(),
                            op: op.to_string(),
                            lhs: "\"i32\"".to_owned(),
                            rhs: "\"i32\"".to_owned(),
                        })
                    }
                }
                ast::BinaryOp::BitOr => Ok(compiler
                    .builder
                    .build_or(lhs, rhs, "int_or")
                    .as_basic_value_enum()),
                ast::BinaryOp::BitXor => Ok(compiler
                    .builder
                    .build_xor(lhs, rhs, "int_xor")
                    .as_basic_value_enum()),
                ast::BinaryOp::BitAnd => Ok(compiler
                    .builder
                    .build_and(lhs, rhs, "int_and")
                    .as_basic_value_enum()),
                ast::BinaryOp::Eq => Ok(compiler
                    .builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "int_eq")
                    .as_basic_value_enum()),
                ast::BinaryOp::Ne => Ok(compiler
                    .builder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "int_ne")
                    .as_basic_value_enum()),
                ast::BinaryOp::Lt => Ok(compiler
                    .builder
                    .build_int_compare(IntPredicate::SLT, lhs, rhs, "int_lt")
                    .as_basic_value_enum()),
                ast::BinaryOp::Gt => Ok(compiler
                    .builder
                    .build_int_compare(IntPredicate::SGT, lhs, rhs, "int_gt")
                    .as_basic_value_enum()),
                ast::BinaryOp::Le => Ok(compiler
                    .builder
                    .build_int_compare(IntPredicate::SLE, lhs, rhs, "int_le")
                    .as_basic_value_enum()),
                ast::BinaryOp::Ge => Ok(compiler
                    .builder
                    .build_int_compare(IntPredicate::SGE, lhs, rhs, "int_ge")
                    .as_basic_value_enum()),
                ast::BinaryOp::Shl => Ok(compiler
                    .builder
                    .build_left_shift(lhs, rhs, "int_shl")
                    .as_basic_value_enum()),
                ast::BinaryOp::Shr => Ok(compiler
                    .builder
                    .build_right_shift(lhs, rhs, false, "int_shr")
                    .as_basic_value_enum()),
                ast::BinaryOp::Add => Ok(compiler
                    .builder
                    .build_int_add(lhs, rhs, "int_add")
                    .as_basic_value_enum()),
                ast::BinaryOp::Sub => Ok(compiler
                    .builder
                    .build_int_sub(lhs, rhs, "int_sub")
                    .as_basic_value_enum()),
                ast::BinaryOp::Mul => Ok(compiler
                    .builder
                    .build_int_mul(lhs, rhs, "int_mul")
                    .as_basic_value_enum()),
                ast::BinaryOp::Div => Ok(compiler
                    .builder
                    .build_int_signed_div(lhs, rhs, "int_div")
                    .as_basic_value_enum()),
                ast::BinaryOp::Mod => Ok(compiler
                    .builder
                    .build_int_signed_rem(lhs, rhs, "int_mod")
                    .as_basic_value_enum()),
            },
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => match op {
                ast::BinaryOp::Eq => Ok(compiler
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "float_eq")
                    .as_basic_value_enum()),
                ast::BinaryOp::Ne => Ok(compiler
                    .builder
                    .build_float_compare(FloatPredicate::ONE, lhs, rhs, "float_ne")
                    .as_basic_value_enum()),
                ast::BinaryOp::Lt => Ok(compiler
                    .builder
                    .build_float_compare(FloatPredicate::OLT, lhs, rhs, "float_lt")
                    .as_basic_value_enum()),
                ast::BinaryOp::Gt => Ok(compiler
                    .builder
                    .build_float_compare(FloatPredicate::OGT, lhs, rhs, "float_gt")
                    .as_basic_value_enum()),
                ast::BinaryOp::Le => Ok(compiler
                    .builder
                    .build_float_compare(FloatPredicate::OLE, lhs, rhs, "float_le")
                    .as_basic_value_enum()),
                ast::BinaryOp::Ge => Ok(compiler
                    .builder
                    .build_float_compare(FloatPredicate::OGE, lhs, rhs, "float_ge")
                    .as_basic_value_enum()),
                ast::BinaryOp::Add => Ok(compiler
                    .builder
                    .build_float_add(lhs, rhs, "float_add")
                    .as_basic_value_enum()),
                ast::BinaryOp::Sub => Ok(compiler
                    .builder
                    .build_float_sub(lhs, rhs, "float_sub")
                    .as_basic_value_enum()),
                ast::BinaryOp::Mul => Ok(compiler
                    .builder
                    .build_float_mul(lhs, rhs, "float_mul")
                    .as_basic_value_enum()),
                ast::BinaryOp::Div => Ok(compiler
                    .builder
                    .build_float_div(lhs, rhs, "float_div")
                    .as_basic_value_enum()),
                ast::BinaryOp::Mod => Ok(compiler
                    .builder
                    .build_float_rem(lhs, rhs, "float_mod")
                    .as_basic_value_enum()),
                op => Err(CodeGenError::IllegalBinaryOperator {
                    loc_lhs: lhs_loc.to_owned(),
                    loc_rhs: rhs_loc.to_owned(),
                    loc_op: op_loc.to_owned(),
                    op: op.to_string(),
                    lhs: "\"f32\"".to_owned(),
                    rhs: "\"f32\"".to_owned(),
                }),
            },
            (lhs, rhs) => Err(CodeGenError::IllegalBinaryOperator {
                loc_lhs: lhs_loc.to_owned(),
                loc_rhs: rhs_loc.to_owned(),
                loc_op: op_loc.to_owned(),
                op: op.to_string(),
                lhs: lhs.get_type().to_string(),
                rhs: rhs.get_type().to_string(),
            }),
        }
    }
}
