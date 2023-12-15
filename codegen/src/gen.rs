use crate::{
    compiler::{Compiler, ScopedGuard},
    error::{CodeGenError, Result},
    ty::{ndim_arr_of, TryIntoFuncType, TryIntoLLVMType},
    val::TryIntoLLVMValue,
};
use ast::{
    AssignStmt, Binary, Block, ConstDecl, Expr, FuncDef, FuncParam, FuncProto, GlobalVarDecl,
    IfStmt, Module, PrimExpr, Sourced, Stmt, Unary, UnaryOp, Unit, VarDecl, WhileStmt,
};
use inkwell::{
    types::BasicType,
    values::{BasicValue, BasicValueEnum, FunctionValue},
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
        let mut compiler = compiler.guard();
        let entry = compiler.ctx.append_basic_block(func, "entry");
        compiler.builder.position_at_end(entry);

        for (idx, (loc, param)) in proto.1.params.1.iter().enumerate() {
            let (ty_loc, ty) = &param.ty;
            let (ident_loc, ident) = &param.ident;
            let (llvm_ty, origin_ty) = match ty {
                ast::Type::Prim(prim) => {
                    let ty = prim.llvm_type(compiler.ctx)?.as_basic_type_enum();
                    (ty, ty)
                }
                ast::Type::Array(prim, dims) => {
                    let ty = prim.llvm_type(compiler.ctx)?;
                    let llvm_ty = ty.ptr_type(AddressSpace::default()).as_basic_type_enum();
                    let origin_ty = ndim_arr_of(ty, &dims.1).as_basic_type_enum();
                    (llvm_ty, origin_ty)
                }
            };

            let ptr = compiler.builder.build_alloca(llvm_ty, ident);
            let val = func.get_nth_param(idx as u32).unwrap();
            val.set_name(ident);
            compiler.builder.build_store(ptr, val);
            compiler
                .scopes
                .insert(ident, (ptr, llvm_ty, origin_ty))
                .map_err(|_| CodeGenError::DuplicateIdentifier {
                    loc: ident_loc.to_owned(),
                    ident: ident.to_string(),
                })?;
        }

        body.codegen(&mut compiler.guard())?;
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
            Stmt::If(stmt) => stmt.codegen(compiler),
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
            Stmt::Return(stmt) => todo!(),
            Stmt::Expr(stmt) => todo!(),
            Stmt::Block(block) => block.codegen(&mut compiler.guard()),
        }
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<VarDecl<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        let (_, this) = self;
        let VarDecl { ty, ident, init } = this;
        todo!()
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<AssignStmt<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        todo!()
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<IfStmt<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        todo!()
    }
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> for Sourced<WhileStmt<'ast>> {
    type Out = ();

    fn codegen(&'ast self, compiler: &mut Compiler<'ast, 'ctx>) -> Result<Self::Out> {
        todo!()
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
            PrimExpr::LValue(lval) => todo!(),
            PrimExpr::Literal(lit) => todo!(),
            PrimExpr::Paren(inner) => inner.codegen(compiler),
            PrimExpr::Call(_, _) => todo!(),
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
                            ty: "int".to_owned(),
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
                            lhs: "int".to_owned(),
                            rhs: "int".to_owned(),
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
                            lhs: "int".to_owned(),
                            rhs: "int".to_owned(),
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
                    lhs: "float".to_owned(),
                    rhs: "float".to_owned(),
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