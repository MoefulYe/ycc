use crate::error::{CodeGenError, Result};
use ast::{FuncParam, FuncProto, Sourced};
use inkwell::{
    context::Context,
    types::{ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    AddressSpace,
};

pub trait TryIntoLLVMType {
    fn llvm_type<'ctx>(&self, ctx: &'ctx Context) -> Result<BasicTypeEnum<'ctx>>;
}

impl TryIntoLLVMType for Sourced<ast::Type> {
    fn llvm_type<'ctx>(&self, ctx: &'ctx Context) -> Result<BasicTypeEnum<'ctx>> {
        match &self.1 {
            ast::Type::Prim(ty) => ty.llvm_type(ctx),
            ast::Type::Array(prim, (dims_loc, dims)) => {
                if illegal(dims) {
                    return Err(CodeGenError::IllegalArraySize {
                        loc: dims_loc.to_owned(),
                    });
                }
                let prim = prim.llvm_type(ctx)?;
                Ok(ndim_arr_of(prim, dims).as_basic_type_enum())
            }
        }
    }
}

impl TryIntoLLVMType for Sourced<ast::PrimType> {
    fn llvm_type<'ctx>(&self, ctx: &'ctx Context) -> Result<BasicTypeEnum<'ctx>> {
        let (loc, this) = self;
        match this {
            ast::PrimType::Void => Err(CodeGenError::IllegalVoid {
                loc: loc.to_owned(),
            }),
            ast::PrimType::Bool => Ok(ctx.bool_type().as_basic_type_enum()),
            ast::PrimType::Int => Ok(ctx.i32_type().as_basic_type_enum()),
            ast::PrimType::Float => Ok(ctx.f32_type().as_basic_type_enum()),
        }
    }
}

pub trait GetElemType {
    fn elem_type<'ctx>(&self, ctx: &'ctx Context) -> Result<BasicTypeEnum<'ctx>>;
}

impl GetElemType for Sourced<ast::Type> {
    fn elem_type<'ctx>(&self, ctx: &'ctx Context) -> Result<BasicTypeEnum<'ctx>> {
        match &self.1 {
            ast::Type::Prim(_) => unreachable!(),
            ast::Type::Array(prim, _) => prim.llvm_type(ctx),
        }
    }
}

pub fn ndim_arr_of<'ctx>(ty: impl BasicType<'ctx>, dims: &[i32]) -> ArrayType<'ctx> {
    let mut it = dims.iter().rev();
    let &size = it.next().unwrap();
    let init = ty.array_type(size as u32);
    it.fold(init, |acc, &size| acc.array_type(size as u32))
}

pub trait TryIntoFuncType {
    fn fn_type<'ctx>(&self, ctx: &'ctx Context) -> Result<FunctionType<'ctx>>;
}

impl<'ast> TryIntoFuncType for Sourced<ast::FuncProto<'ast>> {
    fn fn_type<'ctx>(&self, ctx: &'ctx Context) -> Result<FunctionType<'ctx>> {
        let (_, this) = self;
        let FuncProto {
            ty: (_, ty),
            ident: _,
            params: (_, params),
        } = this;
        let params = params.iter().try_fold(
            vec![],
            |mut acc,
             (
                _,
                FuncParam {
                    ty: (_, ty),
                    ident: _,
                },
            )| {
                let ty = match ty {
                    ast::Type::Prim(prim) => BasicMetadataTypeEnum::from(prim.llvm_type(ctx)?),
                    ast::Type::Array(prim, (dims_loc, dims)) => {
                        if illegal(dims) {
                            return Err(CodeGenError::IllegalArraySize {
                                loc: dims_loc.to_owned(),
                            });
                        } else {
                            BasicMetadataTypeEnum::from(
                                prim.llvm_type(ctx)?.ptr_type(AddressSpace::default()),
                            )
                        }
                    }
                };
                acc.push(ty);
                Ok(acc)
            },
        )?;
        let fn_type = match ty {
            ast::PrimType::Void => ctx.void_type().fn_type(&params, false),
            ast::PrimType::Bool => ctx.bool_type().fn_type(&params, false),
            ast::PrimType::Int => ctx.i32_type().fn_type(&params, false),
            ast::PrimType::Float => ctx.f32_type().fn_type(&params, false),
        };
        Ok(fn_type)
    }
}

pub fn illegal(dims: &[i32]) -> bool {
    dims.iter().any(|&dim| dim <= 0)
}
