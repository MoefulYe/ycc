use crate::error::{CodeGenError, Result};
use ast::Sourced;
use inkwell::{
    context::Context,
    types::{AnyType, ArrayType, BasicType, BasicTypeEnum},
};

pub trait TryIntoLLVMType {
    fn llvm_type<'ctx>(&self, ctx: &'ctx Context) -> Result<BasicTypeEnum<'ctx>>;
}

impl TryIntoLLVMType for Sourced<ast::Type> {
    fn llvm_type<'ctx>(&self, ctx: &'ctx Context) -> Result<BasicTypeEnum<'ctx>> {
        match &self.1 {
            ast::Type::Prim((loc, ty)) => match ty {
                ast::PrimType::Void => Err(CodeGenError::IllegalVoid { loc: *loc }),
                ast::PrimType::Bool => Ok(ctx.bool_type().as_basic_type_enum()),
                ast::PrimType::Int => Ok(ctx.i32_type().as_basic_type_enum()),
                ast::PrimType::Float => Ok(ctx.f32_type().as_basic_type_enum()),
            },
            ast::Type::Array((prim_loc, prim), (dims_loc, dims)) => {
                let prim = match prim {
                    ast::PrimType::Void => Err(CodeGenError::IllegalVoid { loc: *prim_loc })?,
                    ast::PrimType::Bool => ctx.bool_type().as_basic_type_enum(),
                    ast::PrimType::Int => ctx.i32_type().as_basic_type_enum(),
                    ast::PrimType::Float => ctx.f32_type().as_basic_type_enum(),
                };
                Ok(ndim_arr_of(prim, dims).as_basic_type_enum())
            }
        }
    }
}

fn ndim_arr_of<'ctx>(type_: impl BasicType<'ctx>, dims: &[i32]) -> ArrayType<'ctx> {
    let mut it = dims.iter().rev();
    let &size = it.next().unwrap();
    let init = type_.array_type(size as u32);
    it.fold(init, |acc, &size| acc.array_type(size as u32))
}
