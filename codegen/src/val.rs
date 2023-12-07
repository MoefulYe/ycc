use std::ops::Deref;

use ast::{Literal, Loc, PrimType, Sourced, Type};
use inkwell::types::{AnyType, ArrayType, BasicType, BasicTypeEnum, IntType};
use inkwell::values::{ArrayValue, BasicValue, BasicValueEnum};

use crate::compiler::Compiler;
use crate::error::{CodeGenError, Result};

pub trait TryIntoLLVMValue {
    fn llvm_value<'a, 'ctx>(
        &self,
        ty: &Sourced<Type>,
        codegener: &'a Compiler<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>>
    where
        'a: 'ctx;
}

impl TryIntoLLVMValue for Sourced<Literal> {
    fn llvm_value<'a, 'ctx>(
        &self,
        ty: &Sourced<Type>,
        codegener: &'a Compiler<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        let (ty_loc, ty) = ty;
        let (lit_loc, lit) = self;
        match ty {
            Type::Prim(prim) => match prim.1 {
                ast::PrimType::Void => Err(CodeGenError::IllegalVoid { loc: *ty_loc }),
                ast::PrimType::Bool => {
                    if let Literal::Bool(val) = lit {
                        Ok(codegener
                            .bool_type()
                            .const_int((*val).into(), false)
                            .as_basic_value_enum())
                    } else {
                        Err(CodeGenError::TypeMismatch {
                            expected: "bool",
                            expected_loc: *ty_loc,
                            found: lit.type_str(),
                            found_loc: *lit_loc,
                        })
                    }
                }
                ast::PrimType::Int => {
                    if let Literal::Int(val) = lit {
                        Ok(codegener
                            .i32_type()
                            .const_int(*val as u64, false)
                            .as_basic_value_enum())
                    } else {
                        Err(CodeGenError::TypeMismatch {
                            expected: "int",
                            expected_loc: *ty_loc,
                            found: lit.type_str(),
                            found_loc: *lit_loc,
                        })
                    }
                }
                ast::PrimType::Float => {
                    if let Literal::Float(val) = lit {
                        Ok(codegener
                            .f32_type()
                            .const_float(*val as f64)
                            .as_basic_value_enum())
                    } else {
                        Err(CodeGenError::TypeMismatch {
                            expected: "float",
                            expected_loc: *ty_loc,
                            found: lit.type_str(),
                            found_loc: *lit_loc,
                        })
                    }
                }
            },
            Type::Array((prim_loc, prim), (dims_loc, dims)) => {
                if let Literal::List(arr) = lit {
                    todo!()
                } else {
                    Err(CodeGenError::TypeMismatch {
                        expected: ty.as_str(),
                        expected_loc: *ty_loc,
                        found: lit.type_str(),
                        found_loc: *lit_loc,
                    })
                }
            }
        }
    }
}

fn handle_array_int<'ctx>(
    arr: &[Sourced<Literal>],
    prim_loc: Loc,
    dims_loc: Loc,
    type_: impl BasicType<'ctx>,
    dims: &[i32],
) -> Result<BasicValueEnum<'ctx>> {
    if dims.iter().any(|&size| size <= 0) {
        return Err(CodeGenError::IllegalArraySize { loc: dims_loc });
    }
    todo!()
}

fn handle_array_int_<'ctx>(
    arr: &[Sourced<Literal>],
    type_: IntType<'ctx>,
    dims: &[i32],
    prim_loc: Loc,
) -> Result<(ArrayType<'ctx>, ArrayValue<'ctx>)> {
    if dims.is_empty() {
        unreachable!()
    } else if dims.len() == 1 {
        let size = *dims.last().unwrap() as u32;
        let arr_ty = type_.array_type(size);
        let mut res = arr
            .iter()
            .fold(Ok(vec![]), |acc, (lit_loc, lit)| match acc {
                Ok(mut ok) => {
                    if let Literal::Int(val) = lit {
                        let val = type_.const_int(*val as u64, false);
                        ok.push(val);
                        Ok(ok)
                    } else {
                        Err(CodeGenError::TypeMismatch {
                            expected: "int",
                            expected_loc: prim_loc,
                            found: lit.type_str(),
                            found_loc: *lit_loc,
                        })
                    }
                }
                Err(err) => Err(err),
            })?;
        let res_len = res.len();
        if res_len < size as usize {
            (res_len..size as usize).for_each(|_| res.push(type_.const_zero()))
        } else if res_len > size as usize {
            return Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            });
        }
        let arr_val = type_.const_array(&res);
        Ok((arr_ty, arr_val))
    } else {
        let mut res = arr.iter()
    }
}

//构造n维数组类型
fn get_array<'ctx>(dims: &[i32], type_: impl BasicType<'ctx>) -> Result<ArrayType<'ctx>, ()> {
    let mut it = dims.iter().rev();
    let &size = it.next().unwrap();
    let init = if size <= 0 {
        Err(())
    } else {
        Ok(type_.array_type(size as u32))
    };
    it.fold(init, |acc, &size| match acc {
        Ok(ok) => {
            if size <= 0 {
                Err(())
            } else {
                Ok(ok.array_type(size as u32))
            }
        }
        Err(err) => Err(err),
    })
}
