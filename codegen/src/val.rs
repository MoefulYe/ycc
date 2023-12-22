use crate::error::{CodeGenError, Result};
use crate::ty::illegal;
use crate::ty::ndim_arr_of;
use ast::{Literal, Loc, PrimType, Sourced, Type};
use inkwell::context::Context;
use inkwell::types::{ArrayType, FloatType, IntType};
use inkwell::values::{ArrayValue, BasicValue, BasicValueEnum};

pub trait TryIntoLLVMValue {
    fn llvm_value<'ctx>(
        &self,
        ty: &Sourced<Type>,
        ctx: &'ctx Context,
    ) -> Result<BasicValueEnum<'ctx>>;
}

impl TryIntoLLVMValue for Sourced<Literal> {
    fn llvm_value<'ctx>(
        &self,
        (ty_loc, ty): &Sourced<Type>,
        ctx: &'ctx Context,
    ) -> Result<BasicValueEnum<'ctx>> {
        let (lit_loc, lit) = self;
        match ty {
            Type::Prim(prim) => match prim.1 {
                ast::PrimType::Void => Err(CodeGenError::IllegalVoid { loc: *ty_loc }),
                ast::PrimType::Bool => {
                    if let Literal::Bool(val) = lit {
                        Ok(ctx
                            .bool_type()
                            .const_int((*val).into(), false)
                            .as_basic_value_enum())
                    } else {
                        Err(CodeGenError::TypeMismatch {
                            expected: "bool".to_string(),
                            found: lit.type_str().to_string(),
                            found_loc: *lit_loc,
                        })
                    }
                }
                ast::PrimType::Int => {
                    if let Literal::Int(val) = lit {
                        Ok(ctx
                            .i32_type()
                            .const_int(*val as u64, false)
                            .as_basic_value_enum())
                    } else {
                        Err(CodeGenError::TypeMismatch {
                            expected: "int".to_string(),
                            found: lit.type_str().to_string(),
                            found_loc: *lit_loc,
                        })
                    }
                }
                ast::PrimType::Float => {
                    if let Literal::Float(val) = lit {
                        Ok(ctx
                            .f32_type()
                            .const_float(*val as f64)
                            .as_basic_value_enum())
                    } else {
                        Err(CodeGenError::TypeMismatch {
                            expected: "float".to_string(),
                            found: lit.type_str().to_string(),
                            found_loc: *lit_loc,
                        })
                    }
                }
            },
            Type::Array((prim_loc, prim), (dims_loc, dims)) => {
                if let Literal::List(arr) = lit {
                    if illegal(dims) {
                        return Err(CodeGenError::IllegalArraySize { loc: *dims_loc });
                    }
                    let (_, val) = match prim {
                        PrimType::Void => Err(CodeGenError::IllegalVoid { loc: *prim_loc }),
                        PrimType::Bool => handle_array_bool(arr, ctx.bool_type(), dims, *prim_loc),
                        PrimType::Int => handle_array_int(arr, ctx.i32_type(), dims, *prim_loc),
                        PrimType::Float => handle_array_float(arr, ctx.f32_type(), dims, *prim_loc),
                    }?;
                    Ok(val.as_basic_value_enum())
                } else {
                    Err(CodeGenError::TypeMismatch {
                        expected: ty.as_str().to_string(),
                        found: lit.type_str().to_string(),
                        found_loc: *lit_loc,
                    })
                }
            }
        }
    }
}

fn handle_array_int<'ctx>(
    arr: &[Sourced<Literal>],
    type_: IntType<'ctx>,
    dims: &[i32],
    prim_loc: Loc,
) -> Result<(ArrayType<'ctx>, ArrayValue<'ctx>)> {
    if dims.is_empty() {
        unreachable!()
    } else if dims.len() == 1 {
        let size = *dims.first().unwrap() as u32;
        let arr_ty = type_.array_type(size);
        if arr.len() > size as usize {
            return Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            });
        }
        let mut res = arr.iter().try_fold(vec![], |mut acc, (lit_loc, lit)| {
            if let Literal::Int(val) = lit {
                let val = type_.const_int(*val as u64, false);
                acc.push(val);
                Ok(acc)
            } else {
                Err(CodeGenError::TypeMismatch {
                    expected: "int".to_string(),
                    found: lit.type_str().to_string(),
                    found_loc: *lit_loc,
                })
            }
        })?;
        (arr.len()..size as usize).for_each(|_| res.push(type_.const_zero()));
        let arr_val = type_.const_array(&res);
        Ok((arr_ty, arr_val))
    } else {
        let size = *dims.first().unwrap() as u32;
        if arr.len() > size as usize {
            Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            })
        } else if arr.is_empty() {
            let ty = ndim_arr_of(type_, dims);
            let val = ty.const_zero();
            Ok((ty, val))
        } else {
            let (first_loc, first) = arr.first().unwrap();
            let (ty, first) = if let Literal::List(arr) = first {
                handle_array_int(arr, type_, &dims[1..], prim_loc)?
            } else {
                return Err(CodeGenError::TypeMismatch {
                    expected: "array".to_string(),
                    found: first.type_str().to_string(),
                    found_loc: *first_loc,
                });
            };
            let mut val = arr
                .iter()
                .skip(1)
                .try_fold(vec![first], |mut acc, (loc, item)| match item {
                    Literal::List(arr) => {
                        let (_, val) = handle_array_int(arr, type_, dims, prim_loc)?;
                        acc.push(val);
                        Ok(acc)
                    }
                    _ => Err(CodeGenError::TypeMismatch {
                        expected: "array".to_string(),
                        found: item.type_str().to_string(),
                        found_loc: *loc,
                    }),
                })?;
            (arr.len()..size as usize).for_each(|_| val.push(ty.const_zero()));
            let ty = ty.array_type(size);
            let val = ty.const_array(&val);
            Ok((ty, val))
        }
    }
}
//
fn handle_array_bool<'ctx>(
    arr: &[Sourced<Literal>],
    type_: IntType<'ctx>,
    dims: &[i32],
    prim_loc: Loc,
) -> Result<(ArrayType<'ctx>, ArrayValue<'ctx>)> {
    if dims.is_empty() {
        unreachable!()
    } else if dims.len() == 1 {
        let size = *dims.first().unwrap() as u32;
        let arr_ty = type_.array_type(size);
        if arr.len() > size as usize {
            return Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            });
        }
        let mut res = arr.iter().try_fold(vec![], |mut acc, (lit_loc, lit)| {
            if let Literal::Bool(val) = lit {
                let val = type_.const_int(*val as u64, false);
                acc.push(val);
                Ok(acc)
            } else {
                Err(CodeGenError::TypeMismatch {
                    expected: "bool".to_string(),
                    found: lit.type_str().to_string(),
                    found_loc: *lit_loc,
                })
            }
        })?;
        (arr.len()..size as usize).for_each(|_| res.push(type_.const_zero()));
        let arr_val = type_.const_array(&res);
        Ok((arr_ty, arr_val))
    } else {
        let size = *dims.first().unwrap() as u32;
        if arr.len() > size as usize {
            Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            })
        } else if arr.is_empty() {
            let ty = ndim_arr_of(type_, dims);
            let val = ty.const_zero();
            Ok((ty, val))
        } else {
            let (first_loc, first) = arr.first().unwrap();
            let (ty, first) = if let Literal::List(arr) = first {
                handle_array_bool(arr, type_, &dims[1..], prim_loc)?
            } else {
                return Err(CodeGenError::TypeMismatch {
                    expected: "array".to_string(),
                    found: first.type_str().to_string(),
                    found_loc: *first_loc,
                });
            };
            let mut val = arr
                .iter()
                .skip(1)
                .try_fold(vec![first], |mut acc, (loc, item)| match item {
                    Literal::List(arr) => {
                        let (_, val) = handle_array_bool(arr, type_, dims, prim_loc)?;
                        acc.push(val);
                        Ok(acc)
                    }
                    _ => Err(CodeGenError::TypeMismatch {
                        expected: "array".to_string(),
                        found: item.type_str().to_string(),
                        found_loc: *loc,
                    }),
                })?;
            (arr.len()..size as usize).for_each(|_| val.push(ty.const_zero()));
            let ty = ty.array_type(size);
            let val = ty.const_array(&val);
            Ok((ty, val))
        }
    }
}

fn handle_array_float<'ctx>(
    arr: &[Sourced<Literal>],
    type_: FloatType<'ctx>,
    dims: &[i32],
    prim_loc: Loc,
) -> Result<(ArrayType<'ctx>, ArrayValue<'ctx>)> {
    if dims.is_empty() {
        unreachable!()
    } else if dims.len() == 1 {
        let size = *dims.first().unwrap() as u32;
        let arr_ty = type_.array_type(size);
        if arr.len() > size as usize {
            return Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            });
        }
        let mut res = arr.iter().try_fold(vec![], |mut acc, (lit_loc, lit)| {
            if let Literal::Float(val) = lit {
                let val = type_.const_float(*val as f64);
                acc.push(val);
                Ok(acc)
            } else {
                Err(CodeGenError::TypeMismatch {
                    expected: "float".to_string(),
                    found: lit.type_str().to_string(),
                    found_loc: *lit_loc,
                })
            }
        })?;
        (arr.len()..size as usize).for_each(|_| res.push(type_.const_zero()));
        let arr_val = type_.const_array(&res);
        Ok((arr_ty, arr_val))
    } else {
        let size = *dims.first().unwrap() as u32;
        if arr.len() > size as usize {
            Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            })
        } else if arr.is_empty() {
            let ty = ndim_arr_of(type_, dims);
            let val = ty.const_zero();
            Ok((ty, val))
        } else {
            let (first_loc, first) = arr.first().unwrap();
            let (ty, first) = if let Literal::List(arr) = first {
                handle_array_float(arr, type_, &dims[1..], prim_loc)?
            } else {
                return Err(CodeGenError::TypeMismatch {
                    expected: "array".to_string(),
                    found: first.type_str().to_string(),
                    found_loc: *first_loc,
                });
            };
            let mut val = arr
                .iter()
                .skip(1)
                .try_fold(vec![first], |mut acc, (loc, item)| match item {
                    Literal::List(arr) => {
                        let (_, val) = handle_array_float(arr, type_, dims, prim_loc)?;
                        acc.push(val);
                        Ok(acc)
                    }
                    _ => Err(CodeGenError::TypeMismatch {
                        expected: "array".to_string(),
                        found: item.type_str().to_string(),
                        found_loc: *loc,
                    }),
                })?;
            (arr.len()..size as usize).for_each(|_| val.push(ty.const_zero()));
            let ty = ty.array_type(size);
            let val = ty.const_array(&val);
            Ok((ty, val))
        }
    }
}
