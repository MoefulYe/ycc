use crate::error::{CodeGenError, Result};
use crate::ty::illegal;
use crate::ty::ndim_arr_of;
use ast::{Literal, Loc, PrimType, Sourced, Type};
use inkwell::context::Context;
use inkwell::types::{FloatType, IntType};
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
                            expected: "\"i1\"".to_string(),
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
                            expected: "\"i32\"".to_string(),
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
                            expected: "\"f32\"".to_string(),
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
                    let val = match prim {
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
    ty: IntType<'ctx>,
    dims: &[i32],
    prim_loc: Loc,
) -> Result<ArrayValue<'ctx>> {
    if dims.is_empty() {
        unreachable!()
    } else if dims.len() == 1 {
        let size = *dims.first().unwrap() as u32;
        if arr.len() > size as usize {
            return Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            });
        }
        let mut res = arr.iter().try_fold(vec![], |mut acc, (lit_loc, lit)| {
            if let Literal::Int(val) = lit {
                let val = ty.const_int(*val as u64, false);
                acc.push(val);
                Ok(acc)
            } else {
                Err(CodeGenError::TypeMismatch {
                    expected: "\"i32\"".to_string(),
                    found: lit.type_str().to_string(),
                    found_loc: *lit_loc,
                })
            }
        })?;
        res.extend((arr.len()..size as usize).map(|_| ty.const_zero()));
        Ok(ty.const_array(&res))
    } else {
        let size = *dims.first().unwrap() as u32;
        if arr.len() > size as usize {
            Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            })
        } else if arr.is_empty() {
            Ok(ndim_arr_of(ty, dims).const_zero())
        } else {
            let first = arr.first().unwrap();
            let first_elem = if let Literal::List(ref arr) = first.1 {
                handle_array_int(arr, ty, &dims[1..], prim_loc)?
            } else {
                return Err(CodeGenError::TypeMismatch {
                    expected: "\"arr\"".to_string(),
                    found: first.1.type_str().to_string(),
                    found_loc: first.0,
                });
            };
            let mut arr = arr.iter().skip(1).try_fold(
                vec![first_elem],
                |mut acc, (loc, elem)| match elem {
                    Literal::List(elem) => {
                        let val = handle_array_int(elem, ty, &dims[1..], prim_loc)?;
                        acc.push(val);
                        Ok(acc)
                    }
                    _ => Err(CodeGenError::TypeMismatch {
                        expected: "\"arr\"".to_string(),
                        found: elem.type_str().to_string(),
                        found_loc: loc.to_owned(),
                    }),
                },
            )?;
            arr.extend((arr.len()..size as usize).map(|_| first_elem.get_type().const_zero()));
            Ok(first_elem.get_type().const_array(&arr))
        }
    }
}

fn handle_array_bool<'ctx>(
    arr: &[Sourced<Literal>],
    ty: IntType<'ctx>,
    dims: &[i32],
    prim_loc: Loc,
) -> Result<ArrayValue<'ctx>> {
    if dims.is_empty() {
        unreachable!()
    } else if dims.len() == 1 {
        let size = *dims.first().unwrap() as u32;
        if arr.len() > size as usize {
            return Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            });
        }
        let mut res = arr.iter().try_fold(vec![], |mut acc, (lit_loc, lit)| {
            if let Literal::Bool(val) = lit {
                let val = ty.const_int(*val as u64, false);
                acc.push(val);
                Ok(acc)
            } else {
                Err(CodeGenError::TypeMismatch {
                    expected: "\"i1\"".to_string(),
                    found: lit.type_str().to_string(),
                    found_loc: *lit_loc,
                })
            }
        })?;
        res.extend((arr.len()..size as usize).map(|_| ty.const_zero()));
        Ok(ty.const_array(&res))
    } else {
        let size = *dims.first().unwrap() as u32;
        if arr.len() > size as usize {
            Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            })
        } else if arr.is_empty() {
            Ok(ndim_arr_of(ty, dims).const_zero())
        } else {
            let first = arr.first().unwrap();
            let first_elem = if let Literal::List(ref arr) = first.1 {
                handle_array_bool(arr, ty, &dims[1..], prim_loc)?
            } else {
                return Err(CodeGenError::TypeMismatch {
                    expected: "\"arr\"".to_string(),
                    found: first.1.type_str().to_string(),
                    found_loc: first.0,
                });
            };
            let mut arr = arr.iter().skip(1).try_fold(
                vec![first_elem],
                |mut acc, (loc, elem)| match elem {
                    Literal::List(elem) => {
                        let val = handle_array_bool(elem, ty, &dims[1..], prim_loc)?;
                        acc.push(val);
                        Ok(acc)
                    }
                    _ => Err(CodeGenError::TypeMismatch {
                        expected: "\"arr\"".to_string(),
                        found: elem.type_str().to_string(),
                        found_loc: loc.to_owned(),
                    }),
                },
            )?;
            arr.extend((arr.len()..size as usize).map(|_| first_elem.get_type().const_zero()));
            Ok(first_elem.get_type().const_array(&arr))
        }
    }
}

fn handle_array_float<'ctx>(
    arr: &[Sourced<Literal>],
    ty: FloatType<'ctx>,
    dims: &[i32],
    prim_loc: Loc,
) -> Result<ArrayValue<'ctx>> {
    if dims.is_empty() {
        unreachable!()
    } else if dims.len() == 1 {
        let size = *dims.first().unwrap() as u32;
        if arr.len() > size as usize {
            return Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            });
        }
        let mut res = arr.iter().try_fold(vec![], |mut acc, (lit_loc, lit)| {
            if let Literal::Float(val) = lit {
                let val = ty.const_float(*val as f64);
                acc.push(val);
                Ok(acc)
            } else {
                Err(CodeGenError::TypeMismatch {
                    expected: "\"f32\"".to_string(),
                    found: lit.type_str().to_string(),
                    found_loc: *lit_loc,
                })
            }
        })?;
        res.extend((arr.len()..size as usize).map(|_| ty.const_zero()));
        Ok(ty.const_array(&res))
    } else {
        let size = *dims.first().unwrap() as u32;
        if arr.len() > size as usize {
            Err(CodeGenError::ExcessElementsInArrayInitializer {
                loc: arr[size as usize].0,
            })
        } else if arr.is_empty() {
            Ok(ndim_arr_of(ty, dims).const_zero())
        } else {
            let first = arr.first().unwrap();
            let first_elem = if let Literal::List(ref arr) = first.1 {
                handle_array_float(arr, ty, &dims[1..], prim_loc)?
            } else {
                return Err(CodeGenError::TypeMismatch {
                    expected: "\"arr\"".to_string(),
                    found: first.1.type_str().to_string(),
                    found_loc: first.0,
                });
            };
            let mut arr = arr.iter().skip(1).try_fold(
                vec![first_elem],
                |mut acc, (loc, elem)| match elem {
                    Literal::List(elem) => {
                        let val = handle_array_float(elem, ty, &dims[1..], prim_loc)?;
                        acc.push(val);
                        Ok(acc)
                    }
                    _ => Err(CodeGenError::TypeMismatch {
                        expected: "\"arr\"".to_string(),
                        found: elem.type_str().to_string(),
                        found_loc: loc.to_owned(),
                    }),
                },
            )?;
            arr.extend((arr.len()..size as usize).map(|_| first_elem.get_type().const_zero()));
            Ok(first_elem.get_type().const_array(&arr))
        }
    }
}
