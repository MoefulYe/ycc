use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, PointerValue},
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol<'ctx> {
    Const(BasicValueEnum<'ctx>),
    PrimMut {
        ptr: PointerValue<'ctx>,
        pointee_ty: BasicTypeEnum<'ctx>,
    },
    ArrMut {
        ptr: PointerValue<'ctx>,
        elem_ty: BasicTypeEnum<'ctx>,
        pointee_ty: BasicTypeEnum<'ctx>,
    },
    ArrImmut {
        ptr: PointerValue<'ctx>,
        elem_ty: BasicTypeEnum<'ctx>,
        pointee_ty: BasicTypeEnum<'ctx>,
    },
}

#[derive(Debug)]
pub struct Scopes<'ast, 'ctx>(Vec<HashMap<&'ast str, Symbol<'ctx>>>);

impl<'input, 'ctx> Scopes<'input, 'ctx> {
    pub fn new() -> Self {
        Scopes(vec![HashMap::new()])
    }

    pub fn enter(&mut self) {
        self.0.push(HashMap::new())
    }

    pub fn exit(&mut self) {
        self.0.pop().expect("unreachable");
    }

    pub fn find(&self, name: &'input str) -> Option<Symbol<'ctx>> {
        for scope in self.0.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.to_owned());
            }
        }
        None
    }

    pub fn insert(&mut self, name: &'input str, value: impl Into<Symbol<'ctx>>) -> Result<(), ()> {
        let scope = self.0.last_mut().expect("unreachable");
        if scope.insert(name, value.into()).is_some() {
            Err(())
        } else {
            Ok(())
        }
    }
}
