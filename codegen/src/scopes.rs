use std::collections::HashMap;

use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, PointerValue},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol<'ctx> {
    Const(BasicValueEnum<'ctx>),
    Var {
        addr: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
        origin: BasicTypeEnum<'ctx>,
    },
}

impl<'ctx> From<(PointerValue<'ctx>, BasicTypeEnum<'ctx>, BasicTypeEnum<'ctx>)> for Symbol<'ctx> {
    fn from(
        (addr, ty, origin): (PointerValue<'ctx>, BasicTypeEnum<'ctx>, BasicTypeEnum<'ctx>),
    ) -> Self {
        Self::Var { addr, ty, origin }
    }
}

impl<'ctx> From<BasicValueEnum<'ctx>> for Symbol<'ctx> {
    fn from(value: BasicValueEnum<'ctx>) -> Self {
        Self::Const(value)
    }
}

pub struct Scopes<'input, 'ctx>(Vec<HashMap<&'input str, Symbol<'ctx>>>);

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

    pub fn insert(&mut self, name: &'input str, value: Symbol<'ctx>) -> Result<(), ()> {
        let scope = self.0.last_mut().expect("unreachable");
        if let Some(_) = scope.insert(name, value) {
            Err(())
        } else {
            Ok(())
        }
    }
}
