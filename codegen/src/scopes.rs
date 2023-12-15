use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue},
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol<'ctx> {
    Const(BasicValueEnum<'ctx>),
    Var {
        addr: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
        origin: BasicTypeEnum<'ctx>,
    },
}

impl<'ctx, T: Into<BasicTypeEnum<'ctx>>, U: Into<BasicTypeEnum<'ctx>>>
    From<(PointerValue<'ctx>, T, U)> for Symbol<'ctx>
{
    fn from((addr, ty, origin): (PointerValue<'ctx>, T, U)) -> Self {
        Self::Var {
            addr,
            ty: ty.into(),
            origin: origin.into(),
        }
    }
}

impl<'ctx> From<BasicValueEnum<'ctx>> for Symbol<'ctx> {
    fn from(value: BasicValueEnum<'ctx>) -> Self {
        Self::Const(value)
    }
}

impl<'ctx> From<IntValue<'ctx>> for Symbol<'ctx> {
    fn from(value: IntValue<'ctx>) -> Self {
        Self::Const(value.as_basic_value_enum().into())
    }
}

impl<'ctx> From<FloatValue<'ctx>> for Symbol<'ctx> {
    fn from(value: FloatValue<'ctx>) -> Self {
        Self::Const(value.as_basic_value_enum().into())
    }
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
        if let Some(_) = scope.insert(name, value.into()) {
            Err(())
        } else {
            Ok(())
        }
    }
}
