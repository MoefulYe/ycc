use std::collections::HashMap;

use inkwell::values::BasicValueEnum;

pub struct Scopes<'input, 'ctx>(Vec<HashMap<&'input str, BasicValueEnum<'ctx>>>);

impl<'input, 'ctx> Scopes<'input, 'ctx> {
    pub fn new() -> Self {
        Scopes(vec![])
    }

    pub fn enter(&mut self) {
        self.0.push(HashMap::new())
    }

    pub fn exit(&mut self) {
        self.0.pop().expect("unreachable");
    }

    pub fn find(&self, name: &'input str) -> Option<BasicValueEnum<'ctx>> {
        for scope in self.0.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn insert(&mut self, name: &'input str, value: BasicValueEnum<'ctx>) -> Result<(), ()> {
        let scope = self.0.last_mut().expect("unreachable");
        scope.insert(name, value).map(|_| ()).ok_or(())
    }
}
