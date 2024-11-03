use std::collections::HashMap;

use crate::{
    stdlib::init::stdlib,
    type_checker::types::Type,
    util::{failable::Failable, str::Str},
};

use super::{
    error::RuntimeError,
    value::{Function, FunctionVariation, Value},
};

/// The environment is a map of variable names to values.
/// The environment is used to store variables and functions.
#[derive(Debug, Clone)]
pub struct Environment<'a> {
    pub name: Str,
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function>, // Sort function values in a way that allows for fast lookup
    types: HashMap<String, Type>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new(name: Str) -> Self {
        Self {
            name,
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_child<'b: 'a>(&'a self, name: Str) -> Self {
        Self {
            name,
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            parent: Some(self),
        }
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&Value> {
        self.variables
            .get(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_variable(name)))
    }

    pub fn lookup_function(&self, name: &str) -> Option<&Function> {
        self.functions
            .get(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_function(name)))
    }

    pub fn add_local_function_variation(
        &mut self,
        name: &str,
        variation: FunctionVariation,
    ) -> Result<&Function, RuntimeError> {
        if let Some(existing) = self.functions.get_mut(name) {
            if existing.get_variations().contains(&&variation) {
                return Err(RuntimeError {
                    message: format!(
                        "Function variation of '{}' with the same signature already exists",
                        name
                    ),
                });
            }
            existing.add_variation(variation);
        } else {
            self.functions.insert(
                name.to_string(),
                Function::new(name.to_string(), vec![variation]),
            );
        }
        Ok(self.functions.get(name).unwrap())
    }

    /// Get a value from the environment.
    /// If the value is not found in the current environment, the parent environment is searched recursively.
    pub fn lookup_identifier(&self, name: &str) -> (Option<&Value>, Option<&Function>) {
        (self.lookup_variable(name), self.lookup_function(name))
    }

    pub fn get_type(&self, name: &str) -> Option<Type> {
        if let Some(t) = self.types.get(name) {
            Some(t.clone())
        } else if let Some(p) = &self.parent {
            p.get_type(name)
        } else {
            None
        }
    }

    /// Add a type to the environment.
    /// If the type already exists in a parent environment, it is shadowed.
    pub fn add_type(&mut self, name: Str, type_: Type) -> Failable<RuntimeError> {
        if self.types.contains_key(&name.to_string()) {
            panic!("Type {} already exists in the current environment", name);
        }
        self.types.insert(name.to_string(), type_);
        Ok(())
    }

    /// Add a variable (any value or function) to the environment.
    /// If the variable already exists in a parent environment, it is shadowed.
    pub fn add_value(&mut self, name: Str, value: Value) -> Failable<RuntimeError> {
        let name = name.to_string();
        // Check if the variable already exists in the standard library
        if self.variables.contains_key(&name) {
            Err(RuntimeError {
                message: format!(
                    "Variable {} already exists in the current environment",
                    name
                ),
            })
        } else {
            match value {
                Value::Function(f) => {
                    if self.functions.contains_key(&name) {
                        return Err(RuntimeError {
                            message: format!(
                                "Function {} already exists in the current environment",
                                name
                            ),
                        });
                    }
                    self.functions.insert(name, f);
                }
                _ => {
                    if self.variables.contains_key(&name) {
                        return Err(RuntimeError {
                            message: format!(
                                "Variable {} already exists in the current environment",
                                name
                            ),
                        });
                    }
                    self.variables.insert(name, value);
                }
            };
            Ok(())
        }
    }
}

pub fn global_env() -> Environment<'static> {
    let mut global_env = Environment::new(Str::from("global"));
    stdlib().init_environment(&mut global_env);
    global_env
}
