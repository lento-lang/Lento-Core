use std::collections::HashMap;

use crate::{type_checker::types::{Type, std_primitive_types}, util::str::Str};

use super::{value::{Value, Function}, error::RuntimeError};

/**
 * The environment is a map of variable names to values.
 * The environment is used to store variables and functions.
 */
#[derive(Debug, Clone)]
pub struct Environment {
    pub name: Str,
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function>, // Sort function values in a way that allows for fast lookup
    types: HashMap<String, Type>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(name: Str) -> Self {
        Self {
            name,
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Box<Environment>, name: Str) -> Self {
        Self {
            name,
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            parent: Some(parent),
        }
    }

    fn get_variable(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.variables.get(name) {
            Some(v.clone())
        } else if let Some(p) = &self.parent {
            p.get_variable(name)
        } else {
            None
        }
    }

    fn get_function(&self, name: &str) -> Option<Value> {
        if let Some(f) = self.functions.get(name) {
            Some(Value::Function(f.clone()))
        } else if let Some(p) = &self.parent {
            p.get_function(name)
        } else {
            None
        }
    }

    /**
     * Get a value from the environment.
     * If the value is not found in the current environment, the parent environment is searched recursively.
     */
    pub fn get_value(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.variables.get(name) {
            Some(v.clone())
        } else if let Some(f) = self.functions.get(name) {
            Some(Value::Function(f.clone()))
        } else if let Some(p) = &self.parent {
            p.get_value(name)
        } else {
            None
        }
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

    /**
     * Add a type to the environment.
     * If the type already exists in a parent environment, it is shadowed.
     */
    pub fn add_type(&mut self, name: Str, type_: Type) -> Failable<RuntimeError> {
        // Check if the type already exists in the standard library
        if std_primitive_types::find_type(name.to_string()).is_some() {
            panic!("Type {} already exists in the standard library", name);
        } else if self.types.get(&name.to_string()).is_some() {
            panic!("Type {} already exists in the current environment", name);
        } else {
            self.types.insert(name.to_string(), type_);
            Ok(())
        }

    }

    /**
     * Add a variable (any value or function) to the environment.
     * If the variable already exists in a parent environment, it is shadowed.
     */
    pub fn add_value(&mut self, name: Str, value: Value) -> Failable<RuntimeError> {
        let name = name.to_string();
        // Check if the variable already exists in the standard library
        if self.variables.get(&name).is_some() {
            panic!("Variable {} already exists in the current environment", name);
        } else {
            match value {
                Value::Function(f) => {
                    if self.functions.get(&name).is_some() {
                        panic!("Function {} already exists in the current environment", name);
                    } else {
                        self.functions.insert(name, f);
                    }
                },
                _ => {
                    if self.variables.get(&name).is_some() {
                        panic!("Variable {} already exists in the current environment", name);
                    } else {
                        self.variables.insert(name, value);
                    }
                }
            };
            Ok(())
        }
    }
}
