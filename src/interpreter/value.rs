use std::{collections::HashMap, fmt::Display};

use crate::{
    parser::ast::Ast,
    type_checker::types::{
        std_primitive_types, FunctionParameterType, FunctionType, GetType, Type,
    },
};

use super::{interpreter::InterpretResult, number::Number};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordKey {
    String(String),
    Integer(String),
    Char(char),
}

impl Display for RecordKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecordKey::String(s) => write!(f, "{}", s),
            RecordKey::Integer(i) => write!(f, "{}", i),
            RecordKey::Char(c) => write!(f, "{}", c),
        }
    }
}

/// Is the value representation of `FunctionParameterType`.
#[derive(Debug, Clone, PartialEq)]
pub enum NativeFunctionParameters {
    Singles(Vec<Value>),
    Variadic(Vec<Value>, Vec<Value>), // Some initial values of different types, followed by the variadic type values
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionVariation {
    /// User-defined functions
    User {
        params: FunctionParameterType,
        body: Ast,
        ret: Type,
        signature: Type,
    },
    /// Built-in functions
    Native {
        handler: fn(NativeFunctionParameters) -> InterpretResult,
        params: FunctionParameterType,
        ret: Type,
        signature: Type,
    },
}

impl FunctionVariation {
    pub fn new_user(params: FunctionParameterType, body: Ast, ret: Type) -> Self {
        Self::User {
            signature: Type::Function(Box::new(FunctionType::new(params.clone(), ret.clone()))),
            params,
            body,
            ret,
        }
    }

    pub fn new_native(
        handler: fn(NativeFunctionParameters) -> InterpretResult,
        params: FunctionParameterType,
        ret: Type,
    ) -> Self {
        Self::Native {
            signature: Type::Function(Box::new(FunctionType::new(params.clone(), ret.clone()))),
            handler,
            params,
            ret,
        }
    }

    pub fn get_params(&self) -> &FunctionParameterType {
        match self {
            FunctionVariation::User { params, .. } => params,
            FunctionVariation::Native { params, .. } => params,
        }
    }

    pub fn get_return_type(&self) -> &Type {
        match self {
            FunctionVariation::User { ret, .. } => ret,
            FunctionVariation::Native { ret, .. } => ret,
        }
    }
}

impl GetType for FunctionVariation {
    fn get_type(&self) -> &Type {
        match self {
            FunctionVariation::User { signature, .. } => signature,
            FunctionVariation::Native { signature, .. } => signature,
        }
    }
}

impl Display for FunctionVariation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (params, ret_type) = match self {
            FunctionVariation::User { params, ret, .. } => (params, ret),
            FunctionVariation::Native { params, ret, .. } => (params, ret),
        };
        write!(f, "({}) -> {}", params, ret_type) // TODO: Make this a unicode arrow
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    name: String,
    singles: Vec<FunctionVariation>,
    variadics: Vec<FunctionVariation>,
    // TODO: Add an environment for each function variation
    signature: Type,
}

impl Function {
    pub fn new(name: String, variations: Vec<FunctionVariation>) -> Self {
        let signature = Function::signature_from(&variations);
        let (singles, variadics) = Self::split_variations(variations);
        Self {
            signature,
            name,
            singles,
            variadics,
        }
    }

    fn split_variations(
        variations: Vec<FunctionVariation>,
    ) -> (Vec<FunctionVariation>, Vec<FunctionVariation>) {
        let mut singles = vec![];
        let mut variadics = vec![];
        for v in variations {
            match v.get_params() {
                FunctionParameterType::Singles(_) => singles.push(v),
                FunctionParameterType::Variadic(_, _) => variadics.push(v),
            }
        }
        (singles, variadics)
    }

    /// A sum type of all the function variation signatures
    pub fn signature_from(variations: &[FunctionVariation]) -> Type {
        let mut variations = variations
            .iter()
            .map(|v| v.get_type().clone())
            .collect::<Vec<Type>>();
        variations.dedup();
        Type::Sum(variations)
    }

    /// Get the function variations with **singles first** and **then variadics**.
    pub fn get_variations(&self) -> Vec<&FunctionVariation> {
        let mut variations = self.singles.iter().collect::<Vec<&FunctionVariation>>();
        variations.extend(self.variadics.iter());
        variations
    }
}

impl GetType for Function {
    fn get_type(&self) -> &Type {
        &self.signature
    }
}

/// A Lento value is a value that can be stored in a variable, returned from a function, or passed as an argument.
/// These values are stored in the interpreter's memory during runtime and are garbage collected when they are no longer in use.
/// The interpreter takes AST nodes and evaluates them to produce a value.
/// Values can be referenced and used in other expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Number(Number),
    String(String),
    Char(char),
    Boolean(bool),
    Tuple(Vec<Value>, Type),
    List(Vec<Value>, Type),
    Record(HashMap<RecordKey, Value>, Type),
    Function(Function),
    Type(Type),
}

impl GetType for Value {
    fn get_type(&self) -> &Type {
        match self {
            Value::Unit => &std_primitive_types::UNIT,
            Value::Number(n) => n.get_type(),
            Value::String(_) => &std_primitive_types::STRING,
            Value::Char(_) => &std_primitive_types::CHAR,
            Value::Boolean(_) => &std_primitive_types::BOOL,
            Value::Tuple(_, t) => t,
            Value::List(_, t) => t,
            Value::Record(_, t) => t,
            Value::Function(f) => f.get_type(),
            Value::Type(_) => &std_primitive_types::TOP,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Number(n) => n.fmt(f),
            Value::String(s) => write!(f, "{}", s),
            Value::Char(c) => write!(f, "{}", c),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Tuple(t, _) => {
                write!(f, "(")?;
                for (i, v) in t.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i < t.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Value::List(l, _) => {
                write!(f, "[")?;
                for (i, v) in l.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i < l.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Record(r, _) => {
                write!(f, "{{ ")?;
                for (i, (k, v)) in r.iter().enumerate() {
                    write!(f, "{}: {}", k, v)?;
                    if i < r.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
            Value::Function(fun) => {
                writeln!(f, "function[{}] {{", fun.name)?;
                for v in fun.singles.iter() {
                    writeln!(f, "\t{}", v)?;
                }
                write!(f, "}}")
            }
            Value::Type(ty) => write!(f, "{}", ty),
        }
    }
}

impl Value {
    pub fn print_color(&self) -> String {
        use colorful::Colorful;

        match self {
            Value::Unit => format!("{}", "()".light_gray()),
            Value::Number(_) => format!("{}", format!("{}", self).yellow()),
            Value::String(s) => format!("{}", s.clone().light_green()),
            Value::Char(c) => format!("{}", c.to_string().light_green()),
            Value::Boolean(b) => format!("{}", b.to_string().magenta()),
            Value::Tuple(t, _) => {
                let mut result = "(".green().to_string();
                for (i, v) in t.iter().enumerate() {
                    result.push_str(&v.print_color());
                    if i < t.len() - 1 {
                        result.push_str(&", ".green().to_string());
                    }
                }
                result.push_str(&")".green().to_string());
                result
            }
            Value::List(l, _) => {
                let mut result = "[".green().to_string();
                for (i, v) in l.iter().enumerate() {
                    result.push_str(&v.print_color());
                    if i < l.len() - 1 {
                        result.push_str(&", ".green().to_string());
                    }
                }
                result.push_str(&"]".green().to_string());
                result
            }
            Value::Record(r, _) => {
                let mut result = "{ ".green().to_string();
                for (i, (k, v)) in r.iter().enumerate() {
                    result.push_str(&format!("{}: ", k));
                    result.push_str(&v.print_color());
                    if i < r.len() - 1 {
                        result.push_str(&", ".green().to_string());
                    }
                }
                result.push_str(&" }".green().to_string());
                result
            }
            Value::Function(fun) => {
                let mut result = format!("function[{}] {{\n", fun.name).green().to_string();
                for v in fun.singles.iter() {
                    result.push_str(&format!("\t{}\n", v));
                }
                result.push_str(&"}".green().to_string());
                result
            }
            Value::Type(ty) => format!("{}", ty).light_blue().to_string(),
        }
    }
}
