use std::fmt::Display;

use crate::type_checker::{
    checked_ast::CheckedAst,
    types::{std_primitive_types, FunctionParameterType, GetType, Type, VariationType},
};

use super::{environment::Environment, interpreter::InterpretResult, number::Number};

/// A key in a record can be a string, integer, float, or character.
/// This is used to represent the key in the AST.
///
/// ## Example
/// ```ignore
/// record = { "key": 1, 2: 3.0, 'c': "value", 4.0: 'd' }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum RecordKey {
    String(String),
    Number(Number),
    Char(char),
}

impl Display for RecordKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecordKey::String(s) => write!(f, "{}", s),
            RecordKey::Number(n) => write!(f, "{}", n),
            RecordKey::Char(c) => write!(f, "{}", c),
        }
    }
}

/// Is the value representation of `FunctionParameterType`.
#[derive(Debug, Clone)]
pub enum NativeFunctionParameters {
    Singles(Vec<Value>),
    Variadic(Vec<Value>, Vec<Value>), // Some initial values of different types, followed by the variadic type values
}

#[derive(Debug, Clone)]
pub struct UserFunctionVariation {
    pub params: FunctionParameterType,
    pub body: CheckedAst,
    pub closure: Environment<'static>,
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub enum FunctionVariation {
    /// User-defined functions
    User(UserFunctionVariation),
    /// Built-in functions
    Native {
        handler: fn(NativeFunctionParameters) -> InterpretResult,
        params: FunctionParameterType,
        ret: Type,
    },
}

impl FunctionVariation {
    pub fn new_user(
        params: FunctionParameterType,
        body: CheckedAst,
        closure: Environment<'static>,
        ret: Type,
    ) -> Self {
        Self::User(UserFunctionVariation {
            params,
            body,
            closure,
            ret,
        })
    }

    pub fn new_native(
        handler: fn(NativeFunctionParameters) -> InterpretResult,
        params: FunctionParameterType,
        ret: Type,
    ) -> Self {
        Self::Native {
            handler,
            params,
            ret,
        }
    }

    pub fn get_params(&self) -> &FunctionParameterType {
        match self {
            FunctionVariation::User(UserFunctionVariation { params, .. }) => params,
            FunctionVariation::Native { params, .. } => params,
        }
    }

    pub fn get_return_type(&self) -> &Type {
        match self {
            FunctionVariation::User(UserFunctionVariation { ret, .. }) => ret,
            FunctionVariation::Native { ret, .. } => ret,
        }
    }

    pub fn get_type(&self) -> VariationType {
        match self {
            FunctionVariation::User(UserFunctionVariation { params, ret, .. }) => {
                VariationType::new(params.clone(), ret.clone())
            }
            FunctionVariation::Native { params, ret, .. } => {
                VariationType::new(params.clone(), ret.clone())
            }
        }
    }

    pub fn pretty_print_color(&self) -> String {
        use colorful::Colorful;

        let (params, ret) = match self {
            FunctionVariation::User(UserFunctionVariation { params, ret, .. }) => (params, ret),
            FunctionVariation::Native { params, ret, .. } => (params, ret),
        };

        format!(
            "{} {} {}",
            params.pretty_print_color(),
            "->".dark_gray(),
            ret.pretty_print_color()
        )
    }
}

impl Display for FunctionVariation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (params, ret_type) = match self {
            FunctionVariation::User(UserFunctionVariation { params, ret, .. }) => (params, ret),
            FunctionVariation::Native { params, ret, .. } => (params, ret),
        };
        write!(f, "{} -> {}", params, ret_type) // TODO: Make this a unicode arrow
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    // name: String,
    singles: Vec<FunctionVariation>,
    variadics: Vec<FunctionVariation>,
    // TODO: Add an environment for each function variation
    signature: Type,
}

impl Function {
    pub fn new(variations: Vec<FunctionVariation>) -> Self {
        let signature = Function::signature_from(&variations);
        let (singles, variadics) = Self::split_variations(variations);
        Self {
            signature,
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

    // pub fn get_name(&self) -> &str {
    //     &self.name
    // }

    /// A sum type of all the function variation signatures
    pub fn signature_from(variations: &[FunctionVariation]) -> Type {
        Type::Function(
            variations
                .iter()
                .map(|v| v.get_type().clone())
                .collect::<Vec<_>>(),
        )
    }

    /// Get the function variations with **singles first** and **then variadics**.
    pub fn get_variations(&self) -> Vec<&FunctionVariation> {
        let mut variations = self.singles.iter().collect::<Vec<&FunctionVariation>>();
        variations.extend(self.variadics.iter());
        variations
    }

    pub fn add_variation(&mut self, variation: FunctionVariation) {
        match variation.get_params() {
            FunctionParameterType::Singles(_) => self.singles.push(variation),
            FunctionParameterType::Variadic(_, _) => self.variadics.push(variation),
        }
    }

    pub fn get_variation(&self, variation: &VariationType) -> Option<&FunctionVariation> {
        if let Some(v) = self.singles.iter().find(|&s| s.get_type() == *variation) {
            return Some(v);
        }
        self.variadics.iter().find(|&v| v.get_type() == *variation)
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
#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Number(Number),
    String(String),
    Char(char),
    Boolean(bool),
    Tuple(Vec<Value>, Type),
    List(Vec<Value>, Type),
    Record(Vec<(RecordKey, Value)>, Type),
    Variation(Box<FunctionVariation>),
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
            Value::Variation(f) => f.get_return_type(), //.get_type(),
            Value::Function(f) => f.get_type(),
            Value::Type(_) => &std_primitive_types::TYPE,
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
                writeln!(f, "fn {{")?;
                for v in fun.singles.iter() {
                    writeln!(f, "\t{}", v)?;
                }
                write!(f, "}}")
            }
            Value::Variation(var) => write!(f, "{}", var),
            Value::Type(ty) => write!(f, "{}", ty),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Tuple(l0, l1), Self::Tuple(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::List(l0, l1), Self::List(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Record(l0, l1), Self::Record(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Variation(l0), Self::Variation(r0)) => l0.get_type() == r0.get_type(),
            (Self::Function(l0), Self::Function(r0)) => l0.get_type() == r0.get_type(),
            (Self::Type(l0), Self::Type(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Value {
    pub fn pretty_print(&self) -> String {
        match self {
            Value::Unit => "()".to_string(),
            Value::Number(n) => n.to_string(),
            Value::String(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("'{}'", c),
            Value::Boolean(b) => b.to_string(),
            Value::Tuple(t, _) => {
                let mut result = "(".to_string();
                for (i, v) in t.iter().enumerate() {
                    result.push_str(&v.pretty_print());
                    if i < t.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(')');
                result
            }
            Value::List(l, _) => {
                let mut result = "[".to_string();
                for (i, v) in l.iter().enumerate() {
                    result.push_str(&v.pretty_print());
                    if i < l.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(']');
                result
            }
            Value::Record(r, _) => {
                let mut result = "{ ".to_string();
                for (i, (k, v)) in r.iter().enumerate() {
                    result.push_str(&format!("{}: {}", k, v.pretty_print()));
                    if i < r.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(" }");
                result
            }
            Value::Function(fun) => {
                let mut result = "fn {\n".to_string();
                for v in fun.singles.iter() {
                    result.push_str(&format!("\t{}\n", v));
                }
                result.push('}');
                result
            }
            Value::Variation(var) => var.to_string(),
            Value::Type(ty) => ty.to_string(),
        }
    }

    pub fn pretty_print_color(&self) -> String {
        use colorful::Colorful;

        match self {
            Value::Unit => format!("{}", "()".light_gray()),
            Value::Number(_) => format!("{}", self).yellow().to_string(),
            Value::String(s) => format!("\"{}\"", s).light_yellow().to_string(),
            Value::Char(c) => format!("'{}'", c).light_green().to_string(),
            Value::Boolean(b) => format!("{}", b.to_string().magenta()),
            Value::Tuple(t, _) => {
                let mut result = "(".light_gray().to_string();
                for (i, v) in t.iter().enumerate() {
                    result.push_str(&v.pretty_print_color());
                    if i < t.len() - 1 {
                        result.push_str(&", ".light_gray().to_string());
                    }
                }
                result.push_str(&")".light_gray().to_string());
                result
            }
            Value::List(l, _) => {
                let mut result = "[".light_gray().to_string();
                for (i, v) in l.iter().enumerate() {
                    result.push_str(&v.pretty_print_color());
                    if i < l.len() - 1 {
                        result.push_str(&", ".light_gray().to_string());
                    }
                }
                result.push_str(&"]".light_gray().to_string());
                result
            }
            Value::Record(r, _) => {
                let mut result = "{ ".light_gray().to_string();
                for (i, (k, v)) in r.iter().enumerate() {
                    result.push_str(&format!("{}: ", k));
                    result.push_str(&v.pretty_print_color());
                    if i < r.len() - 1 {
                        result.push_str(&", ".light_gray().to_string());
                    }
                }
                result.push_str(&" }".light_gray().to_string());
                result
            }
            Value::Function(fun) => {
                let mut result = "fn {\n".dark_gray().to_string();
                for v in fun.singles.iter() {
                    result.push_str(&format!("    {}\n", v.pretty_print_color()));
                }
                result.push_str(&"}".dark_gray().to_string());
                result
            }
            Value::Variation(var) => var.pretty_print_color(),
            Value::Type(ty) => format!("{}", ty).light_blue().to_string(),
        }
    }
}
