use std::{collections::HashMap, fmt::Display};

use num_bigfloat::BigFloat;
use num_bigint::{BigInt, BigUint};

use crate::{type_checker::types::{Type, FunctionParameterType, std_primitive_types, GetType}, parser::ast::Ast};

use super::interpreter::InterpretResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnsignedInteger {
    UInt1(u8), // Bit
    UInt8(u8), // Byte
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UInt128(u128),
    UIntVar(BigUint), // Arbitrary sized integers (variable size)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignedInteger {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    IntVar(BigInt), // Arbitrary sized integers (variable size)
}

#[derive(Debug, Clone, PartialEq)]
pub enum FloatingPoint {
    Float32(f32),
    Float64(f64),
    FloatBig(BigFloat), // Increased precision floating point numbers (fixed but large size)
    // Look at https://github.com/stencillogic/astro-float for handling Arbitrary precision floating point numbers (variable size)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    UnsignedInteger(UnsignedInteger),
    SignedInteger(SignedInteger),
    FloatingPoint(FloatingPoint),
}

impl GetType for Number {
    fn get_type(&self) -> Type {
        match self {
            Number::UnsignedInteger(u) => match u {
                UnsignedInteger::UInt1(_) => std_primitive_types::UINT1,
                UnsignedInteger::UInt8(_) => std_primitive_types::UINT8,
                UnsignedInteger::UInt16(_) => std_primitive_types::UINT16,
                UnsignedInteger::UInt32(_) => std_primitive_types::UINT32,
                UnsignedInteger::UInt64(_) => std_primitive_types::UINT64,
                UnsignedInteger::UInt128(_) => std_primitive_types::UINT128,
                UnsignedInteger::UIntVar(_) => std_primitive_types::UINTBIG
            },
            Number::SignedInteger(i) => match i {
                SignedInteger::Int8(_) => std_primitive_types::INT8,
                SignedInteger::Int16(_) => std_primitive_types::INT16,
                SignedInteger::Int32(_) => std_primitive_types::INT32,
                SignedInteger::Int64(_) => std_primitive_types::INT64,
                SignedInteger::Int128(_) => std_primitive_types::INT128,
                SignedInteger::IntVar(_) => std_primitive_types::INTBIG
            },
            Number::FloatingPoint(f) => match f {
                FloatingPoint::Float32(_) => std_primitive_types::FLOAT32,
                FloatingPoint::Float64(_) => std_primitive_types::FLOAT64,
                FloatingPoint::FloatBig(_) => std_primitive_types::FLOATBIG
            },
        }
    }
}

impl Number {
    fn parse_big_int(s: String) -> Number {
        let i = BigInt::parse_bytes(s.as_bytes(), 10).unwrap();
        Number::SignedInteger(SignedInteger::IntVar(i))
    }

    fn parse_big_uint(s: String) -> Number {
        let u = BigUint::parse_bytes(s.as_bytes(), 10).unwrap();
        Number::UnsignedInteger(UnsignedInteger::UIntVar(u))
    }

    fn parse_big_float(s: String) -> Number {
        let f = BigFloat::parse(&s).unwrap();
        Number::FloatingPoint(FloatingPoint::FloatBig(f))
    }

    pub fn parse(s: String) -> Number {
        if s.contains('.') { // Floating point number (Only signed floating point numbers are supported)
            let f = s.parse::<f64>();
            if f.is_err() { return Number::parse_big_float(s); }
            let f = f.unwrap();
            if f >= std::f32::MIN as f64 && f <= std::f32::MAX as f64 {
                Number::FloatingPoint(FloatingPoint::Float32(f as f32))
            } else {
                Number::FloatingPoint(FloatingPoint::Float64(f))
            }
        } else {
            if s.starts_with('-') {
                let i = s[1..].parse::<i128>();
                if i.is_err() { return Number::parse_big_int(s); }
                let i = i.unwrap();
                if i >= std::i8::MIN as i128 && i <= std::i8::MAX as i128 {
                    Number::SignedInteger(SignedInteger::Int8(i as i8))
                } else if i >= std::i16::MIN as i128 && i <= std::i16::MAX as i128 {
                    Number::SignedInteger(SignedInteger::Int16(i as i16))
                } else if i >= std::i32::MIN as i128 && i <= std::i32::MAX as i128 {
                    Number::SignedInteger(SignedInteger::Int32(i as i32))
                } else if i >= std::i64::MIN as i128 && i <= std::i64::MAX as i128 {
                    Number::SignedInteger(SignedInteger::Int64(i as i64))
                } else {
                    Number::SignedInteger(SignedInteger::Int128(i as i128))
                }
            } else {
                let u = s.parse::<i128>();
                if u.is_err() { return Number::parse_big_uint(s); }
                let u = u.unwrap();
                if u >= std::u8::MIN as i128 && u <= std::u8::MAX as i128 {
                    Number::UnsignedInteger(UnsignedInteger::UInt8(u as u8))
                } else if u >= std::u16::MIN as i128 && u <= std::u16::MAX as i128 {
                    Number::UnsignedInteger(UnsignedInteger::UInt16(u as u16))
                } else if u >= std::u32::MIN as i128 && u <= std::u32::MAX as i128 {
                    Number::UnsignedInteger(UnsignedInteger::UInt32(u as u32))
                } else if u >= std::u64::MIN as i128 && u <= std::u64::MAX as i128 {
                    Number::UnsignedInteger(UnsignedInteger::UInt64(u as u64))
                } else {
                    Number::UnsignedInteger(UnsignedInteger::UInt128(u as u128))
                }
            }
        }
    }
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct UserFunctionVariation {
    params: FunctionParameterType,
    body: Ast,
    return_type: Type,
}

/**
 * Is the value representation of `FunctionParameterType`.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum NativeFunctionParameters {
    Singles(Vec<Value>),
    Variadic(Option<Vec<Value>>, Vec<Value>), // Some initial values of different types, followed by the variadic type values
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionVariation {
    User(UserFunctionVariation),
    Native(fn(NativeFunctionParameters) -> InterpretResult, FunctionParameterType, Type), // Built-in functions
}

impl GetType for FunctionVariation {
    fn get_type(&self) -> Type {
        match self {
            FunctionVariation::User(v) => Type::Function(Box::new(v.params.clone()), Box::new(v.return_type.clone())),
            FunctionVariation::Native(_, v, r) => Type::Function(Box::new(v.clone()), Box::new(r.clone())),
        }
    }
}

impl Display for FunctionVariation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let ret_type = match self {
            FunctionVariation::User(v) => { v.params.fmt(f)?; &v.return_type },
            FunctionVariation::Native(_, v, r) => { v.fmt(f)?; &r },
        };
        write!(f, ") -> {}", ret_type) // TODO: Make this a unicode arrow
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    name: String,
    variations: Vec<FunctionVariation>, // Function types are inferred from variations
    // TODO: Add an environment for the function
}

impl Function {
    pub fn new(name: String, variations: Vec<FunctionVariation>) -> Self {
        Self { name, variations }
    }
}

/**
 * A Lento value is a value that can be stored in a variable, returned from a function, or passed as an argument.
 * These values are stored in the interpreter's memory during runtime and are garbage collected when they are no longer in use.
 * The interpreter takes AST nodes and evaluates them to produce a value.
 * Values can be referenced and used in other expressions.
 */
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
}

impl GetType for Value {
    fn get_type(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Number(n) => {
                match n {
                    Number::UnsignedInteger(u) => match u {
                        UnsignedInteger::UInt1(_) => std_primitive_types::UINT1,
                        UnsignedInteger::UInt8(_) => std_primitive_types::UINT8,
                        UnsignedInteger::UInt16(_) => std_primitive_types::UINT16,
                        UnsignedInteger::UInt32(_) => std_primitive_types::UINT32,
                        UnsignedInteger::UInt64(_) => std_primitive_types::UINT64,
                        UnsignedInteger::UInt128(_) => std_primitive_types::UINT128,
                        UnsignedInteger::UIntVar(_) => std_primitive_types::UINTBIG
                    },
                    Number::SignedInteger(i) => match i {
                        SignedInteger::Int8(_) => std_primitive_types::INT8,
                        SignedInteger::Int16(_) => std_primitive_types::INT16,
                        SignedInteger::Int32(_) => std_primitive_types::INT32,
                        SignedInteger::Int64(_) => std_primitive_types::INT64,
                        SignedInteger::Int128(_) => std_primitive_types::INT128,
                        SignedInteger::IntVar(_) => std_primitive_types::INTBIG
                    },
                    Number::FloatingPoint(f) => match f {
                        FloatingPoint::Float32(_) => std_primitive_types::FLOAT32,
                        FloatingPoint::Float64(_) => std_primitive_types::FLOAT64,
                        FloatingPoint::FloatBig(_) => std_primitive_types::FLOATBIG
                    },
                }
            },
            Value::String(_) => std_primitive_types::STRING,
            Value::Char(_) => std_primitive_types::CHAR,
            Value::Boolean(_) => std_primitive_types::BOOL,
            Value::Tuple(_, t) => t.clone(),
            Value::List(_, t) => t.clone(),
            Value::Record(_, t) => t.clone(),
            Value::Function(f) => panic!("Cannot get type of functions"), // Because functions can have multiple types
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Number(n) => match n {
                Number::UnsignedInteger(u) => match u {
                    UnsignedInteger::UInt1(b) => write!(f, "{}", b),
                    UnsignedInteger::UInt8(b) => write!(f, "{}", b),
                    UnsignedInteger::UInt16(s) => write!(f, "{}", s),
                    UnsignedInteger::UInt32(i) => write!(f, "{}", i),
                    UnsignedInteger::UInt64(i) => write!(f, "{}", i),
                    UnsignedInteger::UInt128(i) => write!(f, "{}", i),
                    UnsignedInteger::UIntVar(i) => write!(f, "{}", i),
                },
                Number::SignedInteger(s) => match s {
                    SignedInteger::Int8(b) => write!(f, "{}", b),
                    SignedInteger::Int16(s) => write!(f, "{}", s),
                    SignedInteger::Int32(i) => write!(f, "{}", i),
                    SignedInteger::Int64(i) => write!(f, "{}", i),
                    SignedInteger::Int128(i) => write!(f, "{}", i),
                    SignedInteger::IntVar(i) => write!(f, "{}", i),
                },
                Number::FloatingPoint(fl) => match fl {
                    FloatingPoint::Float32(fl) => write!(f, "{}", fl),
                    FloatingPoint::Float64(fl) => write!(f, "{}", fl),
                    FloatingPoint::FloatBig(fl) => write!(f, "{}", fl),
                },
            },
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
                write!(f, "function[{}] {{", fun.name)?;
                for (i, v) in fun.variations.iter().enumerate() {
                    writeln!(f, "\t{}", v)?;
                }
                write!(f, "}}")
            },
        }
    }
}
