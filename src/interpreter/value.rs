use std::{collections::HashMap, fmt::Display};

use num_bigfloat::BigFloat;
use num_bigint::{BigInt, BigUint};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnsignedInteger {
    Bit(u8),
    UByte(u8),
    UShort(u16),
    UInt32(u32),
    UInt64(u64),
    UInt128(u128),
    UIntVar(BigUint), // Arbitrary sized integers (variable size)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignedInteger {
    Byte(i8),
    Short(i16),
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
    Tuple(Vec<Value>),
    List(Vec<Value>),
    Record(HashMap<RecordKey, Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "Unit"),
            Value::Number(n) => match n {
                Number::UnsignedInteger(u) => match u {
                    UnsignedInteger::Bit(b) => write!(f, "{}", b),
                    UnsignedInteger::UByte(b) => write!(f, "{}", b),
                    UnsignedInteger::UShort(s) => write!(f, "{}", s),
                    UnsignedInteger::UInt32(i) => write!(f, "{}", i),
                    UnsignedInteger::UInt64(i) => write!(f, "{}", i),
                    UnsignedInteger::UInt128(i) => write!(f, "{}", i),
                    UnsignedInteger::UIntVar(i) => write!(f, "{}", i),
                },
                Number::SignedInteger(s) => match s {
                    SignedInteger::Byte(b) => write!(f, "{}", b),
                    SignedInteger::Short(s) => write!(f, "{}", s),
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
            Value::Tuple(t) => {
                write!(f, "(")?;
                for (i, v) in t.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i < t.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Value::List(l) => {
                write!(f, "[")?;
                for (i, v) in l.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i < l.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Record(r) => {
                write!(f, "{{ ")?;
                for (i, (k, v)) in r.iter().enumerate() {
                    write!(f, "{}: {}", k, v)?;
                    if i < r.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
        }
    }
}
