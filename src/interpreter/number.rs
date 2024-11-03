use std::{
    cmp::Ordering,
    fmt::{Display, Formatter},
};

use malachite::{
    num::conversion::{string::options::ToSciOptions, traits::ToSci},
    Integer, Natural, Rational,
};

use crate::type_checker::types::{std_primitive_types, GetType, Type};

pub trait NumberCasting<T> {
    /// Upcasts the value to the given reference size
    fn upcast(&self, to_size: BitSize) -> T;

    /// Returns the smallest possible type that can hold the current value without loss of precision or overflow.
    /// Try to downcast the type of the UnsignedInteger to the type of the reference.
    /// Returns None if the downcast is not possible (value is too large to fit in the reference type)
    /// Returns Some(UnsignedInteger) if the downcast is possible
    /// # Arguments
    /// * `to_size` - The reference type size to cast to
    /// # Note
    /// This function does not check if the reference type is smaller or equal to the current type.
    /// This is the responsibility of the caller.
    fn optimize(self) -> T;
}

pub trait ArithmeticOperations<T> {
    fn add(lhs: &T, rhs: &T) -> T;
    fn sub(lhs: &T, rhs: &T) -> T;
    fn mul(lhs: &T, rhs: &T) -> T;
    fn div(lhs: &T, rhs: &T) -> T;
}

/// Represents the size of a number
/// The size is used to determine the maximum and minimum values that can be stored in the number
/// The size is also used to determine the type of the number
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BitSize {
    Bit1 = 1,
    Bit8 = 8,
    Bit16 = 16,
    Bit32 = 32,
    Bit64 = 64,
    Bit128 = 128,
    /// Variable size (arbitrary precision) represented as the maximum value of i32
    BitVar = i32::MAX as isize,
}

trait NumberMethods {
    fn get_size(&self) -> BitSize;
}

/// An unsigned integer number type that can be represented in 1, 8, 16, 32, 64, 128 bits or arbitrary precision.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnsignedInteger {
    UInt1(u8), // Bit
    UInt8(u8), // Byte
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UInt128(u128),
    UIntVar(Natural), // Arbitrary sized integers (variable size)
}

impl GetType for UnsignedInteger {
    fn get_type(&self) -> &Type {
        match self {
            UnsignedInteger::UInt1(_) => &std_primitive_types::UINT1,
            UnsignedInteger::UInt8(_) => &std_primitive_types::UINT8,
            UnsignedInteger::UInt16(_) => &std_primitive_types::UINT16,
            UnsignedInteger::UInt32(_) => &std_primitive_types::UINT32,
            UnsignedInteger::UInt64(_) => &std_primitive_types::UINT64,
            UnsignedInteger::UInt128(_) => &std_primitive_types::UINT128,
            UnsignedInteger::UIntVar(_) => &std_primitive_types::UINTBIG,
        }
    }
}

impl NumberMethods for UnsignedInteger {
    fn get_size(&self) -> BitSize {
        match self {
            UnsignedInteger::UInt1(_) => BitSize::Bit1,
            UnsignedInteger::UInt8(_) => BitSize::Bit8,
            UnsignedInteger::UInt16(_) => BitSize::Bit16,
            UnsignedInteger::UInt32(_) => BitSize::Bit32,
            UnsignedInteger::UInt64(_) => BitSize::Bit64,
            UnsignedInteger::UInt128(_) => BitSize::Bit128,
            UnsignedInteger::UIntVar(_) => BitSize::BitVar,
        }
    }
}

impl NumberCasting<UnsignedInteger> for UnsignedInteger {
    /// Upcasts the value to the given reference size
    /// # Arguments
    /// * `to_size` - The reference type size to cast to
    /// # Note
    /// This function does not check if the reference size is smaller or equal to the current size
    /// This is the responsibility of the caller
    fn upcast(&self, to_size: BitSize) -> UnsignedInteger {
        match (self, to_size) {
            (UnsignedInteger::UInt1(v), BitSize::Bit8) => UnsignedInteger::UInt8(*v),
            (UnsignedInteger::UInt1(v), BitSize::Bit16) => UnsignedInteger::UInt16(*v as u16),
            (UnsignedInteger::UInt1(v), BitSize::Bit32) => UnsignedInteger::UInt32(*v as u32),
            (UnsignedInteger::UInt1(v), BitSize::Bit64) => UnsignedInteger::UInt64(*v as u64),
            (UnsignedInteger::UInt1(v), BitSize::Bit128) => UnsignedInteger::UInt128(*v as u128),
            (UnsignedInteger::UInt1(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(Natural::from(*v))
            }
            (UnsignedInteger::UInt8(v), BitSize::Bit16) => UnsignedInteger::UInt16(*v as u16),
            (UnsignedInteger::UInt8(v), BitSize::Bit32) => UnsignedInteger::UInt32(*v as u32),
            (UnsignedInteger::UInt8(v), BitSize::Bit64) => UnsignedInteger::UInt64(*v as u64),
            (UnsignedInteger::UInt8(v), BitSize::Bit128) => UnsignedInteger::UInt128(*v as u128),
            (UnsignedInteger::UInt8(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(Natural::from(*v))
            }
            (UnsignedInteger::UInt16(v), BitSize::Bit32) => UnsignedInteger::UInt32(*v as u32),
            (UnsignedInteger::UInt16(v), BitSize::Bit64) => UnsignedInteger::UInt64(*v as u64),
            (UnsignedInteger::UInt16(v), BitSize::Bit128) => UnsignedInteger::UInt128(*v as u128),
            (UnsignedInteger::UInt16(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(Natural::from(*v))
            }
            (UnsignedInteger::UInt32(v), BitSize::Bit64) => UnsignedInteger::UInt64(*v as u64),
            (UnsignedInteger::UInt32(v), BitSize::Bit128) => UnsignedInteger::UInt128(*v as u128),
            (UnsignedInteger::UInt32(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(Natural::from(*v))
            }
            (UnsignedInteger::UInt64(v), BitSize::Bit128) => UnsignedInteger::UInt128(*v as u128),
            (UnsignedInteger::UInt64(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(Natural::from(*v))
            }
            (UnsignedInteger::UInt128(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(Natural::from(*v))
            }
            _ => unreachable!(),
        }
    }

    fn optimize(self) -> UnsignedInteger {
        match self {
            UnsignedInteger::UInt1(_) => self,
            UnsignedInteger::UInt8(v) => {
                if v <= 1 {
                    UnsignedInteger::UInt1(v)
                } else {
                    self
                }
            }
            UnsignedInteger::UInt16(v) => {
                if v <= 1 {
                    UnsignedInteger::UInt1(v as u8)
                } else if v <= u8::MAX as u16 {
                    UnsignedInteger::UInt8(v as u8)
                } else {
                    self
                }
            }
            UnsignedInteger::UInt32(v) => {
                if v <= 1 {
                    UnsignedInteger::UInt1(v as u8)
                } else if v <= u8::MAX as u32 {
                    UnsignedInteger::UInt8(v as u8)
                } else if v <= u16::MAX as u32 {
                    UnsignedInteger::UInt16(v as u16)
                } else {
                    self
                }
            }
            UnsignedInteger::UInt64(v) => {
                if v <= 1 {
                    UnsignedInteger::UInt1(v as u8)
                } else if v <= u8::MAX as u64 {
                    UnsignedInteger::UInt8(v as u8)
                } else if v <= u16::MAX as u64 {
                    UnsignedInteger::UInt16(v as u16)
                } else if v <= u32::MAX as u64 {
                    UnsignedInteger::UInt32(v as u32)
                } else {
                    self
                }
            }
            UnsignedInteger::UInt128(v) => {
                if v <= 1 {
                    UnsignedInteger::UInt1(v as u8)
                } else if v <= u8::MAX as u128 {
                    UnsignedInteger::UInt8(v as u8)
                } else if v <= u16::MAX as u128 {
                    UnsignedInteger::UInt16(v as u16)
                } else if v <= u32::MAX as u128 {
                    UnsignedInteger::UInt32(v as u32)
                } else if v <= u64::MAX as u128 {
                    UnsignedInteger::UInt64(v as u64)
                } else {
                    self
                }
            }
            UnsignedInteger::UIntVar(ref v) => {
                if v < &Natural::from(1u8) {
                    if let Ok(v) = u8::try_from(v) {
                        return UnsignedInteger::UInt1(v);
                    }
                }
                if let Ok(v) = u8::try_from(v) {
                    UnsignedInteger::UInt8(v)
                } else if let Ok(v) = u16::try_from(v) {
                    UnsignedInteger::UInt16(v)
                } else if let Ok(v) = u32::try_from(v) {
                    UnsignedInteger::UInt32(v)
                } else if let Ok(v) = u64::try_from(v) {
                    UnsignedInteger::UInt64(v)
                } else if let Ok(v) = u128::try_from(v) {
                    UnsignedInteger::UInt128(v)
                } else {
                    self
                }
            }
        }
    }
}

impl UnsignedInteger {
    pub fn to_signed(&self) -> SignedInteger {
        match self {
            UnsignedInteger::UInt1(v) => SignedInteger::Int8(*v as i8),
            UnsignedInteger::UInt8(v) => SignedInteger::Int8(*v as i8),
            UnsignedInteger::UInt16(v) => SignedInteger::Int16(*v as i16),
            UnsignedInteger::UInt32(v) => SignedInteger::Int32(*v as i32),
            UnsignedInteger::UInt64(v) => SignedInteger::Int64(*v as i64),
            UnsignedInteger::UInt128(v) => SignedInteger::Int128(*v as i128),
            UnsignedInteger::UIntVar(v) => SignedInteger::IntVar(Integer::from(v.clone())),
        }
    }

    pub fn to_float(&self) -> FloatingPoint {
        match self {
            UnsignedInteger::UInt1(v) => FloatingPoint::Float32(*v as f32),
            UnsignedInteger::UInt8(v) => FloatingPoint::Float32(*v as f32),
            UnsignedInteger::UInt16(v) => FloatingPoint::Float32(*v as f32),
            UnsignedInteger::UInt32(v) => FloatingPoint::Float32(*v as f32),
            UnsignedInteger::UInt64(v) => FloatingPoint::Float64(*v as f64),
            UnsignedInteger::UInt128(v) => FloatingPoint::Float64(*v as f64),
            UnsignedInteger::UIntVar(v) => FloatingPoint::FloatBig(Rational::from(v.clone())),
        }
    }
}

impl ArithmeticOperations<UnsignedInteger> for UnsignedInteger {
    fn add(lhs: &UnsignedInteger, rhs: &UnsignedInteger) -> UnsignedInteger {
        match lhs.get_size().cmp(&rhs.get_size()) {
            // If lhs is greater than rhs, upcast rhs to the size of lhs
            Ordering::Greater => UnsignedInteger::add(lhs, &rhs.upcast(lhs.get_size())),
            // If lhs is less than rhs, upcast lhs to the size of rhs
            Ordering::Less => UnsignedInteger::add(&lhs.upcast(rhs.get_size()), rhs),
            // If lhs is equal to rhs, perform the addition
            Ordering::Equal => match (lhs, rhs) {
                (UnsignedInteger::UInt1(lhs), UnsignedInteger::UInt1(rhs)) => {
                    let res = lhs + rhs;
                    if res <= 1 {
                        UnsignedInteger::UInt1(res)
                    } else {
                        UnsignedInteger::UInt8(res)
                    }
                }
                (UnsignedInteger::UInt8(lhs), UnsignedInteger::UInt8(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        UnsignedInteger::UInt8(res)
                    } else {
                        UnsignedInteger::UInt16(u16::from(*lhs) + u16::from(*rhs))
                    }
                }
                (UnsignedInteger::UInt16(lhs), UnsignedInteger::UInt16(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        UnsignedInteger::UInt16(res)
                    } else {
                        UnsignedInteger::UInt32(u32::from(*lhs) + u32::from(*rhs))
                    }
                }
                (UnsignedInteger::UInt32(lhs), UnsignedInteger::UInt32(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        UnsignedInteger::UInt32(res)
                    } else {
                        UnsignedInteger::UIntVar(Natural::from(*lhs) + Natural::from(*rhs))
                    }
                }
                (UnsignedInteger::UInt64(lhs), UnsignedInteger::UInt64(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        UnsignedInteger::UInt64(res)
                    } else {
                        UnsignedInteger::UIntVar(Natural::from(*lhs) + Natural::from(*rhs))
                    }
                }
                (UnsignedInteger::UInt128(lhs), UnsignedInteger::UInt128(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        UnsignedInteger::UInt128(res)
                    } else {
                        UnsignedInteger::UIntVar(Natural::from(*lhs) + Natural::from(*rhs))
                    }
                }
                (UnsignedInteger::UIntVar(lhs), UnsignedInteger::UIntVar(rhs)) => {
                    UnsignedInteger::UIntVar(lhs + rhs)
                }
                _ => panic!("Cannot add unsigned integers of different sizes"),
            },
        }
    }

    fn sub(_lhs: &UnsignedInteger, _rhs: &UnsignedInteger) -> UnsignedInteger {
        todo!()
    }

    fn mul(_lhs: &UnsignedInteger, _rhs: &UnsignedInteger) -> UnsignedInteger {
        todo!()
    }

    fn div(_lhs: &UnsignedInteger, _rhs: &UnsignedInteger) -> UnsignedInteger {
        todo!()
    }
}

/// A signed integer number type that can be represented in 8, 16, 32, 64, 128 bits or arbitrary precision.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignedInteger {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    IntVar(Integer), // Arbitrary sized integers (variable size)
}

impl GetType for SignedInteger {
    fn get_type(&self) -> &Type {
        match self {
            SignedInteger::Int8(_) => &std_primitive_types::INT8,
            SignedInteger::Int16(_) => &std_primitive_types::INT16,
            SignedInteger::Int32(_) => &std_primitive_types::INT32,
            SignedInteger::Int64(_) => &std_primitive_types::INT64,
            SignedInteger::Int128(_) => &std_primitive_types::INT128,
            SignedInteger::IntVar(_) => &std_primitive_types::INTBIG,
        }
    }
}

impl NumberMethods for SignedInteger {
    fn get_size(&self) -> BitSize {
        match self {
            SignedInteger::Int8(_) => BitSize::Bit8,
            SignedInteger::Int16(_) => BitSize::Bit16,
            SignedInteger::Int32(_) => BitSize::Bit32,
            SignedInteger::Int64(_) => BitSize::Bit64,
            SignedInteger::Int128(_) => BitSize::Bit128,
            SignedInteger::IntVar(_) => BitSize::BitVar,
        }
    }
}

impl NumberCasting<SignedInteger> for SignedInteger {
    /// Upcasts the value to the given reference size
    /// # Arguments
    /// * `to_size` - The reference type size to cast to
    /// # Note
    /// This function does not check if the reference size is smaller or equal to the current size
    /// This is the responsibility of the caller
    fn upcast(&self, to_size: BitSize) -> SignedInteger {
        match (self, to_size) {
            (SignedInteger::Int8(v), BitSize::Bit16) => SignedInteger::Int16(*v as i16),
            (SignedInteger::Int8(v), BitSize::Bit32) => SignedInteger::Int32(*v as i32),
            (SignedInteger::Int8(v), BitSize::Bit64) => SignedInteger::Int64(*v as i64),
            (SignedInteger::Int8(v), BitSize::Bit128) => SignedInteger::Int128(*v as i128),
            (SignedInteger::Int8(v), BitSize::BitVar) => SignedInteger::IntVar(Integer::from(*v)),
            (SignedInteger::Int16(v), BitSize::Bit32) => SignedInteger::Int32(*v as i32),
            (SignedInteger::Int16(v), BitSize::Bit64) => SignedInteger::Int64(*v as i64),
            (SignedInteger::Int16(v), BitSize::Bit128) => SignedInteger::Int128(*v as i128),
            (SignedInteger::Int16(v), BitSize::BitVar) => SignedInteger::IntVar(Integer::from(*v)),
            (SignedInteger::Int32(v), BitSize::Bit64) => SignedInteger::Int64(*v as i64),
            (SignedInteger::Int32(v), BitSize::Bit128) => SignedInteger::Int128(*v as i128),
            (SignedInteger::Int32(v), BitSize::BitVar) => SignedInteger::IntVar(Integer::from(*v)),
            (SignedInteger::Int64(v), BitSize::Bit128) => SignedInteger::Int128(*v as i128),
            (SignedInteger::Int64(v), BitSize::BitVar) => SignedInteger::IntVar(Integer::from(*v)),
            (SignedInteger::Int128(v), BitSize::BitVar) => SignedInteger::IntVar(Integer::from(*v)),
            _ => unreachable!(),
        }
    }

    fn optimize(self) -> SignedInteger {
        match self {
            SignedInteger::Int8(_) => self,
            SignedInteger::Int16(v) => {
                if v >= i8::MIN as i16 && v <= i8::MAX as i16 {
                    SignedInteger::Int8(v as i8)
                } else {
                    self
                }
            }
            SignedInteger::Int32(v) => {
                if v >= i8::MIN as i32 && v <= i8::MAX as i32 {
                    SignedInteger::Int8(v as i8)
                } else if v >= i16::MIN as i32 && v <= i16::MAX as i32 {
                    SignedInteger::Int16(v as i16)
                } else {
                    self
                }
            }
            SignedInteger::Int64(v) => {
                if v >= i8::MIN as i64 && v <= i8::MAX as i64 {
                    SignedInteger::Int8(v as i8)
                } else if v >= i16::MIN as i64 && v <= i16::MAX as i64 {
                    SignedInteger::Int16(v as i16)
                } else if v >= i32::MIN as i64 && v <= i32::MAX as i64 {
                    SignedInteger::Int32(v as i32)
                } else {
                    self
                }
            }
            SignedInteger::Int128(v) => {
                if v >= i8::MIN as i128 && v <= i8::MAX as i128 {
                    SignedInteger::Int8(v as i8)
                } else if v >= i16::MIN as i128 && v <= i16::MAX as i128 {
                    SignedInteger::Int16(v as i16)
                } else if v >= i32::MIN as i128 && v <= i32::MAX as i128 {
                    SignedInteger::Int32(v as i32)
                } else if v >= i64::MIN as i128 && v <= i64::MAX as i128 {
                    SignedInteger::Int64(v as i64)
                } else {
                    self
                }
            }
            SignedInteger::IntVar(ref v) => {
                if let Ok(v) = i8::try_from(v) {
                    SignedInteger::Int8(v)
                } else if let Ok(v) = i16::try_from(v) {
                    SignedInteger::Int16(v)
                } else if let Ok(v) = i32::try_from(v) {
                    SignedInteger::Int32(v)
                } else if let Ok(v) = i64::try_from(v) {
                    SignedInteger::Int64(v)
                } else if let Ok(v) = i128::try_from(v) {
                    SignedInteger::Int128(v)
                } else {
                    self
                }
            }
        }
    }
}

impl SignedInteger {
    pub fn to_float(&self) -> FloatingPoint {
        match self {
            SignedInteger::Int8(v) => FloatingPoint::Float32(*v as f32),
            SignedInteger::Int16(v) => FloatingPoint::Float32(*v as f32),
            SignedInteger::Int32(v) => FloatingPoint::Float32(*v as f32),
            SignedInteger::Int64(v) => FloatingPoint::Float64(*v as f64),
            SignedInteger::Int128(v) => FloatingPoint::Float64(*v as f64),
            SignedInteger::IntVar(v) => FloatingPoint::FloatBig(Rational::from(v.clone())),
        }
    }
}

impl ArithmeticOperations<SignedInteger> for SignedInteger {
    fn add(lhs: &SignedInteger, rhs: &SignedInteger) -> SignedInteger {
        match lhs.get_size().cmp(&rhs.get_size()) {
            // If lhs is greater than rhs, upcast rhs to the size of lhs
            Ordering::Greater => SignedInteger::add(lhs, &rhs.upcast(lhs.get_size())),
            // If lhs is less than rhs, upcast lhs to the size of rhs
            Ordering::Less => SignedInteger::add(&lhs.upcast(rhs.get_size()), rhs),
            // If lhs is equal to rhs, perform the addition
            Ordering::Equal => match (lhs, rhs) {
                (SignedInteger::Int8(lhs), SignedInteger::Int8(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        SignedInteger::Int8(res)
                    } else {
                        SignedInteger::Int16(i16::from(*lhs) + i16::from(*rhs))
                    }
                }
                (SignedInteger::Int16(lhs), SignedInteger::Int16(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        SignedInteger::Int16(res)
                    } else {
                        SignedInteger::Int32(i32::from(*lhs) + i32::from(*rhs))
                    }
                }
                (SignedInteger::Int32(lhs), SignedInteger::Int32(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        SignedInteger::Int32(res)
                    } else {
                        SignedInteger::IntVar(Integer::from(*lhs) + Integer::from(*rhs))
                    }
                }
                (SignedInteger::Int64(lhs), SignedInteger::Int64(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        SignedInteger::Int64(res)
                    } else {
                        SignedInteger::IntVar(Integer::from(*lhs) + Integer::from(*rhs))
                    }
                }
                (SignedInteger::Int128(lhs), SignedInteger::Int128(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        SignedInteger::Int128(res)
                    } else {
                        SignedInteger::IntVar(Integer::from(*lhs) + Integer::from(*rhs))
                    }
                }
                (SignedInteger::IntVar(lhs), SignedInteger::IntVar(rhs)) => {
                    SignedInteger::IntVar(lhs + rhs)
                }
                _ => panic!("Cannot add signed integers of different sizes"),
            },
        }
    }

    fn sub(_lhs: &SignedInteger, _rhs: &SignedInteger) -> SignedInteger {
        todo!()
    }

    fn mul(_lhs: &SignedInteger, _rhs: &SignedInteger) -> SignedInteger {
        todo!()
    }

    fn div(_lhs: &SignedInteger, _rhs: &SignedInteger) -> SignedInteger {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FloatingPoint {
    Float32(f32),
    Float64(f64),
    FloatBig(Rational), // Increased precision floating point numbers (fixed but large size)
                        // Look at https://github.com/stencillogic/astro-float for handling Arbitrary precision floating point numbers (variable size)
}

impl GetType for FloatingPoint {
    fn get_type(&self) -> &Type {
        match self {
            FloatingPoint::Float32(_) => &std_primitive_types::FLOAT32,
            FloatingPoint::Float64(_) => &std_primitive_types::FLOAT64,
            FloatingPoint::FloatBig(_) => &std_primitive_types::FLOATBIG,
        }
    }
}

impl NumberMethods for FloatingPoint {
    fn get_size(&self) -> BitSize {
        match self {
            FloatingPoint::Float32(_) => BitSize::Bit32,
            FloatingPoint::Float64(_) => BitSize::Bit64,
            FloatingPoint::FloatBig(_) => BitSize::BitVar,
        }
    }
}

impl NumberCasting<FloatingPoint> for FloatingPoint {
    fn upcast(&self, to_size: BitSize) -> FloatingPoint {
        match (self, to_size) {
            (FloatingPoint::Float32(v), BitSize::Bit64) => FloatingPoint::Float64(*v as f64),
            (FloatingPoint::Float32(v), BitSize::BitVar) => {
                // TODO: Do not unwrap here
                FloatingPoint::FloatBig(Rational::try_from_float_simplest(*v).unwrap())
            }
            (FloatingPoint::Float64(v), BitSize::BitVar) => {
                // TODO: Do not unwrap here
                FloatingPoint::FloatBig(Rational::try_from_float_simplest(*v).unwrap())
            }
            _ => unreachable!(),
        }
    }

    fn optimize(self) -> FloatingPoint {
        match self {
            FloatingPoint::Float32(_) => self,
            FloatingPoint::Float64(v) => {
                // TODO: Look at resolution of Rational to f32/f64, not only max/min values
                if v >= f32::MIN as f64 && v <= f32::MAX as f64 {
                    FloatingPoint::Float32(v as f32)
                } else {
                    self
                }
            }
            FloatingPoint::FloatBig(ref v) => {
                if let Ok(v) = f32::try_from(v) {
                    FloatingPoint::Float32(v)
                } else if let Ok(v) = f64::try_from(v) {
                    FloatingPoint::Float64(v)
                } else {
                    self
                }
            }
        }
    }
}

impl ArithmeticOperations<FloatingPoint> for FloatingPoint {
    fn add(lhs: &FloatingPoint, rhs: &FloatingPoint) -> FloatingPoint {
        match lhs.get_size().cmp(&rhs.get_size()) {
            // If lhs is greater than rhs, upcast rhs to the size of lhs
            Ordering::Greater => FloatingPoint::add(lhs, &rhs.upcast(lhs.get_size())),
            // If lhs is less than rhs, upcast lhs to the size of rhs
            Ordering::Less => FloatingPoint::add(&lhs.upcast(rhs.get_size()), rhs),
            // If lhs is equal to rhs, perform the addition
            Ordering::Equal => match (lhs, rhs) {
                (FloatingPoint::Float32(lhs), FloatingPoint::Float32(rhs)) => {
                    FloatingPoint::Float32(lhs + rhs)
                }
                (FloatingPoint::Float64(lhs), FloatingPoint::Float64(rhs)) => {
                    FloatingPoint::Float64(lhs + rhs)
                }
                (FloatingPoint::FloatBig(lhs), FloatingPoint::FloatBig(rhs)) => {
                    FloatingPoint::FloatBig(lhs + rhs)
                }
                _ => panic!("Cannot add floating point numbers of different sizes"),
            },
        }
    }

    fn sub(_lhs: &FloatingPoint, _rhs: &FloatingPoint) -> FloatingPoint {
        todo!()
    }

    fn mul(_lhs: &FloatingPoint, _rhs: &FloatingPoint) -> FloatingPoint {
        todo!()
    }

    fn div(_lhs: &FloatingPoint, _rhs: &FloatingPoint) -> FloatingPoint {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    UnsignedInteger(UnsignedInteger),
    SignedInteger(SignedInteger),
    FloatingPoint(FloatingPoint),
}

impl GetType for Number {
    fn get_type(&self) -> &Type {
        match self {
            Number::UnsignedInteger(u) => u.get_type(),
            Number::SignedInteger(i) => i.get_type(),
            Number::FloatingPoint(f) => f.get_type(),
        }
    }
}

impl ArithmeticOperations<Number> for Number {
    fn add(lhs: &Number, rhs: &Number) -> Number {
        match (lhs, rhs) {
            (Number::UnsignedInteger(lhs), Number::UnsignedInteger(rhs)) => {
                Number::UnsignedInteger(UnsignedInteger::add(lhs, rhs))
            }
            (Number::SignedInteger(lhs), Number::SignedInteger(rhs)) => {
                Number::SignedInteger(SignedInteger::add(lhs, rhs))
            }
            (Number::FloatingPoint(lhs), Number::FloatingPoint(rhs)) => {
                Number::FloatingPoint(FloatingPoint::add(lhs, rhs))
            }
            (Number::UnsignedInteger(lhs), Number::SignedInteger(rhs)) => {
                Number::SignedInteger(SignedInteger::add(&lhs.to_signed(), rhs))
            }
            (Number::SignedInteger(lhs), Number::UnsignedInteger(rhs)) => {
                Number::SignedInteger(SignedInteger::add(lhs, &rhs.to_signed()))
            }
            (Number::UnsignedInteger(lhs), Number::FloatingPoint(rhs)) => {
                Number::FloatingPoint(FloatingPoint::add(&lhs.to_float(), rhs))
            }
            (Number::FloatingPoint(lhs), Number::UnsignedInteger(rhs)) => {
                Number::FloatingPoint(FloatingPoint::add(lhs, &rhs.to_float()))
            }
            (Number::SignedInteger(lhs), Number::FloatingPoint(rhs)) => {
                Number::FloatingPoint(FloatingPoint::add(&lhs.to_float(), rhs))
            }
            (Number::FloatingPoint(lhs), Number::SignedInteger(rhs)) => {
                Number::FloatingPoint(FloatingPoint::add(lhs, &rhs.to_float()))
            }
        }
    }

    fn sub(_lhs: &Number, _rhs: &Number) -> Number {
        todo!()
    }

    fn mul(_lhs: &Number, _rhs: &Number) -> Number {
        todo!()
    }

    fn div(_lhs: &Number, _rhs: &Number) -> Number {
        todo!()
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
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
                FloatingPoint::FloatBig(fl) => {
                    let mut sci_opts: ToSciOptions = ToSciOptions::default();
                    sci_opts.set_precision(50);
                    sci_opts.set_scale(15);
                    write!(f, "{}", fl.to_sci_with_options(sci_opts))
                }
            },
        }
    }
}
