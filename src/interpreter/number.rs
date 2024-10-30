use num_bigfloat::BigFloat;
use num_bigint::{BigInt, BigUint};

use std::{
    cmp::Ordering,
    fmt::{Display, Formatter},
};

use crate::type_checker::types::{std_primitive_types, CheckedType, GetType, Type};

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

trait NumberMethods<BIT8, BIT16, BIT32, BIT64, BIT128, BITVAR> {
    fn get_number_value<T>(&self) -> Option<T>
    where
        T: From<BIT8> + From<BIT16> + From<BIT32> + From<BIT64> + From<BIT128> + From<BITVAR>;
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
    UIntVar(BigUint), // Arbitrary sized integers (variable size)
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

impl NumberMethods<u8, u16, u32, u64, u128, BigUint> for UnsignedInteger {
    fn get_number_value<T>(&self) -> Option<T>
    where
        T: From<u8> + From<u16> + From<u32> + From<u64> + From<u128> + From<BigUint>,
    {
        match self {
            UnsignedInteger::UInt1(v) => Some(T::from(*v)),
            UnsignedInteger::UInt8(v) => Some(T::from(*v)),
            UnsignedInteger::UInt16(v) => Some(T::from(*v)),
            UnsignedInteger::UInt32(v) => Some(T::from(*v)),
            UnsignedInteger::UInt64(v) => Some(T::from(*v)),
            UnsignedInteger::UInt128(v) => Some(T::from(*v)),
            UnsignedInteger::UIntVar(v) => Some(T::from(v.clone())),
        }
    }

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
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt8(v), BitSize::Bit16) => UnsignedInteger::UInt16(*v as u16),
            (UnsignedInteger::UInt8(v), BitSize::Bit32) => UnsignedInteger::UInt32(*v as u32),
            (UnsignedInteger::UInt8(v), BitSize::Bit64) => UnsignedInteger::UInt64(*v as u64),
            (UnsignedInteger::UInt8(v), BitSize::Bit128) => UnsignedInteger::UInt128(*v as u128),
            (UnsignedInteger::UInt8(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt16(v), BitSize::Bit32) => UnsignedInteger::UInt32(*v as u32),
            (UnsignedInteger::UInt16(v), BitSize::Bit64) => UnsignedInteger::UInt64(*v as u64),
            (UnsignedInteger::UInt16(v), BitSize::Bit128) => UnsignedInteger::UInt128(*v as u128),
            (UnsignedInteger::UInt16(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt32(v), BitSize::Bit64) => UnsignedInteger::UInt64(*v as u64),
            (UnsignedInteger::UInt32(v), BitSize::Bit128) => UnsignedInteger::UInt128(*v as u128),
            (UnsignedInteger::UInt32(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt64(v), BitSize::Bit128) => UnsignedInteger::UInt128(*v as u128),
            (UnsignedInteger::UInt64(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt128(v), BitSize::BitVar) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
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
                if v.le(&BigUint::from(1u8)) {
                    UnsignedInteger::UInt1(v.iter_u32_digits().nth(0).unwrap() as u8)
                } else if v.le(&BigUint::from(u8::MAX)) {
                    UnsignedInteger::UInt8(v.iter_u32_digits().nth(0).unwrap() as u8)
                } else if v.le(&BigUint::from(u16::MAX)) {
                    UnsignedInteger::UInt16(v.iter_u32_digits().nth(0).unwrap() as u16)
                } else if v.le(&BigUint::from(u32::MAX)) {
                    UnsignedInteger::UInt32(v.iter_u32_digits().nth(0).unwrap())
                } else if v.le(&BigUint::from(u64::MAX)) {
                    UnsignedInteger::UInt64(v.iter_u32_digits().nth(0).unwrap() as u64)
                } else if v.le(&BigUint::from(u128::MAX)) {
                    UnsignedInteger::UInt128(v.iter_u32_digits().nth(0).unwrap() as u128)
                } else {
                    self
                }
            }
        }
    }
}

impl UnsignedInteger {
    pub fn to_signed(&self) -> Integer {
        match self {
            UnsignedInteger::UInt1(v) => Integer::Int8(*v as i8),
            UnsignedInteger::UInt8(v) => Integer::Int8(*v as i8),
            UnsignedInteger::UInt16(v) => Integer::Int16(*v as i16),
            UnsignedInteger::UInt32(v) => Integer::Int32(*v as i32),
            UnsignedInteger::UInt64(v) => Integer::Int64(*v as i64),
            UnsignedInteger::UInt128(v) => Integer::Int128(*v as i128),
            UnsignedInteger::UIntVar(v) => Integer::IntVar(BigInt::from(v.clone())),
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
            UnsignedInteger::UIntVar(v) => {
                let mut f = BigFloat::new();
                for d in v.iter_u32_digits().rev() {
                    println!("d: {}", d);
                    f = f.mul(&BigFloat::from(2u32.pow(32))).add(&BigFloat::from(d));
                }
                FloatingPoint::FloatBig(f)
            }
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
                        UnsignedInteger::UIntVar(BigUint::from(*lhs) + BigUint::from(*rhs))
                    }
                }
                (UnsignedInteger::UInt64(lhs), UnsignedInteger::UInt64(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        UnsignedInteger::UInt64(res)
                    } else {
                        UnsignedInteger::UIntVar(BigUint::from(*lhs) + BigUint::from(*rhs))
                    }
                }
                (UnsignedInteger::UInt128(lhs), UnsignedInteger::UInt128(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        UnsignedInteger::UInt128(res)
                    } else {
                        UnsignedInteger::UIntVar(BigUint::from(*lhs) + BigUint::from(*rhs))
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
pub enum Integer {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    IntVar(BigInt), // Arbitrary sized integers (variable size)
}

impl GetType for Integer {
    fn get_type(&self) -> &Type {
        match self {
            Integer::Int8(_) => &std_primitive_types::INT8,
            Integer::Int16(_) => &std_primitive_types::INT16,
            Integer::Int32(_) => &std_primitive_types::INT32,
            Integer::Int64(_) => &std_primitive_types::INT64,
            Integer::Int128(_) => &std_primitive_types::INT128,
            Integer::IntVar(_) => &std_primitive_types::INTBIG,
        }
    }
}

impl NumberMethods<i8, i16, i32, i64, i128, BigInt> for Integer {
    fn get_number_value<T>(&self) -> Option<T>
    where
        T: From<i8> + From<i16> + From<i32> + From<i64> + From<i128> + From<BigInt>,
    {
        match self {
            Integer::Int8(v) => Some(T::from(*v)),
            Integer::Int16(v) => Some(T::from(*v)),
            Integer::Int32(v) => Some(T::from(*v)),
            Integer::Int64(v) => Some(T::from(*v)),
            Integer::Int128(v) => Some(T::from(*v)),
            Integer::IntVar(v) => Some(T::from(v.clone())),
        }
    }

    fn get_size(&self) -> BitSize {
        match self {
            Integer::Int8(_) => BitSize::Bit8,
            Integer::Int16(_) => BitSize::Bit16,
            Integer::Int32(_) => BitSize::Bit32,
            Integer::Int64(_) => BitSize::Bit64,
            Integer::Int128(_) => BitSize::Bit128,
            Integer::IntVar(_) => BitSize::BitVar,
        }
    }
}

impl NumberCasting<Integer> for Integer {
    /// Upcasts the value to the given reference size
    /// # Arguments
    /// * `to_size` - The reference type size to cast to
    /// # Note
    /// This function does not check if the reference size is smaller or equal to the current size
    /// This is the responsibility of the caller
    fn upcast(&self, to_size: BitSize) -> Integer {
        match (self, to_size) {
            (Integer::Int8(v), BitSize::Bit16) => Integer::Int16(*v as i16),
            (Integer::Int8(v), BitSize::Bit32) => Integer::Int32(*v as i32),
            (Integer::Int8(v), BitSize::Bit64) => Integer::Int64(*v as i64),
            (Integer::Int8(v), BitSize::Bit128) => Integer::Int128(*v as i128),
            (Integer::Int8(v), BitSize::BitVar) => Integer::IntVar(BigInt::from(*v)),
            (Integer::Int16(v), BitSize::Bit32) => Integer::Int32(*v as i32),
            (Integer::Int16(v), BitSize::Bit64) => Integer::Int64(*v as i64),
            (Integer::Int16(v), BitSize::Bit128) => Integer::Int128(*v as i128),
            (Integer::Int16(v), BitSize::BitVar) => Integer::IntVar(BigInt::from(*v)),
            (Integer::Int32(v), BitSize::Bit64) => Integer::Int64(*v as i64),
            (Integer::Int32(v), BitSize::Bit128) => Integer::Int128(*v as i128),
            (Integer::Int32(v), BitSize::BitVar) => Integer::IntVar(BigInt::from(*v)),
            (Integer::Int64(v), BitSize::Bit128) => Integer::Int128(*v as i128),
            (Integer::Int64(v), BitSize::BitVar) => Integer::IntVar(BigInt::from(*v)),
            (Integer::Int128(v), BitSize::BitVar) => Integer::IntVar(BigInt::from(*v)),
            _ => unreachable!(),
        }
    }

    fn optimize(self) -> Integer {
        match self {
            Integer::Int8(_) => self,
            Integer::Int16(v) => {
                if v >= i8::MIN as i16 && v <= i8::MAX as i16 {
                    Integer::Int8(v as i8)
                } else {
                    self
                }
            }
            Integer::Int32(v) => {
                if v >= i8::MIN as i32 && v <= i8::MAX as i32 {
                    Integer::Int8(v as i8)
                } else if v >= i16::MIN as i32 && v <= i16::MAX as i32 {
                    Integer::Int16(v as i16)
                } else {
                    self
                }
            }
            Integer::Int64(v) => {
                if v >= i8::MIN as i64 && v <= i8::MAX as i64 {
                    Integer::Int8(v as i8)
                } else if v >= i16::MIN as i64 && v <= i16::MAX as i64 {
                    Integer::Int16(v as i16)
                } else if v >= i32::MIN as i64 && v <= i32::MAX as i64 {
                    Integer::Int32(v as i32)
                } else {
                    self
                }
            }
            Integer::Int128(v) => {
                if v >= i8::MIN as i128 && v <= i8::MAX as i128 {
                    Integer::Int8(v as i8)
                } else if v >= i16::MIN as i128 && v <= i16::MAX as i128 {
                    Integer::Int16(v as i16)
                } else if v >= i32::MIN as i128 && v <= i32::MAX as i128 {
                    Integer::Int32(v as i32)
                } else if v >= i64::MIN as i128 && v <= i64::MAX as i128 {
                    Integer::Int64(v as i64)
                } else {
                    self
                }
            }
            Integer::IntVar(ref v) => {
                if v.ge(&BigInt::from(i8::MIN)) && v.le(&BigInt::from(i8::MAX)) {
                    Integer::Int8(v.iter_u32_digits().nth(0).unwrap() as i8)
                } else if v.ge(&BigInt::from(i16::MIN)) && v.le(&BigInt::from(i16::MAX)) {
                    Integer::Int16(v.iter_u32_digits().nth(0).unwrap() as i16)
                } else if v.ge(&BigInt::from(i32::MIN)) && v.le(&BigInt::from(i32::MAX)) {
                    Integer::Int32(v.iter_u32_digits().nth(0).unwrap() as i32)
                } else if v.ge(&BigInt::from(i64::MIN)) && v.le(&BigInt::from(i64::MAX)) {
                    Integer::Int64(v.iter_u32_digits().nth(0).unwrap() as i64)
                } else {
                    self
                }
            }
        }
    }
}

impl Integer {
    pub fn to_float(&self) -> FloatingPoint {
        match self {
            Integer::Int8(v) => FloatingPoint::Float32(*v as f32),
            Integer::Int16(v) => FloatingPoint::Float32(*v as f32),
            Integer::Int32(v) => FloatingPoint::Float32(*v as f32),
            Integer::Int64(v) => FloatingPoint::Float64(*v as f64),
            Integer::Int128(v) => FloatingPoint::Float64(*v as f64),
            Integer::IntVar(v) => {
                let mut f = BigFloat::new();
                for d in v.iter_u32_digits().rev() {
                    f = f.mul(&BigFloat::from(2u32.pow(32))).add(&BigFloat::from(d));
                }
                FloatingPoint::FloatBig(f)
            }
        }
    }
}

impl ArithmeticOperations<Integer> for Integer {
    fn add(lhs: &Integer, rhs: &Integer) -> Integer {
        match lhs.get_size().cmp(&rhs.get_size()) {
            // If lhs is greater than rhs, upcast rhs to the size of lhs
            Ordering::Greater => Integer::add(lhs, &rhs.upcast(lhs.get_size())),
            // If lhs is less than rhs, upcast lhs to the size of rhs
            Ordering::Less => Integer::add(&lhs.upcast(rhs.get_size()), rhs),
            // If lhs is equal to rhs, perform the addition
            Ordering::Equal => match (lhs, rhs) {
                (Integer::Int8(lhs), Integer::Int8(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        Integer::Int8(res)
                    } else {
                        Integer::Int16(i16::from(*lhs) + i16::from(*rhs))
                    }
                }
                (Integer::Int16(lhs), Integer::Int16(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        Integer::Int16(res)
                    } else {
                        Integer::Int32(i32::from(*lhs) + i32::from(*rhs))
                    }
                }
                (Integer::Int32(lhs), Integer::Int32(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        Integer::Int32(res)
                    } else {
                        Integer::IntVar(BigInt::from(*lhs) + BigInt::from(*rhs))
                    }
                }
                (Integer::Int64(lhs), Integer::Int64(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        Integer::Int64(res)
                    } else {
                        Integer::IntVar(BigInt::from(*lhs) + BigInt::from(*rhs))
                    }
                }
                (Integer::Int128(lhs), Integer::Int128(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        Integer::Int128(res)
                    } else {
                        Integer::IntVar(BigInt::from(*lhs) + BigInt::from(*rhs))
                    }
                }
                (Integer::IntVar(lhs), Integer::IntVar(rhs)) => Integer::IntVar(lhs + rhs),
                _ => panic!("Cannot add signed integers of different sizes"),
            },
        }
    }

    fn sub(_lhs: &Integer, _rhs: &Integer) -> Integer {
        todo!()
    }

    fn mul(_lhs: &Integer, _rhs: &Integer) -> Integer {
        todo!()
    }

    fn div(_lhs: &Integer, _rhs: &Integer) -> Integer {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FloatingPoint {
    Float32(f32),
    Float64(f64),
    FloatBig(BigFloat), // Increased precision floating point numbers (fixed but large size)
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

impl NumberMethods<f32, f32, f32, f64, f64, BigFloat> for FloatingPoint {
    fn get_number_value<T>(&self) -> Option<T>
    where
        T: From<f32> + From<f64> + From<BigFloat>,
    {
        match self {
            FloatingPoint::Float32(v) => Some(T::from(*v)),
            FloatingPoint::Float64(v) => Some(T::from(*v)),
            FloatingPoint::FloatBig(v) => Some(T::from(*v)),
        }
    }

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
                FloatingPoint::FloatBig(BigFloat::from(*v))
            }
            (FloatingPoint::Float64(v), BitSize::BitVar) => {
                FloatingPoint::FloatBig(BigFloat::from(*v))
            }
            _ => unreachable!(),
        }
    }

    fn optimize(self) -> FloatingPoint {
        match self {
            FloatingPoint::Float32(_) => self,
            FloatingPoint::Float64(v) => {
                if v >= f32::MIN as f64 && v <= f32::MAX as f64 {
                    FloatingPoint::Float32(v as f32)
                } else {
                    self
                }
            }
            FloatingPoint::FloatBig(v) => {
                // TODO: Look at resolution of BigFloat to f32/f64, not only max/min values
                if v >= BigFloat::from(f32::MIN) && v <= BigFloat::from(f32::MAX) {
                    FloatingPoint::Float32(v.to_f32())
                } else if v >= BigFloat::from(f64::MIN) && v <= BigFloat::from(f64::MAX) {
                    FloatingPoint::Float64(v.to_f64())
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
                    FloatingPoint::FloatBig(lhs.add(rhs))
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
    SignedInteger(Integer),
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

impl Number {
    pub fn get_number_value<T>(&self) -> Option<T>
    where
        T: From<u8>
            + From<u16>
            + From<u32>
            + From<u64>
            + From<u128>
            + From<i8>
            + From<i16>
            + From<i32>
            + From<i64>
            + From<i128>
            + From<f32>
            + From<f64>
            + From<BigFloat>
            + From<BigInt>
            + From<BigUint>,
    {
        match self {
            Number::UnsignedInteger(u) => u.get_number_value(),
            Number::SignedInteger(i) => i.get_number_value(),
            Number::FloatingPoint(f) => f.get_number_value(),
        }
    }

    fn parse_big_int(s: String) -> Number {
        let i = BigInt::parse_bytes(s.as_bytes(), 10).unwrap();
        Number::SignedInteger(Integer::IntVar(i))
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
        if s.contains('.') {
            // Floating point number (Only signed floating point numbers are supported)
            let f = s.parse::<f64>();
            if f.is_err() {
                return Number::parse_big_float(s);
            }
            let f = f.unwrap();
            if f >= f32::MIN as f64 && f <= f32::MAX as f64 {
                Number::FloatingPoint(FloatingPoint::Float32(f as f32))
            } else {
                Number::FloatingPoint(FloatingPoint::Float64(f))
            }
        } else if let Some(s) = s.strip_prefix('-') {
            let i = s.parse::<i128>();
            if i.is_err() {
                return Number::parse_big_int(s.to_string());
            }
            let i = i.unwrap();
            if i >= i8::MIN as i128 && i <= i8::MAX as i128 {
                Number::SignedInteger(Integer::Int8(i as i8))
            } else if i >= i16::MIN as i128 && i <= i16::MAX as i128 {
                Number::SignedInteger(Integer::Int16(i as i16))
            } else if i >= i32::MIN as i128 && i <= i32::MAX as i128 {
                Number::SignedInteger(Integer::Int32(i as i32))
            } else if i >= i64::MIN as i128 && i <= i64::MAX as i128 {
                Number::SignedInteger(Integer::Int64(i as i64))
            } else {
                Number::SignedInteger(Integer::Int128(i))
            }
        } else {
            let u = s.parse::<u128>();
            if u.is_err() {
                return Number::parse_big_uint(s);
            }
            let u = u.unwrap();
            if u <= 1 {
                Number::UnsignedInteger(UnsignedInteger::UInt1(u as u8))
            } else if u >= u8::MIN as u128 && u <= u8::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt8(u as u8))
            } else if u >= u16::MIN as u128 && u <= u16::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt16(u as u16))
            } else if u >= u32::MIN as u128 && u <= u32::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt32(u as u32))
            } else if u >= u64::MIN as u128 && u <= u64::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt64(u as u64))
            } else {
                Number::UnsignedInteger(UnsignedInteger::UInt128(u))
            }
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
                Number::SignedInteger(Integer::add(lhs, rhs))
            }
            (Number::FloatingPoint(lhs), Number::FloatingPoint(rhs)) => {
                Number::FloatingPoint(FloatingPoint::add(lhs, rhs))
            }
            (Number::UnsignedInteger(lhs), Number::SignedInteger(rhs)) => {
                Number::SignedInteger(Integer::add(&lhs.to_signed(), rhs))
            }
            (Number::SignedInteger(lhs), Number::UnsignedInteger(rhs)) => {
                Number::SignedInteger(Integer::add(lhs, &rhs.to_signed()))
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
                Integer::Int8(b) => write!(f, "{}", b),
                Integer::Int16(s) => write!(f, "{}", s),
                Integer::Int32(i) => write!(f, "{}", i),
                Integer::Int64(i) => write!(f, "{}", i),
                Integer::Int128(i) => write!(f, "{}", i),
                Integer::IntVar(i) => write!(f, "{}", i),
            },
            Number::FloatingPoint(fl) => match fl {
                FloatingPoint::Float32(fl) => write!(f, "{}", fl),
                FloatingPoint::Float64(fl) => write!(f, "{}", fl),
                FloatingPoint::FloatBig(fl) => write!(f, "{}", fl),
            },
        }
    }
}
