use std::{cmp::Ordering, collections::HashMap, fmt::Display, ops::Add};

use num_bigfloat::BigFloat;
use num_bigint::{BigInt, BigUint};

use crate::{
    parser::ast::Ast,
    type_checker::types::{std_primitive_types, CheckedType, FunctionParameterType, GetType, Type},
};

use super::interpreter::InterpretResult;

pub trait NumberCasting<T> {
    fn up_cast_type(&self, to_reference_size: &T) -> T;
    fn try_down_cast_ref(&self, to_reference_size: &T) -> Option<T>;
    fn cast_to_ref(&self, to_reference_size: &T) -> Option<T>;
    fn down_cast_max(&self) -> T;
}

pub trait ArithmeticOperations<T> {
    fn add(lhs: &T, rhs: &T) -> T;
    fn sub(lhs: &T, rhs: &T) -> T;
    fn mul(lhs: &T, rhs: &T) -> T;
    fn div(lhs: &T, rhs: &T) -> T;
}

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

impl UnsignedInteger {
    fn get_size_order(&self) -> u8 {
        match self {
            UnsignedInteger::UInt1(_) => 0,
            UnsignedInteger::UInt8(_) => 1,
            UnsignedInteger::UInt16(_) => 2,
            UnsignedInteger::UInt32(_) => 3,
            UnsignedInteger::UInt64(_) => 4,
            UnsignedInteger::UInt128(_) => 5,
            UnsignedInteger::UIntVar(_) => 6,
        }
    }

    fn is_biguint(&self) -> bool {
        match self {
            UnsignedInteger::UIntVar(_) => true,
            _ => false,
        }
    }

    fn get_u128_value(&self) -> Option<u128> {
        Some(match self {
            UnsignedInteger::UInt1(v) => *v as u128,
            UnsignedInteger::UInt8(v) => *v as u128,
            UnsignedInteger::UInt16(v) => *v as u128,
            UnsignedInteger::UInt32(v) => *v as u128,
            UnsignedInteger::UInt64(v) => *v as u128,
            UnsignedInteger::UInt128(v) => *v,
            UnsignedInteger::UIntVar(_v) => return None,
        })
    }

    fn get_biguint_value(&self) -> Option<BigUint> {
        Some(match self {
            UnsignedInteger::UInt1(v) => BigUint::from(*v),
            UnsignedInteger::UInt8(v) => BigUint::from(*v),
            UnsignedInteger::UInt16(v) => BigUint::from(*v),
            UnsignedInteger::UInt32(v) => BigUint::from(*v),
            UnsignedInteger::UInt64(v) => BigUint::from(*v),
            UnsignedInteger::UInt128(v) => BigUint::from(*v),
            UnsignedInteger::UIntVar(v) => v.clone(),
        })
    }
}

impl NumberCasting<UnsignedInteger> for UnsignedInteger {
    /// Upcasts the value to the given reference size
    /// # Arguments
    /// * `to_reference_size` - The reference type size to cast to
    /// # Note
    /// This function does not check if the reference size is smaller or equal to the current size
    /// This is the responsibility of the caller
    fn up_cast_type(&self, to_reference_size: &UnsignedInteger) -> UnsignedInteger {
        match (self, to_reference_size) {
            (UnsignedInteger::UInt1(v), UnsignedInteger::UInt8(_)) => {
                UnsignedInteger::UInt8(*v)
            }
            (UnsignedInteger::UInt1(v), UnsignedInteger::UInt16(_)) => {
                UnsignedInteger::UInt16(*v as u16)
            }
            (UnsignedInteger::UInt1(v), UnsignedInteger::UInt32(_)) => {
                UnsignedInteger::UInt32(*v as u32)
            }
            (UnsignedInteger::UInt1(v), UnsignedInteger::UInt64(_)) => {
                UnsignedInteger::UInt64(*v as u64)
            }
            (UnsignedInteger::UInt1(v), UnsignedInteger::UInt128(_)) => {
                UnsignedInteger::UInt128(*v as u128)
            }
            (UnsignedInteger::UInt1(v), UnsignedInteger::UIntVar(_)) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt8(v), UnsignedInteger::UInt16(_)) => {
                UnsignedInteger::UInt16(*v as u16)
            }
            (UnsignedInteger::UInt8(v), UnsignedInteger::UInt32(_)) => {
                UnsignedInteger::UInt32(*v as u32)
            }
            (UnsignedInteger::UInt8(v), UnsignedInteger::UInt64(_)) => {
                UnsignedInteger::UInt64(*v as u64)
            }
            (UnsignedInteger::UInt8(v), UnsignedInteger::UInt128(_)) => {
                UnsignedInteger::UInt128(*v as u128)
            }
            (UnsignedInteger::UInt8(v), UnsignedInteger::UIntVar(_)) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt16(v), UnsignedInteger::UInt32(_)) => {
                UnsignedInteger::UInt32(*v as u32)
            }
            (UnsignedInteger::UInt16(v), UnsignedInteger::UInt64(_)) => {
                UnsignedInteger::UInt64(*v as u64)
            }
            (UnsignedInteger::UInt16(v), UnsignedInteger::UInt128(_)) => {
                UnsignedInteger::UInt128(*v as u128)
            }
            (UnsignedInteger::UInt16(v), UnsignedInteger::UIntVar(_)) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt32(v), UnsignedInteger::UInt64(_)) => {
                UnsignedInteger::UInt64(*v as u64)
            }
            (UnsignedInteger::UInt32(v), UnsignedInteger::UInt128(_)) => {
                UnsignedInteger::UInt128(*v as u128)
            }
            (UnsignedInteger::UInt32(v), UnsignedInteger::UIntVar(_)) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt64(v), UnsignedInteger::UInt128(_)) => {
                UnsignedInteger::UInt128(*v as u128)
            }
            (UnsignedInteger::UInt64(v), UnsignedInteger::UIntVar(_)) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            (UnsignedInteger::UInt128(v), UnsignedInteger::UIntVar(_)) => {
                UnsignedInteger::UIntVar(BigUint::from(*v))
            }
            _ => panic!("Cannot upcast type {:?} to {:?}", self, to_reference_size), // Unreachable
        }
    }

    /// Try to downcast the type of the UnsignedInteger to the type of the reference.
    /// Returns None if the downcast is not possible (value is too large to fit in the reference type)
    /// Returns Some(UnsignedInteger) if the downcast is possible
    /// # Arguments
    /// * `to_reference_size` - The reference type size to cast to
    /// # Note
    /// This function does not check if the reference type is smaller or equal to the current type.
    /// This is the responsibility of the caller.
    fn try_down_cast_ref(&self, to_reference_size: &UnsignedInteger) -> Option<UnsignedInteger> {
        match (self, to_reference_size) {
            (UnsignedInteger::UInt8(v), UnsignedInteger::UInt1(_)) => {
                if *v <= 1 {
                    Some(UnsignedInteger::UInt1(*v))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt16(v), UnsignedInteger::UInt1(_)) => {
                if *v <= 1 {
                    Some(UnsignedInteger::UInt1(*v as u8))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt32(v), UnsignedInteger::UInt1(_)) => {
                if *v <= 1 {
                    Some(UnsignedInteger::UInt1(*v as u8))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt64(v), UnsignedInteger::UInt1(_)) => {
                if *v <= 1 {
                    Some(UnsignedInteger::UInt1(*v as u8))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt128(v), UnsignedInteger::UInt1(_)) => {
                if *v <= 1 {
                    Some(UnsignedInteger::UInt1(*v as u8))
                } else {
                    None
                }
            }
            (UnsignedInteger::UIntVar(v), UnsignedInteger::UInt1(_)) => {
                if v.le(&BigUint::from(1u8)) {
                    Some(UnsignedInteger::UInt1(
                        v.iter_u32_digits().nth(0).unwrap() as u8
                    ))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt16(v), UnsignedInteger::UInt8(_)) => {
                if *v <= u8::MAX as u16 {
                    Some(UnsignedInteger::UInt8(*v as u8))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt32(v), UnsignedInteger::UInt8(_)) => {
                if *v <= u8::MAX as u32 {
                    Some(UnsignedInteger::UInt8(*v as u8))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt64(v), UnsignedInteger::UInt8(_)) => {
                if *v <= u8::MAX as u64 {
                    Some(UnsignedInteger::UInt8(*v as u8))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt128(v), UnsignedInteger::UInt8(_)) => {
                if *v <= u8::MAX as u128 {
                    Some(UnsignedInteger::UInt8(*v as u8))
                } else {
                    None
                }
            }
            (UnsignedInteger::UIntVar(v), UnsignedInteger::UInt8(_)) => {
                if v.le(&BigUint::from(u8::MAX)) {
                    Some(UnsignedInteger::UInt8(
                        v.iter_u32_digits().nth(0).unwrap() as u8
                    ))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt32(v), UnsignedInteger::UInt16(_)) => {
                if *v <= u16::MAX as u32 {
                    Some(UnsignedInteger::UInt16(*v as u16))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt64(v), UnsignedInteger::UInt16(_)) => {
                if *v <= u16::MAX as u64 {
                    Some(UnsignedInteger::UInt16(*v as u16))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt128(v), UnsignedInteger::UInt16(_)) => {
                if *v <= u16::MAX as u128 {
                    Some(UnsignedInteger::UInt16(*v as u16))
                } else {
                    None
                }
            }
            (UnsignedInteger::UIntVar(v), UnsignedInteger::UInt16(_)) => {
                if v.le(&BigUint::from(u16::MAX)) {
                    Some(UnsignedInteger::UInt16(
                        v.iter_u32_digits().nth(0).unwrap() as u16
                    ))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt64(v), UnsignedInteger::UInt32(_)) => {
                if *v <= u32::MAX as u64 {
                    Some(UnsignedInteger::UInt32(*v as u32))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt128(v), UnsignedInteger::UInt32(_)) => {
                if *v <= u32::MAX as u128 {
                    Some(UnsignedInteger::UInt32(*v as u32))
                } else {
                    None
                }
            }
            (UnsignedInteger::UIntVar(v), UnsignedInteger::UInt32(_)) => {
                if v.le(&BigUint::from(u32::MAX)) {
                    Some(UnsignedInteger::UInt32(
                        v.iter_u32_digits().nth(0).unwrap()
                    ))
                } else {
                    None
                }
            }
            (UnsignedInteger::UInt128(v), UnsignedInteger::UInt64(_)) => {
                if *v <= u64::MAX as u128 {
                    Some(UnsignedInteger::UInt64(*v as u64))
                } else {
                    None
                }
            }
            (UnsignedInteger::UIntVar(v), UnsignedInteger::UInt64(_)) => {
                if v.le(&BigUint::from(u64::MAX)) {
                    Some(UnsignedInteger::UInt64(
                        v.iter_u32_digits().nth(0).unwrap() as u64
                    ))
                } else {
                    None
                }
            }
            (UnsignedInteger::UIntVar(v), UnsignedInteger::UInt128(_)) => {
                if v.le(&BigUint::from(u128::MAX)) {
                    Some(UnsignedInteger::UInt128(
                        v.iter_u32_digits().nth(0).unwrap() as u128,
                    ))
                } else {
                    None
                }
            }
            _ => panic!("Cannot downcast type {:?} to {:?}", self, to_reference_size),
        }
    }

    /// Casts the current type to the reference type, if possible.
    /// If the current type is smaller than the reference type, it will be upcasted.
    /// If the current type is larger than the reference type, it will be downcasted.
    /// If the current type is equal to the reference type, no casting will be performed.
    /// If the current type is larger than the reference type and cannot be downcasted, None will be returned.
    /// # Arguments
    /// * `to_reference_size` - The reference type size to cast to
    /// # Returns
    /// The casted type, if possible
    /// # Panics
    /// If the current type is larger than the reference type and cannot be downcasted
    fn cast_to_ref(&self, to_reference_size: &UnsignedInteger) -> Option<UnsignedInteger> {
        let cmp_res = self
            .get_size_order()
            .cmp(&to_reference_size.get_size_order());
        if cmp_res == Ordering::Equal {
            Some(self.clone())
        }
        // No casting needed
        // self < reference, upcast to the reference size
        else if cmp_res == Ordering::Less {
            Some(self.up_cast_type(to_reference_size))
        }
        // self > reference, try to downcast to the reference size
        else {
            self.try_down_cast_ref(to_reference_size)
        }
    }

    /// Returns the smallest possible type that can hold the current value without loss of precision or overflow.
    fn down_cast_max(&self) -> UnsignedInteger {
        match self {
            UnsignedInteger::UInt1(_) => self.clone(),
            UnsignedInteger::UInt8(v) => {
                if *v <= 1 {
                    UnsignedInteger::UInt1(*v)
                } else {
                    self.clone()
                }
            }
            UnsignedInteger::UInt16(v) => {
                if *v <= 1 {
                    UnsignedInteger::UInt1(*v as u8)
                } else if *v <= u8::MAX as u16 {
                    UnsignedInteger::UInt8(*v as u8)
                } else {
                    self.clone()
                }
            }
            UnsignedInteger::UInt32(v) => {
                if *v <= 1 {
                    UnsignedInteger::UInt1(*v as u8)
                } else if *v <= u8::MAX as u32 {
                    UnsignedInteger::UInt8(*v as u8)
                } else if *v <= u16::MAX as u32 {
                    UnsignedInteger::UInt16(*v as u16)
                } else {
                    self.clone()
                }
            }
            UnsignedInteger::UInt64(v) => {
                if *v <= 1 {
                    UnsignedInteger::UInt1(*v as u8)
                } else if *v <= u8::MAX as u64 {
                    UnsignedInteger::UInt8(*v as u8)
                } else if *v <= u16::MAX as u64 {
                    UnsignedInteger::UInt16(*v as u16)
                } else if *v <= u32::MAX as u64 {
                    UnsignedInteger::UInt32(*v as u32)
                } else {
                    self.clone()
                }
            }
            UnsignedInteger::UInt128(v) => {
                if *v <= 1 {
                    UnsignedInteger::UInt1(*v as u8)
                } else if *v <= u8::MAX as u128 {
                    UnsignedInteger::UInt8(*v as u8)
                } else if *v <= u16::MAX as u128 {
                    UnsignedInteger::UInt16(*v as u16)
                } else if *v <= u32::MAX as u128 {
                    UnsignedInteger::UInt32(*v as u32)
                } else if *v <= u64::MAX as u128 {
                    UnsignedInteger::UInt64(*v as u64)
                } else {
                    self.clone()
                }
            }
            UnsignedInteger::UIntVar(v) => {
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
                    self.clone()
                }
            }
        }
    }
}

impl ArithmeticOperations<UnsignedInteger> for UnsignedInteger {
    fn add(lhs: &UnsignedInteger, rhs: &UnsignedInteger) -> UnsignedInteger {
        // Always perform arithmetic operations on u128, then cast back to the smallest possible type
        if lhs.is_biguint() || rhs.is_biguint() {
            let lhs = lhs.get_biguint_value().unwrap();
            let rhs = rhs.get_biguint_value().unwrap();
            UnsignedInteger::UIntVar(BigUint::add(lhs, rhs))
        } else {
            let lhs = lhs.get_u128_value().unwrap();
            let rhs = rhs.get_u128_value().unwrap();
            if let Some(res) = lhs.checked_add(rhs) {
                UnsignedInteger::UInt128(res).down_cast_max()
            } else {
                let lhs = BigUint::from(lhs);
                let rhs = BigUint::from(rhs);
                UnsignedInteger::UIntVar(BigUint::add(lhs, rhs))
            }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignedInteger {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    IntVar(BigInt), // Arbitrary sized integers (variable size)
}

impl ArithmeticOperations<SignedInteger> for SignedInteger {
    fn add(_lhs: &SignedInteger, _rhs: &SignedInteger) -> SignedInteger {
        todo!()
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
    FloatBig(BigFloat), // Increased precision floating point numbers (fixed but large size)
                        // Look at https://github.com/stencillogic/astro-float for handling Arbitrary precision floating point numbers (variable size)
}

impl ArithmeticOperations<FloatingPoint> for FloatingPoint {
    fn add(_lhs: &FloatingPoint, _rhs: &FloatingPoint) -> FloatingPoint {
        todo!()
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
    fn get_type(&self) -> CheckedType {
        CheckedType::Checked(match self {
            Number::UnsignedInteger(u) => match u {
                UnsignedInteger::UInt1(_) => std_primitive_types::UINT1,
                UnsignedInteger::UInt8(_) => std_primitive_types::UINT8,
                UnsignedInteger::UInt16(_) => std_primitive_types::UINT16,
                UnsignedInteger::UInt32(_) => std_primitive_types::UINT32,
                UnsignedInteger::UInt64(_) => std_primitive_types::UINT64,
                UnsignedInteger::UInt128(_) => std_primitive_types::UINT128,
                UnsignedInteger::UIntVar(_) => std_primitive_types::UINTBIG,
            },
            Number::SignedInteger(i) => match i {
                SignedInteger::Int8(_) => std_primitive_types::INT8,
                SignedInteger::Int16(_) => std_primitive_types::INT16,
                SignedInteger::Int32(_) => std_primitive_types::INT32,
                SignedInteger::Int64(_) => std_primitive_types::INT64,
                SignedInteger::Int128(_) => std_primitive_types::INT128,
                SignedInteger::IntVar(_) => std_primitive_types::INTBIG,
            },
            Number::FloatingPoint(f) => match f {
                FloatingPoint::Float32(_) => std_primitive_types::FLOAT32,
                FloatingPoint::Float64(_) => std_primitive_types::FLOAT64,
                FloatingPoint::FloatBig(_) => std_primitive_types::FLOATBIG,
            },
        })
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
        if s.contains('.') {
            // Floating point number (Only signed floating point numbers are supported)
            let f = s.parse::<f64>();
            if f.is_err() {
                return Number::parse_big_float(s);
            }
            let f = f.unwrap();
            if f >= std::f32::MIN as f64 && f <= std::f32::MAX as f64 {
                Number::FloatingPoint(FloatingPoint::Float32(f as f32))
            } else {
                Number::FloatingPoint(FloatingPoint::Float64(f))
            }
        } else if s.starts_with('-') {
            let i = s[1..].parse::<i128>();
            if i.is_err() {
                return Number::parse_big_int(s);
            }
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
                Number::SignedInteger(SignedInteger::Int128(i))
            }
        } else {
            let u = s.parse::<u128>();
            if u.is_err() {
                return Number::parse_big_uint(s);
            }
            let u = u.unwrap();
            if u >= std::u8::MIN as u128 && u <= std::u8::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt8(u as u8))
            } else if u >= std::u16::MIN as u128 && u <= std::u16::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt16(u as u16))
            } else if u >= std::u32::MIN as u128 && u <= std::u32::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt32(u as u32))
            } else if u >= std::u64::MIN as u128 && u <= std::u64::MAX as u128 {
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
            (Number::SignedInteger(_lhs), Number::SignedInteger(_rhs)) => todo!(),
            (Number::FloatingPoint(_lhs), Number::FloatingPoint(_rhs)) => todo!(),
            _ => todo!(),
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
 * Is the value representation of `FunctionParameterType`.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum NativeFunctionParameters {
    Singles(Vec<Value>),
    Variadic(Vec<Value>, Vec<Value>), // Some initial values of different types, followed by the variadic type values
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionVariation {
    User(FunctionParameterType, Ast, Type),
    Native(
        fn(NativeFunctionParameters) -> InterpretResult,
        FunctionParameterType,
        Type,
    ), // Built-in functions
}

/**
 * Compares two `FunctionVariation`s by their FunctionParameterType.
 * Used as compare function in the sort_by function.
 * This function will sort single functions before variadic functions.
 */
pub fn compare_function_variations(a: &FunctionVariation, b: &FunctionVariation) -> Ordering {
    match (a.get_params(), b.get_params()) {
        (FunctionParameterType::Singles(_), FunctionParameterType::Variadic(_, _)) => {
            Ordering::Less
        }
        (FunctionParameterType::Variadic(_, _), FunctionParameterType::Singles(_)) => {
            Ordering::Greater
        }
        _ => Ordering::Equal,
    }
}

impl FunctionVariation {
    pub fn get_params(&self) -> &FunctionParameterType {
        match self {
            FunctionVariation::User(p, _, _) => p,
            FunctionVariation::Native(_, p, _) => p,
        }
    }

    pub fn get_return_type(&self) -> &Type {
        match self {
            FunctionVariation::User(_, _, r) => r,
            FunctionVariation::Native(_, _, r) => r,
        }
    }
}

impl GetType for FunctionVariation {
    fn get_type(&self) -> CheckedType {
        CheckedType::Checked(match self {
            FunctionVariation::User(p, _, r) => {
                Type::Function(Box::new(p.clone()), Box::new(r.clone()))
            }
            FunctionVariation::Native(_, v, r) => {
                Type::Function(Box::new(v.clone()), Box::new(r.clone()))
            }
        })
    }
}

impl Display for FunctionVariation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let ret_type = match self {
            FunctionVariation::User(p, _, r) => {
                p.fmt(f)?;
                r
            }
            FunctionVariation::Native(_, v, r) => {
                v.fmt(f)?;
                r
            }
        };
        write!(f, ") -> {}", ret_type) // TODO: Make this a unicode arrow
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub variations: Vec<FunctionVariation>, // Function types are inferred from variations
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
    fn get_type(&self) -> CheckedType {
        CheckedType::Checked(match self {
            Value::Unit => Type::Unit,
            Value::Number(n) => match n {
                Number::UnsignedInteger(u) => match u {
                    UnsignedInteger::UInt1(_) => std_primitive_types::UINT1,
                    UnsignedInteger::UInt8(_) => std_primitive_types::UINT8,
                    UnsignedInteger::UInt16(_) => std_primitive_types::UINT16,
                    UnsignedInteger::UInt32(_) => std_primitive_types::UINT32,
                    UnsignedInteger::UInt64(_) => std_primitive_types::UINT64,
                    UnsignedInteger::UInt128(_) => std_primitive_types::UINT128,
                    UnsignedInteger::UIntVar(_) => std_primitive_types::UINTBIG,
                },
                Number::SignedInteger(i) => match i {
                    SignedInteger::Int8(_) => std_primitive_types::INT8,
                    SignedInteger::Int16(_) => std_primitive_types::INT16,
                    SignedInteger::Int32(_) => std_primitive_types::INT32,
                    SignedInteger::Int64(_) => std_primitive_types::INT64,
                    SignedInteger::Int128(_) => std_primitive_types::INT128,
                    SignedInteger::IntVar(_) => std_primitive_types::INTBIG,
                },
                Number::FloatingPoint(f) => match f {
                    FloatingPoint::Float32(_) => std_primitive_types::FLOAT32,
                    FloatingPoint::Float64(_) => std_primitive_types::FLOAT64,
                    FloatingPoint::FloatBig(_) => std_primitive_types::FLOATBIG,
                },
            },
            Value::String(_) => std_primitive_types::STRING,
            Value::Char(_) => std_primitive_types::CHAR,
            Value::Boolean(_) => std_primitive_types::BOOL,
            Value::Tuple(_, t) => t.clone(),
            Value::List(_, t) => t.clone(),
            Value::Record(_, t) => t.clone(),
            Value::Function(_) => panic!("Cannot get type of functions"), // Because functions can have multiple types
        })
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
                writeln!(f, "function[{}] {{", fun.name)?;
                for (_, v) in fun.variations.iter().enumerate() {
                    writeln!(f, "\t{}", v)?;
                }
                write!(f, "}}")
            }
        }
    }
}
