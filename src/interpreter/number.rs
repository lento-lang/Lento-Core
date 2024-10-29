use num_bigfloat::BigFloat;
use num_bigint::{BigInt, BigUint};

use std::{
    cmp::Ordering,
    fmt::{Display, Formatter},
};

use crate::type_checker::types::{std_primitive_types, CheckedType, GetType};

pub trait NumberCasting<T> {
    /// Upcasts the value to the given reference size
    fn upcast(&self, to_size: BitSize) -> T;
    // fn try_downcast(&self, to_size: BitSize) -> Option<T>;
    // fn try_cast(&self, to_size: BitSize) -> Option<T>;
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
    fn get_type(&self) -> CheckedType {
        CheckedType::Checked(match self {
            UnsignedInteger::UInt1(_) => std_primitive_types::UINT1,
            UnsignedInteger::UInt8(_) => std_primitive_types::UINT8,
            UnsignedInteger::UInt16(_) => std_primitive_types::UINT16,
            UnsignedInteger::UInt32(_) => std_primitive_types::UINT32,
            UnsignedInteger::UInt64(_) => std_primitive_types::UINT64,
            UnsignedInteger::UInt128(_) => std_primitive_types::UINT128,
            UnsignedInteger::UIntVar(_) => std_primitive_types::UINTBIG,
        })
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

    /// Try to downcast the type of the UnsignedInteger to the type of the reference.
    /// Returns None if the downcast is not possible (value is too large to fit in the reference type)
    /// Returns Some(UnsignedInteger) if the downcast is possible
    /// # Arguments
    /// * `to_size` - The reference type size to cast to
    /// # Note
    /// This function does not check if the reference type is smaller or equal to the current type.
    /// This is the responsibility of the caller.
    // fn try_downcast(&self, to_size: BitSize) -> Option<UnsignedInteger> {
    //     match (self, to_size) {
    //         (UnsignedInteger::UInt8(v), BitSize::Bit1) => {
    //             if *v <= 1 {
    //                 Some(UnsignedInteger::UInt1(*v))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt16(v), BitSize::Bit1) => {
    //             if *v <= 1 {
    //                 Some(UnsignedInteger::UInt1(*v as u8))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt32(v), BitSize::Bit1) => {
    //             if *v <= 1 {
    //                 Some(UnsignedInteger::UInt1(*v as u8))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt64(v), BitSize::Bit1) => {
    //             if *v <= 1 {
    //                 Some(UnsignedInteger::UInt1(*v as u8))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt128(v), BitSize::Bit1) => {
    //             if *v <= 1 {
    //                 Some(UnsignedInteger::UInt1(*v as u8))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UIntVar(v), BitSize::Bit1) => {
    //             if v.le(&BigUint::from(1u8)) {
    //                 Some(UnsignedInteger::UInt1(
    //                     v.iter_u32_digits().nth(0).unwrap() as u8
    //                 ))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt16(v), BitSize::Bit8) => {
    //             if *v <= u8::MAX as u16 {
    //                 Some(UnsignedInteger::UInt8(*v as u8))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt32(v), BitSize::Bit8) => {
    //             if *v <= u8::MAX as u32 {
    //                 Some(UnsignedInteger::UInt8(*v as u8))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt64(v), BitSize::Bit8) => {
    //             if *v <= u8::MAX as u64 {
    //                 Some(UnsignedInteger::UInt8(*v as u8))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt128(v), BitSize::Bit8) => {
    //             if *v <= u8::MAX as u128 {
    //                 Some(UnsignedInteger::UInt8(*v as u8))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UIntVar(v), BitSize::Bit8) => {
    //             if v.le(&BigUint::from(u8::MAX)) {
    //                 Some(UnsignedInteger::UInt8(
    //                     v.iter_u32_digits().nth(0).unwrap() as u8
    //                 ))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt32(v), BitSize::Bit16) => {
    //             if *v <= u16::MAX as u32 {
    //                 Some(UnsignedInteger::UInt16(*v as u16))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt64(v), BitSize::Bit16) => {
    //             if *v <= u16::MAX as u64 {
    //                 Some(UnsignedInteger::UInt16(*v as u16))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt128(v), BitSize::Bit16) => {
    //             if *v <= u16::MAX as u128 {
    //                 Some(UnsignedInteger::UInt16(*v as u16))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UIntVar(v), BitSize::Bit16) => {
    //             if v.le(&BigUint::from(u16::MAX)) {
    //                 Some(UnsignedInteger::UInt16(
    //                     v.iter_u32_digits().nth(0).unwrap() as u16
    //                 ))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt64(v), BitSize::Bit32) => {
    //             if *v <= u32::MAX as u64 {
    //                 Some(UnsignedInteger::UInt32(*v as u32))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt128(v), BitSize::Bit32) => {
    //             if *v <= u32::MAX as u128 {
    //                 Some(UnsignedInteger::UInt32(*v as u32))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UIntVar(v), BitSize::Bit32) => {
    //             if v.le(&BigUint::from(u32::MAX)) {
    //                 Some(UnsignedInteger::UInt32(v.iter_u32_digits().nth(0).unwrap()))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UInt128(v), BitSize::Bit64) => {
    //             if *v <= u64::MAX as u128 {
    //                 Some(UnsignedInteger::UInt64(*v as u64))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UIntVar(v), BitSize::Bit64) => {
    //             if v.le(&BigUint::from(u64::MAX)) {
    //                 Some(UnsignedInteger::UInt64(
    //                     v.iter_u32_digits().nth(0).unwrap() as u64
    //                 ))
    //             } else {
    //                 None
    //             }
    //         }
    //         (UnsignedInteger::UIntVar(v), BitSize::Bit128) => {
    //             if v.le(&BigUint::from(u128::MAX)) {
    //                 Some(UnsignedInteger::UInt128(
    //                     v.iter_u32_digits().nth(0).unwrap() as u128,
    //                 ))
    //             } else {
    //                 None
    //             }
    //         }
    //         _ => unreachable!(),
    //     }
    // }

    /// Casts the current type to the reference type, if possible.
    /// If the current type is smaller than the reference type, it will be upcasted.
    /// If the current type is larger than the reference type, it will be downcasted.
    /// If the current type is equal to the reference type, no casting will be performed.
    /// If the current type is larger than the reference type and cannot be downcasted, None will be returned.
    /// # Arguments
    /// * `to_size` - The reference type size to cast to
    /// # Returns
    /// The casted type, if possible
    /// # Panics
    /// If the current type is larger than the reference type and cannot be downcasted
    // fn try_cast(&self, to_size: BitSize) -> Option<UnsignedInteger> {
    //     match self.get_size().cmp(&to_size) {
    //         Ordering::Less => Some(self.upcast(to_size)),
    //         Ordering::Equal => Some(self.clone()),
    //         Ordering::Greater => self.try_downcast(to_size),
    //     }
    // }

    /// Returns the smallest possible type that can hold the current value without loss of precision or overflow.
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignedInteger {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    IntVar(BigInt), // Arbitrary sized integers (variable size)
}

impl GetType for SignedInteger {
    fn get_type(&self) -> CheckedType {
        CheckedType::Checked(match self {
            SignedInteger::Int8(_) => std_primitive_types::INT8,
            SignedInteger::Int16(_) => std_primitive_types::INT16,
            SignedInteger::Int32(_) => std_primitive_types::INT32,
            SignedInteger::Int64(_) => std_primitive_types::INT64,
            SignedInteger::Int128(_) => std_primitive_types::INT128,
            SignedInteger::IntVar(_) => std_primitive_types::INTBIG,
        })
    }
}

impl NumberMethods<i8, i16, i32, i64, i128, BigInt> for SignedInteger {
    fn get_number_value<T>(&self) -> Option<T>
    where
        T: From<i8> + From<i16> + From<i32> + From<i64> + From<i128> + From<BigInt>,
    {
        match self {
            SignedInteger::Int8(v) => Some(T::from(*v)),
            SignedInteger::Int16(v) => Some(T::from(*v)),
            SignedInteger::Int32(v) => Some(T::from(*v)),
            SignedInteger::Int64(v) => Some(T::from(*v)),
            SignedInteger::Int128(v) => Some(T::from(*v)),
            SignedInteger::IntVar(v) => Some(T::from(v.clone())),
        }
    }

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
            (SignedInteger::Int8(v), BitSize::BitVar) => SignedInteger::IntVar(BigInt::from(*v)),
            (SignedInteger::Int16(v), BitSize::Bit32) => SignedInteger::Int32(*v as i32),
            (SignedInteger::Int16(v), BitSize::Bit64) => SignedInteger::Int64(*v as i64),
            (SignedInteger::Int16(v), BitSize::Bit128) => SignedInteger::Int128(*v as i128),
            (SignedInteger::Int16(v), BitSize::BitVar) => SignedInteger::IntVar(BigInt::from(*v)),
            (SignedInteger::Int32(v), BitSize::Bit64) => SignedInteger::Int64(*v as i64),
            (SignedInteger::Int32(v), BitSize::Bit128) => SignedInteger::Int128(*v as i128),
            (SignedInteger::Int32(v), BitSize::BitVar) => SignedInteger::IntVar(BigInt::from(*v)),
            (SignedInteger::Int64(v), BitSize::Bit128) => SignedInteger::Int128(*v as i128),
            (SignedInteger::Int64(v), BitSize::BitVar) => SignedInteger::IntVar(BigInt::from(*v)),
            (SignedInteger::Int128(v), BitSize::BitVar) => SignedInteger::IntVar(BigInt::from(*v)),
            _ => unreachable!(),
        }
    }

    // fn try_downcast(&self, to_size: BitSize) -> Option<SignedInteger> {
    //     match (self, to_size) {
    //         (SignedInteger::Int8(v), BitSize::Bit16) => {
    //             if *v >= i16::MIN as i8 && *v <= i16::MAX as i8 {
    //                 Some(SignedInteger::Int16(*v as i16))
    //             } else {
    //                 None
    //             }
    //         }
    //         (SignedInteger::Int8(v), BitSize::Bit32) => {
    //             if *v >= i32::MIN as i8 && *v <= i32::MAX as i8 {
    //                 Some(SignedInteger::Int32(*v as i32))
    //             } else {
    //                 None
    //             }
    //         }
    //         (SignedInteger::Int8(v), BitSize::Bit64) => {
    //             if *v >= i64::MIN as i8 && *v <= i64::MAX as i8 {
    //                 Some(SignedInteger::Int64(*v as i64))
    //             } else {
    //                 None
    //             }
    //         }
    //         (SignedInteger::Int8(v), BitSize::Bit128) => {
    //             if *v >= i128::MIN as i8 && *v <= i128::MAX as i8 {
    //                 Some(SignedInteger::Int128(*v as i128))
    //             } else {
    //                 None
    //             }
    //         }
    //         (SignedInteger::Int8(v), BitSize::BitVar) => {
    //             Some(SignedInteger::IntVar(BigInt::from(*v)))
    //         }
    //         (SignedInteger::Int16(v), BitSize::Bit32) => {
    //             if *v >= i32::MIN as i16 && *v <= i32::MAX as i16 {
    //                 Some(SignedInteger::Int32(*v as i32))
    //             } else {
    //                 None
    //             }
    //         }
    //         (SignedInteger::Int16(v), BitSize::Bit64) => {
    //             if *v >= i64::MIN as i16 && *v <= i64::MAX as i16 {
    //                 Some(SignedInteger::Int64(*v as i64))
    //             } else {
    //                 None
    //             }
    //         }
    //         (SignedInteger::Int16(v), BitSize::Bit128) => {
    //             if *v >= i128::MIN as i16 && *v <= i128::MAX as i16 {
    //                 Some(SignedInteger::Int128(*v as i128))
    //             } else {
    //                 None
    //             }
    //         }
    //         _ => unreachable!(),
    //     }
    // }

    // fn try_cast(&self, to_size: BitSize) -> Option<SignedInteger> {
    //     match self.get_size().cmp(&to_size) {
    //         Ordering::Less => Some(self.upcast(to_size)),
    //         Ordering::Equal => Some(self.clone()),
    //         Ordering::Greater => self.try_downcast(to_size),
    //     }
    // }

    fn optimize(self) -> SignedInteger {
        todo!()
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
                        SignedInteger::IntVar(BigInt::from(*lhs) + BigInt::from(*rhs))
                    }
                }
                (SignedInteger::Int64(lhs), SignedInteger::Int64(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        SignedInteger::Int64(res)
                    } else {
                        SignedInteger::IntVar(BigInt::from(*lhs) + BigInt::from(*rhs))
                    }
                }
                (SignedInteger::Int128(lhs), SignedInteger::Int128(rhs)) => {
                    if let Some(res) = lhs.checked_add(*rhs) {
                        SignedInteger::Int128(res)
                    } else {
                        SignedInteger::IntVar(BigInt::from(*lhs) + BigInt::from(*rhs))
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
    FloatBig(BigFloat), // Increased precision floating point numbers (fixed but large size)
                        // Look at https://github.com/stencillogic/astro-float for handling Arbitrary precision floating point numbers (variable size)
}

impl GetType for FloatingPoint {
    fn get_type(&self) -> CheckedType {
        CheckedType::Checked(match self {
            FloatingPoint::Float32(_) => std_primitive_types::FLOAT32,
            FloatingPoint::Float64(_) => std_primitive_types::FLOAT64,
            FloatingPoint::FloatBig(_) => std_primitive_types::FLOATBIG,
        })
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

    // fn try_downcast(&self, to_size: BitSize) -> Option<FloatingPoint> {
    //     match (self, to_size) {
    //         (FloatingPoint::Float64(v), BitSize::Bit32) => {
    //             if *v >= f32::MIN as f64 && *v <= f32::MAX as f64 {
    //                 Some(FloatingPoint::Float32(*v as f32))
    //             } else {
    //                 None
    //             }
    //         }
    //         (FloatingPoint::FloatBig(v), BitSize::Bit32) => {
    //             if v >= BigFloat::from(f32::MIN) && v <= BigFloat::from(f32::MAX) {
    //                 Some(FloatingPoint::Float32(v.to_f32().unwrap()))
    //             } else {
    //                 None
    //             }
    //         }
    //         (FloatingPoint::FloatBig(v), BitSize::Bit64) => {
    //             if v >= BigFloat::from(f64::MIN) && v <= BigFloat::from(f64::MAX) {
    //                 Some(FloatingPoint::Float64(v.to_f64().unwrap()))
    //             } else {
    //                 None
    //             }
    //         }
    //         _ => unreachable!(),
    //     }
    // }

    // fn try_cast(&self, to_size: BitSize) -> Option<FloatingPoint> {
    //     match self.get_size().cmp(&to_size) {
    //         Ordering::Less => Some(self.upcast(to_size)),
    //         Ordering::Equal => Some(self.clone()),
    //         Ordering::Greater => self.try_downcast(to_size),
    //     }
    // }

    fn optimize(self) -> FloatingPoint {
        todo!()
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
    SignedInteger(SignedInteger),
    FloatingPoint(FloatingPoint),
}

impl GetType for Number {
    fn get_type(&self) -> CheckedType {
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
                Number::SignedInteger(SignedInteger::Int8(i as i8))
            } else if i >= i16::MIN as i128 && i <= i16::MAX as i128 {
                Number::SignedInteger(SignedInteger::Int16(i as i16))
            } else if i >= i32::MIN as i128 && i <= i32::MAX as i128 {
                Number::SignedInteger(SignedInteger::Int32(i as i32))
            } else if i >= i64::MIN as i128 && i <= i64::MAX as i128 {
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
                Number::SignedInteger(SignedInteger::add(lhs, rhs))
            }
            (Number::FloatingPoint(lhs), Number::FloatingPoint(rhs)) => {
                Number::FloatingPoint(FloatingPoint::add(lhs, rhs))
            }
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
                FloatingPoint::FloatBig(fl) => write!(f, "{}", fl),
            },
        }
    }
}
