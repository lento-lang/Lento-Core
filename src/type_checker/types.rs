use std::fmt::Display;
use crate::util::str::Str;


//--------------------------------------------------------------------------------------//
//                              Compound Type Expressions                               //
//--------------------------------------------------------------------------------------//

pub type NamedType = (String, Type);
pub type CheckedType = Option<Type>; // None means the type is not checked

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionParameterType {
    Singles(Vec<NamedType>),
    Variadic(Option<Vec<NamedType>>, NamedType), // Some initial types, followed by a variadic type
}

impl FunctionParameterType {
    fn fmt_named(f: &mut std::fmt::Formatter<'_>, named: &Vec<NamedType>) -> std::fmt::Result {
        for (i, (name, t)) in named.iter().enumerate() {
            if i != 0 { write!(f, ", ")?; }
            write!(f, "{} {}", t, name)?;
        }
        Ok(())
    }
}

impl Display for FunctionParameterType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionParameterType::Singles(types) => FunctionParameterType::fmt_named(f, types),
            FunctionParameterType::Variadic(types, variadic) => {
                if let Some(t) = types {
                    FunctionParameterType::fmt_named(f, t)?;
                    write!(f, ", ")?;
                }
                write!(f, "{} ...{}", variadic.1, variadic.0)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /**
     * The type of the `any` value.
     * This is the top type.
     * ! Should only accessible within the compiler and native functions in the standard library.
     */
    Any,
    /**
     * The unit type.
     */
    Unit,
    /**
     * A literal type (name).
     * Examples such as `int`, `float`, `string`, `char`, `bool`, `unit`, `any`.
     * Or `IO`, `List`, `Option`, `Result`, `Either`, `Tuple`, `Record`, `Function`, `UserDefinedType`.
     */
    Literal(Str),
    /**
     * A function type.
     * The first argument is the list of parameter types.
     * The second argument is the return type.
     */
    Function(Box<FunctionParameterType>, Box<Type>),
    /**
     * A tuple type.
     * The first argument is the list of element types.
     * ! There must be at least one element in the tuple.
     * A tuple type is a product type.
     */
    Tuple(Vec<Type>),
    /**
     * A list type.
     * The first argument is the type of the elements in the list.
     * A list type is a product type.
     */
    List(Box<Type>),
    /**
     * A record type.
     * The first argument is the list of fields in the record.
     * A record type is a product type.
     */
    Record(Vec<(Str, Type)>),
    /**
     * Generic type with name and parameters.
     * The parameters are the type parameters of the generic type.
     * The definition is the definition of the generic type with access to the type parameters.
     */
    Generic(Str, Vec<Type>, Box<Type>),
    /**
     * A sum type.
     * The first argument is the list of variants in the sum type.
     * A sum type must only refer to existing types. To create alterative types/data structures holding data, use an enum type.
     * Examples of sum types are `int | float`, `string | char`, `bool | unit`, `Option | Result | Either`.
     */
    Sum(Vec<Type>),
    // TODO: Figure out how to separate or join sum and enum types.
    /**
     * An enum type.
     * The first argument is the name of the enum type plus any generic type parameters.
        * It can be a reference to a literal type or a generic type.
     * The second argument is the list of variants in the enum type.
        * Each variant is a tuple of the variant name and the contained data types.
        * The variant data types can be inferred from the enum type parameters or be references to already existing types.
     * Enums must be named using the `enum` keyword: `enum Dir = Left | Right | Up | Down | Forward(i32) | Backward(i32)`.
     * An enum type is a sum type of literal or product types.

     ## Examples:
     ```fsharp
    enum Dir = Left | Right | Up | Down | Forward(i32) | Backward(i32)
    enum Option<T> = None | Some(T)
    enum Result<T, E> = Ok(T) | Err(E)
    enum Either<T, U> = Left(T) | Right(U)
    enum List<T> = Nil | Cons(T, List<T>)
    enum Tree<T> = Leaf(T) | Node(Tree<T>, Tree<T>)
    ```
     */
    Enum(Box<Type>, Vec<(Str, Vec<Type>)>),
}

impl Type {
    pub fn subtype(&self, other: Type) -> bool {
        match (self.to_owned(), other) {
            (Type::Literal(s1), Type::Literal(s2)) => s1 == s2,
            (Type::Generic(s1, params1, _), Type::Generic(s2, params2, _)) => {
                s1 == s2 && params1.len() == params2.len() && params1.iter().zip(params2).all(|(p1, p2)| p1.subtype(p2))
            }
            _ => todo!("implement subtype"),
        }
    }

    pub fn cast(&self, _other: &Self) -> bool {
        todo!("implement type casting")
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Unit => write!(f, "()"),
            Type::Literal(t) => write!(f, "{}", t),
            Type::Function(v, r) => {
                write!(f, "(")?;
                let mut print_params = |p: &Vec<NamedType>| -> std::fmt::Result {
                    for (i, (_, t)) in p.iter().enumerate() {
                        if i > 0 { write!(f, ", ")?; }
                        write!(f, "{}", t)?;
                    }
                    Ok(())
                };
                match v.as_ref() {
                    FunctionParameterType::Singles(s) => print_params(s)?,
                    FunctionParameterType::Variadic(s, v) => {
                        if let Some(s) = s { print_params(s)?; }
                        write!(f, ", ...{}", v.1)?;
                    }
                };
                write!(f, ") -> {}", r)
            }
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::List(t) => write!(f, "[{}]", t),
            Type::Record(fields) => {
                write!(f, "{{")?;
                for (i, (name, t)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, t)?;
                }
                write!(f, "}}")
            }
            Type::Sum(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Generic(s, params, _) => {
                write!(f, "{}<", s)?;
                if !params.is_empty() {
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", param)?;
                    }
                }
                write!(f, ">")
            },
            Type::Enum(e, _) => write!(f, "{}", e),
        }
    }
}

// Standard types
pub mod std_primitive_types {
    use super::*;

    /**
     * The any type.
     * The type of the `any` value.
     * This is the top type.
     * ! Should only accessible within the compiler and native functions in the standard library.
     */
    pub const ANY: Type = Type::Any;

    /**
     * The unit type.
     */
    pub const UNIT: Type = Type::Unit;

    /**
     * A string type. (supports unicode)
     */
    pub const STRING: Type = Type::Literal(Str::Str("str"));

    /**
     * A character type. (supports unicode)
     */
    pub const CHAR: Type = Type::Literal(Str::Str("char"));

    /**
     * A boolean type.
     */
    pub const BOOL: Type = Type::Literal(Str::Str("bool"));

//--------------------------------------------------------------------------------------//
//                                Unsigned integer types                                //
//--------------------------------------------------------------------------------------//

    /**
     * A 1-bit binary value (or boolean).
     */
    pub const UINT1: Type = Type::Literal(Str::Str("u1"));

    /**
     * A 8-bit (1 byte) unsigned integer.
     */
    pub const UINT8: Type = Type::Literal(Str::Str("u8"));

    /**
     * A 16-bit (2 bytes) unsigned integer.
     */
    pub const UINT16: Type = Type::Literal(Str::Str("u16"));

    /**
     * A 32-bit (4 byte) unsigned integer.
     */
    pub const UINT32: Type = Type::Literal(Str::Str("u32"));

    /**
     * A 64-bit (8 byte) unsigned integer.
     */
    pub const UINT64: Type = Type::Literal(Str::Str("u64"));

    /**
     * A 128-bit (16 byte) unsigned integer.
     */
    pub const UINT128: Type = Type::Literal(Str::Str("u128"));

    /**
     * An arbitrary-precision unsigned integer.
     */
    pub const UINTBIG: Type = Type::Literal(Str::Str("ubig"));

//--------------------------------------------------------------------------------------//
//                                 Signed integer types                                 //
//--------------------------------------------------------------------------------------//

    /**
     * A 8-bit (1 byte) signed integer.
     */
    pub const INT8: Type = Type::Literal(Str::Str("i8"));

    /**
     * A 16-bit (2 bytes) signed integer.
     */
    pub const INT16: Type = Type::Literal(Str::Str("i16"));

    /**
     * A 32-bit (4 byte) signed integer.
     */
    pub const INT32: Type = Type::Literal(Str::Str("i32"));

    /**
     * A 64-bit (8 byte) signed integer.
     */
    pub const INT64: Type = Type::Literal(Str::Str("i64"));

    /**
     * A 128-bit (16 byte) signed integer.
     */
    pub const INT128: Type = Type::Literal(Str::Str("i128"));

    /**
     * An arbitrary-precision signed integer.
     */
    pub const INTBIG: Type = Type::Literal(Str::Str("ibig"));

//--------------------------------------------------------------------------------------//
//                             Signed floating-point types                              //
//--------------------------------------------------------------------------------------//

    /**
     * A 32-bit (4 byte) signed floating-point number.
     */
    pub const FLOAT32: Type = Type::Literal(Str::Str("f32"));

    /**
     * A 64-bit (8 byte) signed floating-point number.
     */
    pub const FLOAT64: Type = Type::Literal(Str::Str("f64"));

    /**
     * Increases the precision of a floating-point number.
     */
    pub const FLOATBIG: Type = Type::Literal(Str::Str("fbig"));

    /**
     * Find a standard primitive type from a literal name or alias.
     */
    pub fn find_type(t: String) -> Option<Type> {
        match t.as_str() {
            "u1" | "bit" => Some(UINT1),
            "u8" | "byte" => Some(UINT8),
            "u16" | "ushort" => Some(UINT16),
            "u32" | "uint" => Some(UINT32),
            "u64" | "ulong" => Some(UINT64),
            "u128" => Some(UINT128),
            "ubig" => Some(UINTBIG),
            "i8" => Some(INT8),
            "i16" | "short" => Some(INT16),
            "i32" | "int" => Some(INT32),
            "i64" | "long" => Some(INT64),
            "i128" => Some(INT128),
            "ibig" => Some(INTBIG),
            "f32" | "float" => Some(FLOAT32),
            "f64" => Some(FLOAT64),
            "fbig" => Some(FLOATBIG),
            "string" => Some(STRING),
            "char" => Some(CHAR),
            "bool" => Some(BOOL),
            "unit" | "()" => Some(UNIT),
            _ => None,
        }
    }

    pub fn find_type_str(t: &str) -> Option<Type> {
        find_type(t.to_string())
    }

}

//--------------------------------------------------------------------------------------//
//                                     Value trait                                      //
//--------------------------------------------------------------------------------------//

pub trait GetType {
    fn get_type(&self) -> CheckedType;
}
