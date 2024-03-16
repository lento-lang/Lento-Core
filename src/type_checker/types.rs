use crate::{parser::ast::Ast, util::str::Str};
use std::fmt::{Debug, Display};

//--------------------------------------------------------------------------------------//
//                                     Type System                                      //
//--------------------------------------------------------------------------------------//

// Compound Type Expressions
pub type NamedType = (String, Type);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CheckedType {
    Checked(Type),
    Unchecked,
}

impl CheckedType {
    /// Should always be ok after type checking
    pub fn unwrap_checked(&self) -> &Type {
        match self {
            CheckedType::Checked(t) => t,
            CheckedType::Unchecked => panic!("Unwrap of unchecked type"),
        }
    }

    pub fn is_exact_type(&self, t: &Type) -> bool {
        match self {
            CheckedType::Checked(t1) => t1 == t,
            CheckedType::Unchecked => false,
        }
    }
}

impl Display for CheckedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CheckedType::Checked(t) => write!(f, "{}", t),
            CheckedType::Unchecked => write!(f, "unchecked"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionParameterType {
    Singles(Vec<NamedType>),
    Variadic(Vec<NamedType>, NamedType), // Some initial types, followed by a variadic type
}

impl FunctionParameterType {
    fn fmt_named(f: &mut std::fmt::Formatter<'_>, named: &[NamedType]) -> std::fmt::Result {
        for (i, (name, t)) in named.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} {}", t, name)?;
        }
        Ok(())
    }

    /// Match the given arguments to the function parameters.
    /// Return true if the arguments suffice the parameter types.
    pub fn match_args(&self, args: &[Ast]) -> bool {
        match self {
            FunctionParameterType::Singles(types) => {
                if types.len() != args.len() {
                    return false;
                }
                for (i, (_, t)) in types.iter().enumerate() {
                    if !args[i].get_type().unwrap_checked().subtype(t) {
                        return false;
                    }
                }
                true
            }
            FunctionParameterType::Variadic(types, (_, var_type)) => {
                if types.len() > args.len() {
                    return false;
                }
                for (i, (_, t)) in types.iter().enumerate() {
                    if !t.subtype(args[i].get_type().unwrap_checked()) {
                        return false;
                    }
                }
                for arg in &args[types.len()..] {
                    if !var_type.subtype(arg.get_type().unwrap_checked()) {
                        return false;
                    }
                }
                true
            }
        }
    }

    pub fn is_variadic(&self) -> bool {
        match self {
            FunctionParameterType::Singles(_) => false,
            FunctionParameterType::Variadic(_, _) => true,
        }
    }

    pub fn as_single(&self) -> Option<&Vec<NamedType>> {
        match self {
            FunctionParameterType::Singles(types) => Some(types),
            FunctionParameterType::Variadic(_, _) => None,
        }
    }

    pub fn as_variadic(&self) -> Option<(&Vec<NamedType>, &NamedType)> {
        match self {
            FunctionParameterType::Singles(_) => None,
            FunctionParameterType::Variadic(types, variadic) => Some((types, variadic)),
        }
    }
}

impl Display for FunctionParameterType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionParameterType::Singles(types) => FunctionParameterType::fmt_named(f, types),
            FunctionParameterType::Variadic(types, variadic) => {
                FunctionParameterType::fmt_named(f, types)?;
                if !types.is_empty() {
                    write!(f, ", ")?;
                }
                write!(f, "{} ...{}", variadic.1, variadic.0)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// The type of the `any` value.
    /// This is the top type.
    ///! Should only accessible within the compiler and native functions in the standard library.
    Any,

    /// The unit type.
    Unit,

    /// A literal type (name).
    /// Examples such as `int`, `float`, `string`, `char`, `bool`, `unit`, `any`.
    /// Or `IO`, `List`, `Option`, `Result`, `Either`, `Tuple`, `Record`, `Function`, `UserDefinedType`.
    Literal(Str),

    /// A function type.
    /// The first argument is the list of parameter types.
    /// The second argument is the return type.
    Function(Box<FunctionParameterType>, Box<Type>),

    /// A tuple type.
    /// The first argument is the list of element types.
    ///! There must be at least one element in the tuple.
    /// A tuple type is a product type.
    Tuple(Vec<Type>),

    /// A list type.
    /// The first argument is the type of the elements in the list.
    /// A list type is a product type.
    List(Box<Type>),

    /// A record type.
    /// The first argument is the list of fields in the record.
    /// A record type is a product type.
    Record(Vec<(Str, Type)>),

    /// Generic type with name and parameters.
    /// The parameters are the type parameters of the generic type.
    /// The definition is the definition of the generic type with access to the type parameters.
    Generic(Str, Vec<Type>, Box<Type>),

    /// A sum type.
    /// The first argument is the list of variants in the sum type.
    /// A sum type must only refer to existing types. To create alterative types/data structures holding data, use an enum type.
    /// Examples of sum types are `int | float`, `string | char`, `bool | unit`, `Option | Result | Either`.
    Sum(Vec<Type>),

    /// An enum type.
    /// The first argument is the name of the enum type plus any generic type parameters.
    ///  * It can be a reference to a literal type or a generic type.
    ///
    /// The second argument is the list of variants in the enum type.
    ///
    ///  * Each variant is a tuple of the variant name and the contained data types.
    ///  * The variant data types can be inferred from the enum type parameters or be references to already existing types.
    ///
    /// Enums must be named using the `enum` keyword: `enum Dir = Left | Right | Up | Down | Forward(i32) | Backward(i32)`.
    /// An enum type is a sum type of literal or product types.
    ///
    /// ## Examples:
    /// ```fsharp
    /// enum Dir = Left | Right | Up | Down | Forward(i32) | Backward(i32)
    /// enum Option<T> = None | Some(T)
    /// enum Result<T, E> = Ok(T) | Err(E)
    /// enum Either<T, U> = Left(T) | Right(U)
    /// enum List<T> = Nil | Cons(T, List<T>)
    /// enum Tree<T> = Leaf(T) | Node(Tree<T>, Tree<T>)
    /// ```
    Enum(Box<Type>, Vec<(Str, Vec<Type>)>),
    // TODO: Figure out how to separate or join sum and enum types.
}

impl Type {
    pub fn subtype(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Literal(s1), Type::Literal(s2)) => *s1 == *s2,
            (Type::Generic(s1, params1, _), Type::Generic(s2, params2, _)) => {
                s1 == s2
                    && params1.len() == params2.len()
                    && params1.iter().zip(params2).all(|(p1, p2)| p1.subtype(p2))
            }
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                match (params1.is_variadic(), params2.is_variadic()) {
                    (true, true) => {
                        let (params1, variadic1) = params1.as_variadic().unwrap();
                        let (params2, variadic2) = params2.as_variadic().unwrap();
                        params1.len() == params2.len()
                            && params1
                                .iter()
                                .zip(params2)
                                .all(|((_, p1), (_, p2))| p1.subtype(p2))
                            && variadic1.1.subtype(&variadic2.1)
                            && ret1.subtype(ret2)
                    }
                    (false, false) => {
                        let params1 = params1.as_single().unwrap();
                        let params2 = params2.as_single().unwrap();
                        params1.len() == params2.len()
                            && params1
                                .iter()
                                .zip(params2)
                                .all(|((_, p1), (_, p2))| p1.subtype(p2))
                            && ret1.subtype(ret2)
                    }
                    (true, false) => false, // Cannot convert a variadic function to a non-variadic function.
                    (false, true) => false, // Cannot convert a non-variadic function to a variadic function.
                }
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                types1.len() == types2.len()
                    && types1.iter().zip(types2).all(|(t1, t2)| t1.subtype(t2))
            }
            (Type::List(t1), Type::List(t2)) => t1.subtype(t2),
            (Type::Record(fields1), Type::Record(fields2)) => {
                fields1.len() == fields2.len()
                    && fields1
                        .iter()
                        .zip(fields2)
                        .all(|((n1, t1), (n2, t2))| n1 == n2 && t1.subtype(t2))
            }
            (Type::Sum(types1), Type::Sum(types2)) => {
                types1.len() == types2.len()
                    && types1.iter().zip(types2).all(|(t1, t2)| t1.subtype(t2))
            }
            (_, Type::Sum(types)) => types.iter().any(|t| self.subtype(t)),
            (Type::Enum(name1, variants1), Type::Enum(name2, variants2)) => {
                name1 == name2
                    && variants1.len() == variants2.len()
                    && variants1.iter().zip(variants2).all(|((n1, t1), (n2, t2))| {
                        n1 == n2
                            && t1.len() == t2.len()
                            && t1.iter().zip(t2).all(|(t1, t2)| t1.subtype(t2))
                    })
            }
            (Type::Any, _) => true,
            (_, Type::Any) => true,
            _ => false, // todo!("implement subtype"),
        }
    }

    pub fn cast(&self, _other: &Self) -> bool {
        todo!("implement type casting")
    }

    pub fn simplify(&self) -> Self {
        match self {
            Type::Literal(_) => self.clone(),
            Type::Generic(s, params, body) => Type::Generic(
                s.clone(),
                params.iter().map(Type::simplify).collect(),
                Box::new(body.simplify()),
            ),
            Type::Function(params, ret) => Type::Function(params.clone(), Box::new(ret.simplify())),
            Type::Tuple(types) => Type::Tuple(types.iter().map(Type::simplify).collect()),
            Type::List(t) => Type::List(Box::new(t.simplify())),
            Type::Record(fields) => Type::Record(
                fields
                    .iter()
                    .map(|(n, t)| (n.clone(), t.simplify()))
                    .collect(),
            ),
            Type::Sum(types) => {
                // Flatten nested sums.
                let mut result = vec![];
                for t in types {
                    match t.simplify() {
                        Type::Sum(types) => result.extend(types),
                        t => result.push(t),
                    }
                }
                Type::Sum(result)
            }
            Type::Enum(name, variants) => Type::Enum(
                name.clone(),
                variants
                    .iter()
                    .map(|(n, t)| (n.clone(), t.iter().map(Type::simplify).collect()))
                    .collect(),
            ),
            Type::Any => self.clone(),
            Type::Unit => self.clone(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.simplify() {
            Type::Any => write!(f, "any"),
            Type::Unit => write!(f, "()"),
            Type::Literal(t) => write!(f, "{}", t),
            Type::Function(v, r) => {
                write!(f, "(")?;
                let mut print_params = |p: &Vec<NamedType>| -> std::fmt::Result {
                    for (i, (_, t)) in p.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", t)?;
                    }
                    Ok(())
                };
                match v.as_ref() {
                    FunctionParameterType::Singles(s) => print_params(s)?,
                    FunctionParameterType::Variadic(s, v) => {
                        print_params(s)?;
                        if !s.is_empty() {
                            write!(f, ", ")?;
                        }
                        write!(f, "...{}", v.1)?;
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
            }
            Type::Enum(e, _) => write!(f, "{}", e),
        }
    }
}

// Standard types
pub mod std_primitive_types {
    use super::*;

    /// The any type.
    /// The type of the `any` value.
    /// This is the top type.
    /// ! Should only accessible within the compiler and native functions in the standard library.
    pub const ANY: Type = Type::Any;

    /// The unit type.
    pub const UNIT: Type = Type::Unit;

    /// A string type. (supports unicode)
    pub const STRING: Type = Type::Literal(Str::Str("str"));

    /// A character type. (supports unicode)
    pub const CHAR: Type = Type::Literal(Str::Str("char"));

    /// A boolean type.
    pub const BOOL: Type = Type::Literal(Str::Str("bool"));

    //--------------------------------------------------------------------------------------//
    //                                Unsigned integer types                                //
    //--------------------------------------------------------------------------------------//

    /// A 1-bit binary value (or boolean).
    pub const UINT1: Type = Type::Literal(Str::Str("u1"));

    /// A 8-bit (1 byte) unsigned integer.
    pub const UINT8: Type = Type::Literal(Str::Str("u8"));

    /// A 16-bit (2 bytes) unsigned integer.
    pub const UINT16: Type = Type::Literal(Str::Str("u16"));

    /// A 32-bit (4 byte) unsigned integer.
    pub const UINT32: Type = Type::Literal(Str::Str("u32"));

    /// A 64-bit (8 byte) unsigned integer.
    pub const UINT64: Type = Type::Literal(Str::Str("u64"));

    /// A 128-bit (16 byte) unsigned integer.
    pub const UINT128: Type = Type::Literal(Str::Str("u128"));

    /// An arbitrary-precision unsigned integer.
    pub const UINTBIG: Type = Type::Literal(Str::Str("ubig"));

    //--------------------------------------------------------------------------------------//
    //                                 Signed integer types                                 //
    //--------------------------------------------------------------------------------------//

    /// A 8-bit (1 byte) signed integer.
    pub const INT8: Type = Type::Literal(Str::Str("i8"));

    /// A 16-bit (2 bytes) signed integer.
    pub const INT16: Type = Type::Literal(Str::Str("i16"));

    /// A 32-bit (4 byte) signed integer.
    pub const INT32: Type = Type::Literal(Str::Str("i32"));

    /// A 64-bit (8 byte) signed integer.
    pub const INT64: Type = Type::Literal(Str::Str("i64"));

    /// A 128-bit (16 byte) signed integer.
    pub const INT128: Type = Type::Literal(Str::Str("i128"));

    /// An arbitrary-precision signed integer.
    pub const INTBIG: Type = Type::Literal(Str::Str("ibig"));

    //--------------------------------------------------------------------------------------//
    //                             Signed floating-point types                              //
    //--------------------------------------------------------------------------------------//

    /// A 32-bit (4 byte) signed floating-point number.
    pub const FLOAT32: Type = Type::Literal(Str::Str("f32"));

    /// A 64-bit (8 byte) signed floating-point number.
    pub const FLOAT64: Type = Type::Literal(Str::Str("f64"));

    /// Increases the precision of a floating-point number.
    pub const FLOATBIG: Type = Type::Literal(Str::Str("fbig"));

    /// Find a standard primitive type from a literal name or alias.
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

pub mod std_compount_types {
    use super::*;

    pub fn any_signed_integer() -> Type {
        Type::Sum(vec![
            std_primitive_types::INT8,
            std_primitive_types::INT16,
            std_primitive_types::INT32,
            std_primitive_types::INT64,
            std_primitive_types::INT128,
            std_primitive_types::INTBIG,
        ])
    }

    pub fn any_unsigned_integer() -> Type {
        Type::Sum(vec![
            std_primitive_types::UINT1,
            std_primitive_types::UINT8,
            std_primitive_types::UINT16,
            std_primitive_types::UINT32,
            std_primitive_types::UINT64,
            std_primitive_types::UINT128,
            std_primitive_types::UINTBIG,
        ])
    }

    pub fn any_float() -> Type {
        Type::Sum(vec![
            std_primitive_types::FLOAT32,
            std_primitive_types::FLOAT64,
            std_primitive_types::FLOATBIG,
        ])
    }

    pub fn any_integer() -> Type {
        Type::Sum(vec![any_signed_integer(), any_unsigned_integer()])
    }

    pub fn any_number() -> Type {
        Type::Sum(vec![any_integer(), any_float()])
    }
}

//--------------------------------------------------------------------------------------//
//                                     Value trait                                      //
//--------------------------------------------------------------------------------------//

pub trait GetType {
    fn get_type(&self) -> CheckedType;
}
