use crate::{
    interpreter::value::{RecordKey, Value},
    util::str::Str,
};
use std::fmt::{Debug, Display};

//--------------------------------------------------------------------------------------//
//                                     Type System                                      //
//--------------------------------------------------------------------------------------//

/// Generalized trait for type implementations
pub trait TypeTrait {
    /// Check if the type is equal to the other type.
    /// Two types are equal if they are the same type.
    /// The equality relation is reflexive, symmetric, and transitive.
    fn equals(&self, other: &Self) -> bool {
        self.subtype(other) && other.subtype(self)
    }

    /// Check if the type is a subtype of the other type.
    /// A type is a subtype of another type if it can be used in place of the other type.
    ///
    /// ## Mathematical Notation
    /// ```ignore
    /// A <: B, where A and B are types.
    /// ```
    ///
    /// ## Examples
    /// ```ignore
    /// int <: number
    /// list int <: list number
    /// list int <: list any <: any
    ///
    /// // Covariant return type and contravariant parameter type
    /// f(x: int) -> int <: f(x: number) -> number
    /// ```
    ///
    /// **Covariant return type**: The return type of the subtype function (int) is a subtype of the return type of the supertype function (number). \
    /// **Contravariant parameter type** : The parameter type of the supertype function (number) is a supertype of the parameter type of the subtype function (int).
    fn subtype(&self, other: &Self) -> bool;
    fn simplify(self) -> Self;
}

// Compound Type Expressions
pub type NamedType = (String, Type);

/// TODO: Reimplement function parameter types
/// TODO: So that it allows multiple variadic parameters mixed with single parameters
/// TODO: while they do not have ambiguities, conflicts, or overlaps.
/// TODO: Eg. `(a: int, b: int, ...c: int, d: bool, ...e: bool, f: string)`
/// TODO: BUT NOT: `(a: int, b: int, ...c: int, d: int, ...e: int, f: string)`
/// TODO: because `c`, `d`, and `e` are ambiguous.
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionParameterType {
    Singles(Vec<NamedType>),
    Variadic(Vec<NamedType>, NamedType), // Some initial types, followed by a variadic type
}

impl FunctionParameterType {
    /// Match the given arguments to the function parameters.
    /// Return true if the arguments suffice the parameter types.
    pub fn match_args(&self, args: &[Value]) -> bool {
        match self {
            FunctionParameterType::Singles(types) => {
                if types.len() != args.len() {
                    return false;
                }
                for (i, (_, t)) in types.iter().enumerate() {
                    if !args[i].get_type().subtype(t) {
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
                    if !t.subtype(args[i].get_type()) {
                        return false;
                    }
                }
                for arg in &args[types.len()..] {
                    if !var_type.subtype(arg.get_type()) {
                        return false;
                    }
                }
                true
            }
        }
    }

    pub fn match_args_types(&self, arg_types: &[&Type]) -> bool {
        match self {
            FunctionParameterType::Singles(types) => {
                if types.len() != arg_types.len() {
                    return false;
                }
                for (i, (_, t)) in types.iter().enumerate() {
                    if !arg_types[i].subtype(t) {
                        return false;
                    }
                }
                true
            }
            FunctionParameterType::Variadic(types, (_, var_type)) => {
                if types.len() > arg_types.len() {
                    return false;
                }
                for (i, (_, t)) in types.iter().enumerate() {
                    if !t.subtype(arg_types[i]) {
                        return false;
                    }
                }
                for arg in &arg_types[types.len()..] {
                    if !var_type.subtype(arg) {
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

    fn fmt_named(f: &mut std::fmt::Formatter<'_>, named: &[NamedType]) -> std::fmt::Result {
        if named.is_empty() {
            return write!(f, "()");
        }

        if named.len() > 1 {
            write!(f, "(")?;
        }
        for (i, (name, t)) in named.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} {}", t, name)?;
        }
        if named.len() > 1 {
            write!(f, ")")?;
        }
        Ok(())
    }

    fn fmt_named_color(named: &[NamedType]) -> String {
        use colorful::Colorful;

        if named.is_empty() {
            return "()".to_string();
        }

        let mut result = String::new();
        if named.len() > 1 {
            result.push_str(&"(".dark_gray().to_string());
        }
        for (i, (name, t)) in named.iter().enumerate() {
            if i != 0 {
                result.push_str(&", ".dark_gray().to_string());
            }
            result.push_str(&format!("{} {}", t.pretty_print_color(), name));
        }
        if named.len() > 1 {
            result.push_str(&")".dark_gray().to_string());
        }
        result
    }

    fn fmt_unnamed_color(types: &[&Type]) -> String {
        use colorful::Colorful;

        if types.is_empty() {
            return "()".to_string();
        }

        let mut result = String::new();
        if types.len() > 1 {
            result.push_str(&"(".dark_gray().to_string());
        }
        for (i, t) in types.iter().enumerate() {
            if i != 0 {
                result.push_str(&", ".dark_gray().to_string());
            }
            result.push_str(&t.pretty_print_color());
        }
        if types.len() > 1 {
            result.push_str(&")".dark_gray().to_string());
        }
        result
    }

    pub fn pretty_print_color(&self) -> String {
        use colorful::Colorful;

        match self {
            FunctionParameterType::Singles(types) => FunctionParameterType::fmt_named_color(types),
            FunctionParameterType::Variadic(types, variadic) => {
                let mut result = FunctionParameterType::fmt_named_color(types);
                if !types.is_empty() {
                    result.push_str(&", ".dark_gray().to_string());
                }
                result.push_str(&format!(
                    "...{} {}",
                    variadic.1.to_string().light_blue(),
                    variadic.0
                ));
                result
            }
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
                write!(f, "...{} {}", variadic.1, variadic.0)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariationType {
    pub params: FunctionParameterType,
    pub ret: Type,
}

impl VariationType {
    pub fn new(params: FunctionParameterType, ret: Type) -> Self {
        VariationType { params, ret }
    }

    pub fn get_params(&self) -> &FunctionParameterType {
        &self.params
    }

    pub fn get_return_type(&self) -> &Type {
        &self.ret
    }
}

impl TypeTrait for VariationType {
    fn subtype(&self, other: &Self) -> bool {
        match (self.params.is_variadic(), other.params.is_variadic()) {
            (true, true) => {
                let (params, variadic) = self.params.as_variadic().unwrap();
                let (other_params, other_variadic) = other.params.as_variadic().unwrap();
                params.len() == other_params.len()
                    && params
                        .iter()
                        .zip(other_params)
                        .all(|((_, p1), (_, p2))| p1.subtype(p2))
                    && variadic.1.subtype(&other_variadic.1)
                    && self.ret.subtype(&other.ret)
            }
            (false, false) => {
                let params = self.params.as_single().unwrap();
                let other_params = other.params.as_single().unwrap();
                params.len() == other_params.len()
                    && params
                        .iter()
                        .zip(other_params)
                        .all(|((_, p1), (_, p2))| p1.subtype(p2))
                    && self.ret.subtype(&other.ret)
            }
            (true, false) => false, // Cannot convert a variadic function to a non-variadic function.
            (false, true) => false, // Cannot convert a non-variadic function to a variadic function.
        }
    }

    fn simplify(self) -> Self {
        VariationType {
            params: match self.params {
                FunctionParameterType::Singles(types) => FunctionParameterType::Singles(
                    types.into_iter().map(|(n, t)| (n, t.simplify())).collect(),
                ),
                FunctionParameterType::Variadic(types, variadic) => {
                    FunctionParameterType::Variadic(
                        types.into_iter().map(|(n, t)| (n, t.simplify())).collect(),
                        (variadic.0, variadic.1.simplify()),
                    )
                }
            },
            ret: self.ret.simplify(),
        }
    }
}

impl Display for VariationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.params, self.ret)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// A literal type (name).
    /// Examples such as `int`, `float`, `string`, `char`, `bool`, `unit`, `any`.
    /// Or `IO`, `List`, `Option`, `Result`, `Either`, `Tuple`, `Record`, `Function`, `UserDefinedType`.
    Literal(Str),

    /// A function type.
    /// The first argument is the list of parameter types.
    /// The second argument is the return type.
    Function(Vec<VariationType>),

    /// A tuple type.
    /// The first argument is the list of element types.
    /// ! There must be at least one element in the tuple.
    /// A tuple type is a product type.
    Tuple(Vec<Type>),

    /// A list type.
    /// The first argument is the type of the elements in the list.
    /// A list type is a product type.
    List(Box<Type>),

    /// A record type.
    /// The first argument is the list of fields in the record.
    /// A record type is a product type.
    Record(Vec<(RecordKey, Type)>),

    /// Generic type with name and parameters.
    /// The parameters are the type parameters of the generic type.
    /// The definition is the definition of the generic type with access to the type parameters.
    Generic(Str, Vec<Type>, Box<Type>),

    /// A sum type.
    /// The first argument is the list of variants in the sum type.
    /// A sum type must only refer to existing types. To create alterative types/data structures holding data, use an enum type.
    /// Examples of sum types are `int | float`, `string | char`, `bool | unit`, `Option | Result | Either` or
    /// `Dir = Left | Right | Up | Down | Forward(i32) | Backward(i32)`.
    Sum(Vec<Type>),

    /// A variant type.
    /// The first argument is a reference to the parent type holding a sum type of one or more variants.
    /// The second argument is the type of the boxed value.
    /// The third argument is the name of the boxed variant.
    /// A variant type is a product type of one or more wrapped types.
    ///
    /// ## Examples:
    /// ```fsharp
    /// type Box T = Wrapped T        // Box.Wrapped(T)
    /// type Option T = Some T | None
    /// type Result T E = Ok(T) | Err(E)
    /// type Either T U = Left T | Right U
    /// type List T = Nil | Cons(T, (List T))
    /// type Tree T = Leaf T | Node (Tree T) (Tree T)
    /// ```
    ///
    /// ## Note
    /// The variant type is a product type, but it is not a sum type.
    /// It is often used to create sum types with named variants and to create recursive data structures.
    ///
    /// ## Note on equality
    /// Two variant types are equal if they have the same name and the same number of fields with the same types.
    Variant(Box<Type>, Str, Vec<Type>),
}

impl TypeTrait for Type {
    fn subtype(&self, other: &Type) -> bool {
        match (self, other) {
            (&std_primitive_types::ANY, _) => true,
            (_, &std_primitive_types::ANY) => true,
            (Type::Literal(s1), Type::Literal(s2)) => *s1 == *s2,
            (Type::Generic(s1, params1, _), Type::Generic(s2, params2, _)) => {
                s1 == s2
                    && params1.len() == params2.len()
                    && params1.iter().zip(params2).all(|(p1, p2)| p1.subtype(p2))
            }
            (Type::Function(ty1), Type::Function(ty2)) => {
                ty1.len() == ty2.len() && ty1.iter().zip(ty2).all(|(t1, t2)| t1.subtype(t2))
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
            (Type::Variant(parent1, name1, fields1), Type::Variant(parent2, name2, fields2)) => {
                parent1.eq(parent2)
                    && name1 == name2
                    && fields1.len() == fields2.len()
                    && fields1.iter().zip(fields2).all(|(t1, t2)| t1.subtype(t2))
            }
            _ => false,
        }
    }

    fn simplify(self) -> Self {
        match self {
            Type::Literal(_) => self,
            Type::Generic(s, params, body) => Type::Generic(
                s,
                params.into_iter().map(Type::simplify).collect(),
                Box::new(body.simplify()),
            ),
            Type::Function(ty) => {
                Type::Function(ty.into_iter().map(VariationType::simplify).collect())
            }
            Type::Tuple(types) => Type::Tuple(types.into_iter().map(Type::simplify).collect()),
            Type::List(t) => Type::List(Box::new(t.simplify())),
            Type::Record(fields) => {
                Type::Record(fields.into_iter().map(|(n, t)| (n, t.simplify())).collect())
            }
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

            // Type::Enum(name, variants) => Type::Enum(
            //     name,
            //     variants
            //         .into_iter()
            //         .map(|(n, t)| (n, t.into_iter().map(Type::simplify).collect()))
            //         .collect(),
            // ),
            Type::Variant(parent, name, fields) => Type::Variant(
                parent,
                name,
                fields.into_iter().map(Type::simplify).collect(),
            ),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone().simplify() {
            Type::Literal(t) => write!(f, "{}", t),
            Type::Function(variations) => {
                let print_params =
                    |f: &mut std::fmt::Formatter<'_>, p: &Vec<NamedType>| -> std::fmt::Result {
                        if p.len() != 1 {
                            write!(f, "(")?;
                        }
                        for (i, (_, t)) in p.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", t)?;
                        }
                        if p.len() != 1 {
                            write!(f, ")")
                        } else {
                            Ok(())
                        }
                    };
                for (i, variation) in variations.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    match &variation.params {
                        FunctionParameterType::Singles(s) => print_params(f, s)?,
                        FunctionParameterType::Variadic(s, v) => {
                            print_params(f, s)?;
                            if !s.is_empty() {
                                write!(f, ", ")?;
                            }
                            write!(f, "...{}", v.1)?;
                        }
                    };
                    write!(f, " -> {}", variation.ret)?;
                }
                Ok(())
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
            // Type::Enum(e, _) => write!(f, "{}", e),
            Type::Variant(_, name, fields) => {
                write!(f, "{}(", name)?;
                for (i, t) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Type {
    pub fn pretty_print_color(&self) -> String {
        use colorful::Colorful;

        match self.clone().simplify() {
            Type::Literal(t) => t.to_string().light_blue().to_string(),
            Type::Function(variations) => {
                let mut result = String::new();
                for (i, variation) in variations.iter().enumerate() {
                    if i > 0 {
                        result.push_str(&" | ".dark_gray().to_string());
                    }
                    match &variation.params {
                        FunctionParameterType::Singles(s) => {
                            result.push_str(&FunctionParameterType::fmt_unnamed_color(
                                &s.iter().map(|(_, t)| t).collect::<Vec<_>>(),
                            ));
                        }
                        FunctionParameterType::Variadic(s, v) => {
                            result.push_str(&FunctionParameterType::fmt_unnamed_color(
                                &s.iter().map(|(_, t)| t).collect::<Vec<_>>(),
                            ));
                            if !s.is_empty() {
                                result.push_str(&", ".dark_gray().to_string());
                            }
                            result.push_str(&format!("...{}", v.1.to_string().light_blue()));
                        }
                    };
                    result.push_str(&format!(
                        " {} {}",
                        "->".dark_gray(),
                        variation.ret.to_string().light_blue()
                    ));
                }
                result
            }
            Type::Tuple(types) => {
                if types.is_empty() {
                    "()".to_string()
                } else {
                    let mut result = String::new();
                    result.push_str(&"(".dark_gray().to_string());
                    for (i, t) in types.iter().enumerate() {
                        if i > 0 {
                            result.push_str(&", ".dark_gray().to_string());
                        }
                        result.push_str(&t.pretty_print_color());
                    }
                    result.push_str(&")".dark_gray().to_string());
                    result
                }
            }
            Type::List(t) => format!("[{}]", t.pretty_print_color()),
            Type::Record(fields) => {
                let mut result = String::new();
                result.push_str(&"{".dark_gray().to_string());
                for (i, (name, t)) in fields.iter().enumerate() {
                    if i > 0 {
                        result.push_str(&", ".dark_gray().to_string());
                    }
                    result.push_str(&format!("{}: {}", name, t.pretty_print_color()));
                }
                result.push_str(&"}".dark_gray().to_string());
                result
            }
            Type::Sum(types) => {
                if types.is_empty() {
                    "()".to_string()
                } else {
                    let mut result = String::new();
                    result.push_str(&"(".dark_gray().to_string());
                    for (i, t) in types.iter().enumerate() {
                        if i > 0 {
                            result.push_str(&" | ".dark_gray().to_string());
                        }
                        result.push_str(&t.pretty_print_color());
                    }
                    result.push_str(&")".dark_gray().to_string());
                    result
                }
            }
            Type::Generic(s, params, _) => {
                let mut result = String::new();
                result.push_str(&s.to_string().light_blue().to_string());
                if !params.is_empty() {
                    result.push_str(&"<".dark_gray().to_string());
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            result.push_str(&", ".dark_gray().to_string());
                        }
                        result.push_str(&param.pretty_print_color());
                    }
                    result.push_str(&">".dark_gray().to_string());
                }
                result
            }
            Type::Variant(_, name, fields) => {
                let mut result = String::new();
                result.push_str(&name.to_string().light_blue().to_string());
                result.push_str(&"(".dark_gray().to_string());
                for (i, t) in fields.iter().enumerate() {
                    if i > 0 {
                        result.push_str(&", ".dark_gray().to_string());
                    }
                    result.push_str(&t.pretty_print_color());
                }
                result.push_str(&")".dark_gray().to_string());
                result
            }
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
    pub const ANY: Type = Type::Literal(Str::Str("any"));

    /// The top type, supertype of all types and itself.
    pub const TYPE: Type = Type::Literal(Str::Str("type"));

    /// The unit type.
    pub const UNIT: Type = Type::Literal(Str::Str("unit"));

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
}

pub mod std_compound_types {
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

pub mod std_collection_types {
    use super::*;

    pub fn list(t: Type) -> Type {
        Type::List(Box::new(t))
    }

    pub fn tuple(types: Vec<Type>) -> Type {
        Type::Tuple(types)
    }

    pub fn record(fields: Vec<(RecordKey, Type)>) -> Type {
        Type::Record(fields)
    }
}

//--------------------------------------------------------------------------------------//
//                                     Value trait                                      //
//--------------------------------------------------------------------------------------//

pub trait GetType {
    fn get_type(&self) -> &Type;
}
