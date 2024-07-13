use crate::{
    interpreter::{
        error::runtime_error,
        value::{ArithmeticOperations, FunctionVariation, NativeFunctionParameters, Number, Value},
    },
    type_checker::types::{std_compount_types, FunctionParameterType, GetType, TypeTrait},
};

//--------------------------------------------------------------------------------------//
//                               Native Runtime Functions                               //
//--------------------------------------------------------------------------------------//

pub fn add() -> FunctionVariation {
    FunctionVariation::Native(
        |vals| {
            let ty_num = std_compount_types::any_number();
            let vals = if let NativeFunctionParameters::Singles(v) = vals {
                v
            } else {
                panic!("A native function with Singles function parameter type should not be able to receive a Variadic function parameter type")
            };
            if vals.len() != 2 {
                return Err(runtime_error("add() expects 2 arguments".to_string()));
            }
            let lhs = vals[0].clone();
            let rhs = vals[1].clone();
            if lhs.get_type().unwrap_checked().subtype(&ty_num)
                && rhs.get_type().unwrap_checked().subtype(&ty_num)
            {
                match (lhs, rhs) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(Number::add(&l, &r))),
                    _ => panic!("add expects 2 arguments of type '{}'", ty_num),
                }
            } else {
                Err(runtime_error(format!(
                    "add expects 2 arguments of type '{}', got '{}' and '{}'",
                    ty_num,
                    lhs.get_type(),
                    rhs.get_type()
                )))
            }
        },
        FunctionParameterType::Singles(vec![
            ("lhs".to_string(), std_compount_types::any_number()),
            ("rhs".to_string(), std_compount_types::any_number()),
        ]),
        std_compount_types::any_number(),
    )
}
