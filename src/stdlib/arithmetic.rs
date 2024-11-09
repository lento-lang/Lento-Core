use crate::{
    interpreter::{
        error::runtime_error,
        number::{ArithmeticOperations, Number},
        value::{FunctionVariation, Value},
    },
    type_checker::types::{std_types, FunctionParameterType, GetType, TypeTrait},
};

//--------------------------------------------------------------------------------------//
//                               Native Runtime Functions                               //
//--------------------------------------------------------------------------------------//

pub fn add() -> FunctionVariation {
    FunctionVariation::new_native(
        |values| {
            let ty_num = std_types::NUM();
            let values = values.unwrap_singles();
            if values.len() != 2 {
                return Err(runtime_error("add() expects 2 arguments".to_string()));
            }
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            if lhs.get_type().subtype(&ty_num) && rhs.get_type().subtype(&ty_num) {
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
            ("lhs".to_string(), std_types::NUM()),
            ("rhs".to_string(), std_types::NUM()),
        ]),
        std_types::NUM(),
    )
}
    )
}
