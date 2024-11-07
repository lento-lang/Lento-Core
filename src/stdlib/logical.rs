use crate::{
    interpreter::{
        error::runtime_error,
        value::{FunctionVariation, Value},
    },
    type_checker::types::{std_primitive_types, FunctionParameterType},
};

pub fn eq() -> FunctionVariation {
    FunctionVariation::new_native(
        |values| {
            let values = values.unwrap_singles();
            if values.len() != 2 {
                return Err(runtime_error("eq() expects 2 arguments".to_string()));
            }
            Ok(Value::Boolean(values[0] == values[1]))
        },
        FunctionParameterType::Singles(vec![
            ("lhs".to_string(), std_primitive_types::ANY),
            ("rhs".to_string(), std_primitive_types::ANY),
        ]),
        std_primitive_types::BOOL,
    )
}
