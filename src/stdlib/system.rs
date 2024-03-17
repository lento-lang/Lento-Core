use crate::{
    interpreter::value::{FunctionVariation, NativeFunctionParameters, Value},
    type_checker::types::{std_primitive_types, FunctionParameterType, Type},
};

const TY_ANY: Type = std_primitive_types::ANY;
const TY_UNIT: Type = std_primitive_types::UNIT;

//--------------------------------------------------------------------------------------//
//                               Native Runtime Functions                               //
//--------------------------------------------------------------------------------------//

pub fn print() -> FunctionVariation {
    FunctionVariation::Native(
        |vals| {
            let vals = if let NativeFunctionParameters::Variadic(_, v) = vals {
                v
            } else {
                panic!("A native function with Variadic function parameter type should not be able to receive a Singles function parameter type")
            };
            for val in vals {
                println!("{}", val);
            }
            Ok(Value::Unit)
        },
        FunctionParameterType::Variadic(vec![], ("params".to_string(), TY_ANY)),
        TY_UNIT,
    )
}
