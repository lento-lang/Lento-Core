use crate::{
    lexer::op::{Operator, StaticOperatorAst},
    parser::ast::{Ast, Module},
    type_checker::types::{FunctionParameterType, GetType, Type},
    util::str::Str,
};

use super::{
    environment::Environment,
    error::{runtime_error, RuntimeError},
    value::{compare_function_variations, FunctionVariation, NativeFunctionParameters, Value},
};

//--------------------------------------------------------------------------------------//
//                                     Interpreter                                      //
//--------------------------------------------------------------------------------------//

pub type InterpretResult = Result<Value, RuntimeError>;

fn eval_function_variation_invocation(
    name: String,
    variation: FunctionVariation,
    arg_asts: Vec<Ast>,
    env: &mut Environment,
) -> InterpretResult {
    // Bind the arguments to the parameters of the function variation
    let var_params = variation.get_params();

    // Evaluate the arguments
    let mut arg_vals: Vec<Value> = vec![];
    for a in arg_asts {
        arg_vals.push(interpret_ast(&a, env)?);
    }

    // Evaluate the function body or invoke the native handler
    match variation.clone() {
        FunctionVariation::User(_, body, _) => {
            let mut new_env = env.new_child(Str::String(format!("Function closure: {}", name)));
            // Zip and add parameters and arguments as constants in the environment
            match var_params {
                FunctionParameterType::Singles(params) => {
                    for (i, (name, _)) in params.iter().enumerate() {
                        new_env.add_value(Str::String(name.to_string()), arg_vals[i].clone())?;
                    }
                }
                FunctionParameterType::Variadic(params, (var_name, var_type)) => {
                    for (i, (name, _)) in params.iter().enumerate() {
                        new_env.add_value(Str::String(name.to_string()), arg_vals[i].clone())?;
                    }
                    // Collect rest of the arguments into a list and bind it to the variadic parameter
                    let rest = arg_vals[params.len()..].to_vec();
                    new_env.add_value(
                        Str::String(var_name.to_string()),
                        Value::List(rest, Type::List(Box::new(var_type.clone()))),
                    )?;
                }
            };
            interpret_ast(&body, &mut new_env)
        }
        FunctionVariation::Native(handler, _, _) => {
            // Setup NativeFunctionParameters and invoke the handler
            let args_native: NativeFunctionParameters = match var_params {
                FunctionParameterType::Singles(_) => NativeFunctionParameters::Singles(arg_vals),
                FunctionParameterType::Variadic(params, _) => {
                    // Create a new vec to store the same number of arguments as the params length
                    // Then place the rest of the arguments into the variadic parameter
                    // Assume that the type checker has already checked that the variadic parameter is a list
                    let singles = arg_vals[0..params.len()].to_vec();
                    let variadic = arg_vals[params.len()..].to_vec();
                    NativeFunctionParameters::Variadic(singles, variadic)
                }
            };
            handler(args_native)
        }
    }
}

fn eval_function_call(name: String, arg_asts: Vec<Ast>, env: &mut Environment) -> InterpretResult {
    // Look for the name in the environment
    match env.get_value(&name) {
        Some(Value::Function(f)) => {
            // Find a matching variation to the call signature (only arguments are considered)
            // Bind the arguments to the function's parameters
            // Evaluate the function body
            // Return the result

            let mut found: Option<FunctionVariation> = None;
            let mut variations = f.variations.clone();
            variations.sort_by(compare_function_variations);
            for variation in variations {
                if !variation.get_params().match_args(&arg_asts) {
                    continue;
                }
                found = Some(variation);
                break;
            }
            // Bind the arguments to the parameters
            if found.is_none() {
                return Err(runtime_error(format!(
                    "No matching function variation for {}",
                    name
                )));
            }
            eval_function_variation_invocation(name, found.unwrap(), arg_asts, env)
        }
        Some(_) => Err(runtime_error(format!("{} is not a function", name))),
        None => Err(runtime_error(format!("Unknown function: {}", name))),
    }
}

/**
 * Assume `elems` are a non-empty vector
 */
fn eval_tuple(elems: Vec<Ast>, env: &mut Environment) -> InterpretResult {
    let (values, types): (Vec<Value>, Vec<Type>) = elems
        .iter()
        .map(|e| {
            let value = interpret_ast(e, env)?;
            let value_type = value.get_type().unwrap_checked().clone();
            Ok((value, value_type))
        })
        .collect::<Result<Vec<(Value, Type)>, _>>()?
        .into_iter()
        .unzip();

    Ok(Value::Tuple(values, Type::Tuple(types)))
}

/**
 * Interpret an AST node
 * ! All nodes in the AST are assumed to be type-checked before being interpreted !
 */
pub fn interpret_ast(ast: &Ast, env: &mut Environment) -> InterpretResult {
    Ok(match ast.to_owned() {
        Ast::FunctionCall(name, args, _) => eval_function_call(name, args, env)?,
        Ast::Tuple(v, _) => {
            if v.is_empty() {
                Value::Unit
            } else {
                eval_tuple(v, env)?
            }
        }
        Ast::Literal(l) => l,
        Ast::Identifier(id, _) => match env.get_value(&id) {
            Some(v) => v,
            None => return Err(runtime_error(format!("Unknown identifier: '{}'", id))),
        },
        Ast::Binary(lhs, op, rhs, _) => match op {
            Operator::Runtime(rt) => {
                eval_function_variation_invocation(rt.name, *rt.handler, vec![*lhs, *rhs], env)?
            }
            Operator::Static(st) => {
                let sop = StaticOperatorAst::Infix(*lhs, *rhs);
                let transformed = (st.handler)(sop);
                interpret_ast(&transformed, env)?
            }
        },
        Ast::Assignment(lhs, rhs, _) => {
            let lhs = match *lhs {
                Ast::Identifier(id, _) => id,
                _ => {
                    return Err(runtime_error(
                        "Assignment expects an identifier".to_string(),
                    ))
                }
            };
            let rhs = interpret_ast(&rhs, env)?;
            env.add_value(Str::String(lhs), rhs.clone())?;
            rhs
        }
        _ => todo!("Implement AST node"),
    })
}

pub fn interpret_module(module: &Module, env: &mut Environment) -> InterpretResult {
    let mut result = Value::Unit;
    for expr in &module.expressions {
        result = interpret_ast(expr, env)?;
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::value::{Number, UnsignedInteger},
        stdlib::arithmetic,
        type_checker::types::{std_primitive_types, CheckedType},
    };

    use super::*;

    fn make_u8(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(n)))
    }

    #[test]
    fn test_interpret_binary_add() {
        let mut env = Environment::new(Str::Str("test"));
        let ast = Ast::Binary(
            Box::new(Ast::Literal(make_u8(1))),
            Operator::Runtime(arithmetic::op_add()),
            Box::new(Ast::Literal(make_u8(2))),
            CheckedType::Unchecked, // Not required for this test
        );
        // TODO: let checked_ast = type_check(&ast).unwrap();
        let result = interpret_ast(&ast, &mut env).unwrap();
        assert_eq!(
            result.get_type().unwrap_checked().clone(),
            std_primitive_types::UINT8
        );
        assert_eq!(result, make_u8(3));
    }
}
