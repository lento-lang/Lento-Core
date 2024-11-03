use crate::{
    type_checker::{
        checked_ast::{CheckedAst, CheckedModule},
        types::{FunctionParameterType, GetType, Type},
    },
    util::str::Str,
};

use super::{
    environment::Environment,
    error::{runtime_error, RuntimeError},
    value::{Function, FunctionVariation, NativeFunctionParameters, Value},
};

//--------------------------------------------------------------------------------------//
//                                     Interpreter                                      //
//--------------------------------------------------------------------------------------//

pub type InterpretResult = Result<Value, RuntimeError>;

fn eval_function_variation_invocation(
    name: Option<Str>,
    variation: &FunctionVariation,
    args: &[Value],
    env: &mut Environment,
) -> InterpretResult {
    // Bind the arguments to the parameters of the function variation
    let variation_params = variation.get_params();

    // Evaluate the function body or invoke the native handler
    match variation {
        FunctionVariation::User { body, .. } => {
            let name = name.unwrap_or(Str::Str("anonymous"));
            let mut new_env = env.new_child(Str::String(format!("Function closure: {}", name)));
            // Zip and add parameters and arguments as constants in the environment
            match variation_params {
                FunctionParameterType::Singles(params) => {
                    for (i, (name, _)) in params.iter().enumerate() {
                        new_env.add_value(Str::String(name.to_string()), args[i].clone())?;
                    }
                }
                FunctionParameterType::Variadic(params, (var_name, var_type)) => {
                    for (i, (name, _)) in params.iter().enumerate() {
                        new_env.add_value(Str::String(name.to_string()), args[i].clone())?;
                    }
                    // Collect rest of the arguments into a list and bind it to the variadic parameter
                    let rest = args[params.len()..].to_vec();
                    new_env.add_value(
                        Str::String(var_name.to_string()),
                        Value::List(rest, Type::List(Box::new(var_type.clone()))),
                    )?;
                }
            };
            interpret_ast(body, &mut new_env)
        }
        FunctionVariation::Native { handler, .. } => {
            // Setup NativeFunctionParameters and invoke the handler
            let args_native: NativeFunctionParameters = match variation_params {
                FunctionParameterType::Singles(_) => {
                    NativeFunctionParameters::Singles(args.to_vec())
                }
                FunctionParameterType::Variadic(params, _) => {
                    // Create a new vec to store the same number of arguments as the params length
                    // Then place the rest of the arguments into the variadic parameter
                    // Assume that the type checker has already checked that the variadic parameter is a list
                    let singles = args[0..params.len()].to_vec();
                    let variadic = args[params.len()..].to_vec();
                    NativeFunctionParameters::Variadic(singles, variadic)
                }
            };
            handler(args_native)
        }
    }
}

// Evaluate the arguments
fn eval_arguments(args: &[CheckedAst], env: &mut Environment) -> Result<Vec<Value>, RuntimeError> {
    args.iter().map(|a| interpret_ast(a, env)).collect()
}

/// Assume `elems` are a non-empty vector
fn eval_tuple(elems: &[CheckedAst], env: &mut Environment) -> InterpretResult {
    if elems.is_empty() {
        return Ok(Value::Unit);
    }
    let (values, types): (Vec<Value>, Vec<Type>) = elems
        .iter()
        .map(|e| {
            let value = interpret_ast(e, env)?;
            let ty = value.get_type().clone();
            Ok((value, ty))
        })
        .collect::<Result<Vec<(Value, Type)>, _>>()?
        .into_iter()
        .unzip();

    Ok(Value::Tuple(values, Type::Tuple(types)))
}

/// Interpret an AST node
///
/// ## Note
/// ! All nodes in the AST are assumed to be type-checked before being interpreted!
pub fn interpret_ast(ast: &CheckedAst, env: &mut Environment) -> InterpretResult {
    Ok(match ast {
        CheckedAst::VariationCall(name, variation, args, _) => eval_function_variation_invocation(
            name.clone(),
            variation,
            &eval_arguments(args, env)?,
            env,
        )?,
        CheckedAst::Tuple(v, _) => eval_tuple(v, env)?,
        CheckedAst::Literal(l) => l.clone(),
        CheckedAst::Identifier(id, _) => match env.lookup_identifier(id) {
            (Some(_), Some(_)) => {
                return Err(runtime_error(format!("Ambiguous identifier '{}'", id)))
            }
            (Some(v), _) => v.clone(),
            (_, Some(f)) => Value::Function(f.clone()),
            (None, None) => return Err(runtime_error(format!("Unknown identifier '{}'", id))),
        },
        CheckedAst::Binary(lhs, op, rhs, _) => {
            let lhs = interpret_ast(lhs, env)?;
            let rhs = interpret_ast(rhs, env)?;
            eval_function_variation_invocation(
                Some(op.name.clone().into()),
                &op.handler,
                &[lhs, rhs],
                env,
            )?
        }
        CheckedAst::Assignment(lhs, rhs, _) => {
            let lhs = match *lhs.to_owned() {
                CheckedAst::Identifier(id, _) => id,
                _ => {
                    return Err(runtime_error(
                        "Assignment expects an identifier".to_string(),
                    ))
                }
            };
            let rhs = interpret_ast(rhs, env)?;
            env.add_value(Str::String(lhs), rhs.clone())?;
            rhs
        }
        CheckedAst::List(elems, ty) => {
            let values = elems
                .iter()
                .map(|e| interpret_ast(e, env))
                .collect::<Result<Vec<Value>, _>>()?;
            Value::List(values, ty.clone())
        }
        CheckedAst::Record(expr, ty) => {
            let mut record = Vec::new();
            for (key, value) in expr {
                let value = interpret_ast(value, env)?;
                record.push((key.clone(), value));
            }
            Value::Record(record, ty.clone())
        }
        CheckedAst::FunctionDecl(name, func) => {
            if let Some(name) = name {
                // Add a new variation to the local environment
                // Failing if a variation of the same signature already exists
                Value::Function(
                    env.add_local_function_variation(
                        name,
                        FunctionVariation::new_user(
                            func.params.clone(),
                            *func.body.clone(),
                            func.return_type.clone(),
                        ),
                    )?
                    .clone(),
                )
            } else {
                // Anonymous function
                Value::Function(Function::new(
                    "anon".to_string(),
                    vec![FunctionVariation::new_user(
                        func.params.clone(),
                        *func.body.clone(),
                        func.return_type.clone(),
                    )],
                ))
            }
        }
        CheckedAst::Unary(_, _, _) => todo!("Implement Unary AST node: {:?}", ast),
        CheckedAst::Block(exprs, _) => {
            let mut result = Value::Unit;
            for expr in exprs {
                result = interpret_ast(expr, env)?;
            }
            result
        }
    })
}

pub fn interpret_module(module: &CheckedModule, env: &mut Environment) -> InterpretResult {
    let mut result = Value::Unit;
    for expr in &module.expressions {
        result = interpret_ast(expr, env)?;
    }
    Ok(result)
}
