use crate::{
    type_checker::{
        checked_ast::{CheckedAst, CheckedModule},
        types::{FunctionParameterType, GetType, Type, VariationType},
    },
    util::str::Str,
};

use super::{
    environment::Environment,
    error::{runtime_error, RuntimeError},
    value::{FunctionVariation, NativeFunctionParameters, UserFunctionVariation, Value},
};

//--------------------------------------------------------------------------------------//
//                                     Interpreter                                      //
//--------------------------------------------------------------------------------------//

pub type InterpretResult = Result<Value, RuntimeError>;

/// Interpret a module
pub fn interpret_module(module: &CheckedModule, env: &mut Environment) -> InterpretResult {
    let mut result = Value::Unit;
    for expr in &module.expressions {
        result = interpret_ast(expr, env)?;
    }
    Ok(result)
}

/// Interpret a type-checked AST node
pub fn interpret_ast(ast: &CheckedAst, env: &mut Environment) -> InterpretResult {
    Ok(match ast {
        CheckedAst::Call {
            function,
            variation,
            args,
        } => {
            // eval_call(name.clone(), variation, &eval_arguments(args, env)?, env)?
            eval_call(function, variation, &eval_arguments(args, env)?, env)?
        }
        CheckedAst::DirectCall { variation, args } => {
            // eval_call(name.clone(), variation, &eval_arguments(args, env)?, env)?
            eval_variation_call(variation, &eval_arguments(args, env)?, env)?
        }
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
        // CheckedAst::Binary(lhs, op, rhs, _) => {
        //     let lhs = interpret_ast(lhs, env)?;
        //     let rhs = interpret_ast(rhs, env)?;
        //     eval_call(&op.name, &op.handler, &[lhs, rhs], env)?
        // }
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
        CheckedAst::Function {
            params,
            body,
            return_type,
        } => {
            // if let Some(name) = name {
            //     // Add a new variation to the local environment
            //     // Failing if a variation of the same signature already exists
            //     Value::Function(
            //         env.add_function_variation(
            //             name,
            //             FunctionVariation::new_user(
            //                 func.params.clone(),
            //                 *func.body.clone(),
            //                 func.return_type.clone(),
            //             ),
            //         )?
            //         .clone(),
            //     )
            // } else {
            //     // Anonymous function
            //     Value::Function(Function::new(
            //         "anon".to_string(),
            //         vec![FunctionVariation::new_user(
            //             func.params.clone(),
            //             *func.body.clone(),
            //             func.return_type.clone(),
            //         )],
            //     ))
            // }
            Value::Variation(Box::new(FunctionVariation::new_user(
                params.clone(),
                *body.clone(),
                env.deep_clone(),
                return_type.clone(),
            )))
        }
        // CheckedAst::Unary(_, _, _) => todo!("Implement Unary AST node: {:?}", ast),
        CheckedAst::Block(exprs, _) => {
            let mut result = Value::Unit;
            let mut scope = env.new_child(Str::Str("<block>"));
            for expr in exprs {
                result = interpret_ast(expr, &mut scope)?;
            }
            result
        }
    })
}

fn eval_variation_call(
    variation: &FunctionVariation,
    args: &[Value],
    env: &mut Environment,
) -> InterpretResult {
    // match variation {
    //     FunctionVariation::Native(native) => {
    //         let params = native.params.clone();
    //         let params = NativeFunctionParameters::new(params, args.to_vec());
    //         (native.handler)(&params)
    //     }
    //     FunctionVariation::User(user) => {
    //         let mut child_env = env.new_child(user.env.clone());
    //         for (param, arg) in user.params.iter().zip(args.iter()) {
    //             child_env.add_value(param.clone(), arg.clone())?;
    //         }
    //         interpret_ast(&user.body, &mut child_env)
    //     }
    // }
    match variation {
        FunctionVariation::User(UserFunctionVariation { body, .. }) => {
            // let name = name.unwrap_or(Str::Str("anonymous"));
            let mut closure = env.new_child(Str::Str("<closure>"));
            // Zip and add parameters and arguments as constants in the environment
            match variation.get_params() {
                FunctionParameterType::Singles(params) => {
                    // Bind the arguments to the parameters of the function variation
                    for (i, (name, _)) in params.iter().enumerate() {
                        closure.add_value(Str::String(name.to_string()), args[i].clone())?;
                    }
                }
                FunctionParameterType::Variadic(params, (var_name, var_type)) => {
                    // Bind the arguments to the parameters of the function variation
                    for (i, (name, _)) in params.iter().enumerate() {
                        closure.add_value(Str::String(name.to_string()), args[i].clone())?;
                    }
                    // Collect rest of the arguments into a list and bind it to the variadic parameter
                    let rest = args[params.len()..].to_vec();
                    closure.add_value(
                        Str::String(var_name.to_string()),
                        Value::List(rest, Type::List(Box::new(var_type.clone()))),
                    )?;
                }
            };
            interpret_ast(body, &mut closure)
        }
        FunctionVariation::Native { handler, .. } => {
            // Setup NativeFunctionParameters and invoke the handler
            let args_native: NativeFunctionParameters = match variation.get_params() {
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

fn eval_call(
    // name: Option<Str>,
    function: &str,
    variation_type: &VariationType,
    args: &[Value],
    env: &mut Environment,
) -> InterpretResult {
    let variation = {
        let Some(variation) = env.lookup_function(function) else {
            return Err(runtime_error(format!("Unknown function '{}'", function)));
        };

        // Get the variation of the given type
        let Some(variation) = variation.get_variation(variation_type) else {
            // ! Compiler panic! expected type checker to catch this
            unreachable!(
                "Function variation of type {:?} not found! Fix the type checker",
                variation_type
            );
        };
        variation.clone()
    };

    // Evaluate the function body or invoke the native handler
    eval_variation_call(&variation, args, env)
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
