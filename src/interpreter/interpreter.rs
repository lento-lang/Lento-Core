use crate::{
    parser::ast::{Ast, Module},
    type_checker::types::{CheckedType, FunctionParameterType, GetType, Type},
    util::str::Str,
};

use super::{
    environment::Environment,
    error::{runtime_error, RuntimeError},
    value::{FunctionVariation, NativeFunctionParameters, Value},
};

//--------------------------------------------------------------------------------------//
//                                     Interpreter                                      //
//--------------------------------------------------------------------------------------//

pub type InterpretResult = Result<Value, RuntimeError>;

fn eval_function_variation_invocation(
    name: Option<&str>,
    variation: &FunctionVariation,
    args: &[Value],
    env: &mut Environment,
) -> InterpretResult {
    // Bind the arguments to the parameters of the function variation
    let variation_params = variation.get_params();

    // Evaluate the function body or invoke the native handler
    match variation {
        FunctionVariation::User { body, .. } => {
            let name = name.unwrap_or("anonymous");
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
fn eval_arguments(args: &[Ast], env: &mut Environment) -> Result<Vec<Value>, RuntimeError> {
    args.iter().map(|a| interpret_ast(a, env)).collect()
}

fn eval_function_call(name: &str, args: &[Value], env: &mut Environment) -> InterpretResult {
    // Look for the name in the environment
    match env.get_value(name) {
        Some(Value::Function(f)) => {
            // Find a matching variation to the call signature (only arguments are considered)
            // Bind the arguments to the function's parameters
            // Evaluate the function body
            // Return the result

            let mut found: Option<&FunctionVariation> = None;
            for variation in f.get_variations() {
                if !variation.get_params().match_args(args) {
                    continue;
                }
                found = Some(variation);
                break;
            }
            // Bind the arguments to the parameters
            if let Some(found) = found {
                eval_function_variation_invocation(Some(name), found, args, env)
            } else {
                Err(runtime_error(format!(
                    "No matching function variation for {}",
                    name
                )))
            }
        }
        Some(_) => Err(runtime_error(format!("{} is not a function", name))),
        None => Err(runtime_error(format!("Unknown function: {}", name))),
    }
}

/// Assume `elems` are a non-empty vector
fn eval_tuple(elems: &[Ast], env: &mut Environment) -> InterpretResult {
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
pub fn interpret_ast(ast: &Ast, env: &mut Environment) -> InterpretResult {
    // Assert that the type is checked!
    debug_assert!(
        matches!(ast.get_checked_type(), CheckedType::Checked(_)),
        "Type is not checked! {:?}",
        ast
    );
    Ok(match ast {
        Ast::FunctionCall(name, args, _) => {
            eval_function_call(name, &eval_arguments(args, env)?, env)?
        }
        Ast::VariationCall(variation, args, _) => {
            eval_function_variation_invocation(None, variation, &eval_arguments(args, env)?, env)?
        }
        Ast::Tuple(v, _) => {
            if v.is_empty() {
                Value::Unit
            } else {
                eval_tuple(v, env)?
            }
        }
        Ast::Literal(l) => l.clone(),
        Ast::Identifier(id, _) => match env.get_value(id) {
            Some(v) => v,
            None => return Err(runtime_error(format!("Unknown identifier '{}'", id))),
        },
        Ast::Binary(lhs, op, rhs, _) => {
            let lhs = interpret_ast(lhs, env)?;
            let rhs = interpret_ast(rhs, env)?;
            eval_function_variation_invocation(Some(&op.name), &op.handler, &[lhs, rhs], env)?
        }
        Ast::Assignment(lhs, rhs, _) => {
            let lhs = match *lhs.to_owned() {
                Ast::Identifier(id, _) => id,
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
        Ast::Type(ty) => Value::Type(ty.clone()),
        Ast::List(elems, ty) => {
            let values = elems
                .iter()
                .map(|e| interpret_ast(e, env))
                .collect::<Result<Vec<Value>, _>>()?;
            Value::List(values, ty.unwrap_checked_ref().clone())
        }
        Ast::Record(_, _) => todo!("Implement Record AST node: {:?}", ast),
        Ast::Function(_) => todo!("Implement Function AST node: {:?}", ast),
        Ast::Unary(_, _, _) => todo!("Implement Unary AST node: {:?}", ast),
        Ast::Block(_, _) => todo!("Implement Block AST node: {:?}", ast),
    })
}

pub fn interpret_module(module: &Module, env: &mut Environment) -> InterpretResult {
    let mut result = Value::Unit;
    for expr in &module.expressions {
        result = interpret_ast(expr, env)?;
    }
    Ok(result)
}
