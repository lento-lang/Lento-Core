use crate::{parser::ast::Ast, type_checker::types::{Type, GetType, FunctionParameterType}, util::{str::Str, failable::Failable}};

use super::{value::{Value, FunctionVariation, compare_function_variations, NativeFunctionParameters}, error::{RuntimeError, runtime_error}, environment::Environment};


pub type InterpretResult = Result<Value, RuntimeError>;


fn eval_function_call(name: String, arg_asts: Vec<Ast>, env: &Environment) -> InterpretResult {
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
                if !variation.get_params().match_args(&arg_asts) { continue; }
                found = Some(variation);
                break;
            }
            // Bind the arguments to the parameters
            if found.is_none() { return Err(runtime_error(format!("No matching function variation for {}", name))); }

            // Bind the arguments to the parameters of the function variation
            let variation = found.unwrap();
            let var_params = variation.get_params();

            // Evaluate the arguments
            let mut arg_vals: Vec<Value> = vec![];
            for a in arg_asts { arg_vals.push(interpret_ast(&a, env)?); }

            // Evaluate the function body or invoke the native handler
            match variation.clone() {
                FunctionVariation::User(_, body, _) => {
                    let mut new_env = env.new_child(Str::String(format!("Function closure: {}", name)));
                    match var_params {
                        FunctionParameterType::Singles(params) => {
                            for (i, (name, _)) in params.iter().enumerate() {
                                new_env.add_value(Str::String(name.to_string()), arg_vals[i].clone())?;
                            }
                        },
                        FunctionParameterType::Variadic(params, (var_name, var_type)) => {
                            for (i, (name, _)) in params.iter().enumerate() {
                                new_env.add_value(Str::String(name.to_string()), arg_vals[i].clone())?;
                            }
                            // Collect rest of the arguments into a list and bind it to the variadic parameter
                            let mut rest = arg_vals[params.len()..].to_vec();
                            new_env.add_value(Str::String(var_name.to_string()), Value::List(rest, Type::List(Box::new(var_type.clone()))))?;
                        }
                    };
                    interpret_ast(&body, &new_env)
                },
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
        },
        Some(_) => Err(runtime_error(format!("{} is not a function", name))),
        None => Err(runtime_error(format!("Unknown function: {}", name)))
    }
}

/**
 * Assume `elems` are a non-empty vector
 */
fn eval_tuple(elems: Vec<Ast>, env: &Environment) -> InterpretResult {
    let values = elems.iter().map(|e| interpret_ast(e, env)).collect::<Result<Vec<Value>, _>>()?;
    let mut type_ = vec![Type::Any; values.len()];
    for (i, v) in values.iter().enumerate() {
        if let Some(t) = v.get_type() { type_[i] = t; }
        else { return Err(runtime_error(format!("Cannot infer type of tuple element {}", i))); }
    }
    Ok(Value::Tuple(values, Type::Tuple(type_)))
}

/**
 * Interpret an AST node
 * ! All nodes in the AST are assumed to be type-checked before being interpreted !
 */
pub fn interpret_ast(ast: &Ast, env: &Environment) -> InterpretResult {
    Ok(match ast.to_owned() {
        Ast::FunctionCall(name, args, _) => eval_function_call(name, args, env)?,
        Ast::Tuple(v, _) => {
            if v.len() == 0 { Value::Unit }
            else { eval_tuple(v, env)? }
        },
        Ast::Literal(l) => l,
        Ast::Identifier(id, _) => {
            match env.get_value(&id) {
                Some(v) => v,
                None => return Err(runtime_error(format!("Unknown identifier: '{}'", id)))
            }
        },
        _ => todo!("Implement other AST nodes")
    })
}
