use std::borrow::Borrow;

use crate::{
    interpreter::value::NativeFunction,
    type_checker::{
        checked_ast::{CheckedAst, CheckedModule},
        types::{GetType, Type},
    },
    util::str::Str,
};

use super::{
    environment::Environment,
    error::{runtime_error, RuntimeError},
    value::{Function, UserFunction, Value},
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
        CheckedAst::Call { function, arg, .. } => {
            // eval_call(name.clone(), variation, &eval_arguments(args, env)?, env)?
            eval_call(function, arg, env)?
        }
        // CheckedAst::DirectCall { variation, args } => {
        //     // eval_call(name.clone(), variation, &eval_arguments(args, env)?, env)?
        //     eval_variation_call(variation, &eval_arguments(args, env)?, env)?
        // }
        CheckedAst::Tuple(v, _) => eval_tuple(v, env)?,
        CheckedAst::Literal(l) => l.clone(),
        CheckedAst::Identifier(id, _) => match env.lookup_identifier(id) {
            (Some(_), Some(_)) => {
                return Err(runtime_error(format!("Ambiguous identifier '{}'", id)))
            }
            (Some(v), _) => v.clone(),
            (_, Some(f)) => Value::Function(Box::new(f.clone())),
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
        CheckedAst::Function(func) => Value::Function(Box::new(Function::new_user(
            func.param.clone(),
            func.body.clone(),
            env.deep_clone(),
            func.return_type.clone(),
        ))),
        // Value::Variation(Box::new(Function::new_user(
        //     params.clone(),
        //     *body.clone(),
        //     env.deep_clone(),
        //     return_type.clone(),
        // ))),
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

fn eval_call(
    // name: Option<Str>,
    function: &CheckedAst,
    arg: &CheckedAst,
    env: &mut Environment,
) -> InterpretResult {
    /// Unwrap a native function call
    /// Returns a tuple of the native function and the arguments
    /// If the expression is not a native function, return None
    ///
    /// ## Memory
    /// This function is recursive and will consume stack memory
    /// proportional to the depth of the function call.
    /// However it will *not* consume any heap memory due to the
    /// use of references.
    fn unwrap_native<'a, 'b>(
        expr: &'b CheckedAst,
        mut args: Vec<&'b CheckedAst>,
        env: &'a mut Environment,
    ) -> Option<(&'a NativeFunction, Vec<&'b CheckedAst>)> {
        match expr {
            CheckedAst::Identifier(id, _) => match env.lookup_function(id) {
                Some(Function::Native(native)) => Some((native, args)),
                _ => None,
            },
            CheckedAst::Call { function, arg, .. } => {
                args.push(arg);
                // Recurse until we find a native function (if any)
                unwrap_native(function, args, env)
            }
            _ => None,
        }
    }

    // First try to unwrap any native function call
    if let Some((native, args)) = unwrap_native(function, vec![arg], env) {
        // We only allow fully-applied native functions
        if args.len() != native.params.len() {
            return Err(runtime_error(format!(
                "Expected {} arguments, found {} when calling native function '{}'",
                native.params.len(),
                args.len(),
                native.name
            )));
        }
        // Extract the handler from the native function,
        // so that the lifetime of the `native` ref is
        // dropped before we use `env` again to evaluate
        // the arguments.
        let handler = native.handler;
        let args = args
            .iter()
            .map(|arg| interpret_ast(arg, env))
            .collect::<Result<Vec<Value>, _>>()?;
        // Invoke the native function
        return handler(args);
    }

    // TODO: Implement support for function overloading (multiple variations)
    // let variation = {
    //     let Some(variation) = env.lookup_function(function) else {
    //         return Err(runtime_error(format!("Unknown function '{}'", function)));
    //     };

    //     // // Get the variation of the given type
    //     // let Some(variation) = variation.get_variation(variation_type) else {
    //     //     // ! Compiler panic! expected type checker to catch this
    //     //     unreachable!(
    //     //         "Function variation of type {:?} not found! Fix the type checker",
    //     //         variation_type
    //     //     );
    //     // };
    //     variation.clone()
    // };

    let function = interpret_ast(function, env)?;
    let Value::Function(function) = function else {
        unreachable!("This should have been checked by the type checker");
    };

    // Evaluate the function body or invoke the native handler
    match function.borrow() {
        Function::User(UserFunction { body, param, .. }) => {
            let arg = interpret_ast(arg, env)?;
            // let name = name.unwrap_or(Str::Str("anonymous"));
            let mut closure = env.new_child(Str::Str("<closure>"));
            // Zip and add parameters and arguments as constants in the environment
            // match variation.get_param() {
            //     FunctionParameterType::Singles(params) => {
            //         // Bind the arguments to the parameters of the function variation
            //         for (i, (name, _)) in params.iter().enumerate() {
            //             closure.add_value(Str::String(name.to_string()), args[i].clone())?;
            //         }
            //     }
            //     FunctionParameterType::Variadic(params, (var_name, var_type)) => {
            //         // Bind the arguments to the parameters of the function variation
            //         for (i, (name, _)) in params.iter().enumerate() {
            //             closure.add_value(Str::String(name.to_string()), args[i].clone())?;
            //         }
            //         // Collect rest of the arguments into a list and bind it to the variadic parameter
            //         let rest = args[params.len()..].to_vec();
            //         closure.add_value(
            //             Str::String(var_name.to_string()),
            //             Value::List(rest, Type::List(Box::new(var_type.clone()))),
            //         )?;
            //     }
            // };

            // Bind the arguments to the parameters of the function variation
            closure.add_value(Str::String(param.name.clone()), arg.clone())?;

            // Native functions must not reach this level!!!
            interpret_ast(body, &mut closure)
        }
        // Function::Native { handler, .. } => {
        //     // Setup NativeFunctionParameters and invoke the handler
        //     let args_native: NativeFunction = match variation.get_param() {
        //         FunctionParameterType::Singles(_) => NativeFunction::Singles(args.to_vec()),
        //         FunctionParameterType::Variadic(params, _) => {
        //             // Create a new vec to store the same number of arguments as the params length
        //             // Then place the rest of the arguments into the variadic parameter
        //             // Assume that the type checker has already checked that the variadic parameter is a list
        //             let singles = args[0..params.len()].to_vec();
        //             let variadic = args[params.len()..].to_vec();
        //             NativeFunction::Variadic(singles, variadic)
        //         }
        //     };
        //     handler(args_native)
        // }
        Function::Native { .. } => {
            unreachable!("Native functions must not reach this!!!");
        }
    }
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
