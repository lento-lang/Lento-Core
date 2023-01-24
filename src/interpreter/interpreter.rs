use crate::{parser::ast::Ast, type_checker::types::{Type, GetType}};

use super::{value::Value, error::{RuntimeError, runtime_error}, environment::Environment};

pub type InterpretResult = Result<Value, RuntimeError>;

fn eval_function_call(name: String, args: Vec<Ast>, env: &Environment) -> InterpretResult {
    Ok(match name.as_str() {
        "print" => {
            if args.len() != 1 {
                return Err(runtime_error(format!("Expected 1 argument, got {}", args.len())));
            }
            let arg = args.get(0).unwrap();
            println!("{}", interpret_ast(arg, env)?);
            Value::Unit
        },
        _ => panic!("Call to unknown function: {}", name)
    })
}

/**
 * Assume `elems` are a non-empty vector
 */
fn eval_tuple(elems: Vec<Ast>, env: &Environment) -> InterpretResult {
    let values = elems.iter().map(|e| interpret_ast(e, env)).collect::<Result<Vec<Value>, _>>()?;
    let typle = values.iter().map(Value::get_type).collect::<Vec<Type>>();
    Ok(Value::Tuple(values, Type::Tuple(typle)))
}

pub fn interpret_ast(ast: &Ast, env: &Environment) -> InterpretResult {
    Ok(match ast.to_owned() {
        Ast::FunctionCall(name, args, _) => eval_function_call(name, args, env)?,
        Ast::Tuple(v, _) => {
            if v.len() == 0 { Value::Unit }
            else { eval_tuple(v, env)? }
        },
        Ast::String(s) => Value::String(s),
        _ => todo!("Implement other AST nodes")
    })
}
