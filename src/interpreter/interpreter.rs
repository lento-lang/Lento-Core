use crate::parser::ast::Ast;

use super::{value::Value, error::{RuntimeError, runtime_error}};

fn eval_function_call(name: String, args: Vec<Ast>) -> Result<Value, RuntimeError> {
    Ok(match name.as_str() {
        "print" => {
            if args.len() != 1 {
                return Err(runtime_error(format!("Expected 1 argument, got {}", args.len())));
            }
            let arg = args.get(0).unwrap();
            println!("{}", interpret_ast(arg)?);
            Value::Unit
        },
        _ => panic!("Call to unknown function: {}", name)
    })
}

pub fn interpret_ast(ast: &Ast) -> Result<Value, RuntimeError> {
    Ok(match ast.to_owned() {
        Ast::FunctionCall(name, args) => eval_function_call(name, args)?,
        Ast::Tuple(v) => {
            if v.len() == 0 {
                Value::Unit
            } else {
                Value::Tuple(v.into_iter().map(|e| interpret_ast(&e)).collect::<Result<Vec<Value>, _>>()?)
            }
        },
        Ast::String(s) => Value::String(s),
        _ => todo!("Implement other AST nodes")
    })
}
