use std::{io::{BufRead, Seek}, vec};

use crate::{lexer::op::{Operator, RuntimeOperator, StaticOperator, OperatorAssociativity, StaticOperatorHandler, RuntimeOperatorHandler, StaticOperatorAst, OperatorPosition}, parser::ast::Ast, interpreter::{value::{FunctionVariation, NativeFunctionParameters, Value, Number, SignedInteger, Function, FloatingPoint}, error::{runtime_error, RuntimeError}, environment::Environment}, type_checker::types::{Type, FunctionParameterType, std_primitive_types, GetType}, util::{str::Str, failable::Failable}};

use super::super::lexer::lexer::Lexer;


const TY_ANY: Type = std_primitive_types::ANY;
const TY_INT: Type = std_primitive_types::INT32;
const TY_UNIT: Type = std_primitive_types::UNIT;

//--------------------------------------------------------------------------------------//
//                                                                                      //
//                                      Init Lexer                                      //
//                                                                                      //
//--------------------------------------------------------------------------------------//


pub fn init_lexer<R: BufRead + Seek>(lexer: &mut Lexer<R>) {

//--------------------------------------------------------------------------------------//
//                                       Helpers                                        //
//--------------------------------------------------------------------------------------//

    let add_static = |lexer: &mut Lexer<R>, name: &str, sym: &str, pos: OperatorPosition, prec: u16, assoc: OperatorAssociativity, overloadable: bool, handler: StaticOperatorHandler| {
        lexer.define_op(Operator::Static(StaticOperator::new_str(name, sym, pos, prec, assoc, overloadable, handler)));
    };
    let add_runtime = |lexer: &mut Lexer<R>, name: &str, sym: &str, pos: OperatorPosition, prec: u16, assoc: OperatorAssociativity, overloadable: bool, handler: RuntimeOperatorHandler| {
        lexer.define_op(Operator::Runtime(RuntimeOperator::new_str(name, sym, pos, prec, assoc, overloadable, handler)));
    };

//--------------------------------------------------------------------------------------//
//                               Native Static Functions                                //
//--------------------------------------------------------------------------------------//

fn assign_handler(op: StaticOperatorAst) -> Ast {
    if let StaticOperatorAst::Infix(lhs, rhs) = op {
        Ast::Assignment(Box::new(lhs), Box::new(rhs), None)
    } else {
        panic!("assign_handler() expects an infix operator")
    }
}

//--------------------------------------------------------------------------------------//
//                               Native Runtime Operators                               //
//--------------------------------------------------------------------------------------//

    let rt_add: FunctionVariation = FunctionVariation::Native(|vals| {
        let vals = if let NativeFunctionParameters::Singles(v) = vals { v } else { panic!("A native function with Singles function parameter type should not be able to receive a Variadic function parameter type") };
        if vals.len() != 2 { return Err(runtime_error("add() expects 2 arguments".to_string())); }
        let lhs = vals[0].clone();
        let rhs = vals[1].clone();
        if lhs.get_type().unwrap().subtype(&TY_INT) && rhs.get_type().unwrap().subtype(&TY_INT) {
            let lhs_val = if let Value::Number(Number::SignedInteger(SignedInteger::Int32(v))) = lhs { v } else { panic!("add() expects 2 arguments of type '{}'", TY_INT) };
            let rhs_val = if let Value::Number(Number::SignedInteger(SignedInteger::Int32(v))) = rhs { v } else { panic!("add() expects 2 arguments of type '{}'", TY_INT) };
            Ok(Value::Number(Number::SignedInteger(SignedInteger::Int32(lhs_val + rhs_val))))
        } else {
            Err(runtime_error(format!("add() expects 2 arguments of type '{}', got '{:?}' and '{:?}'", TY_INT, lhs.get_type(), rhs.get_type())))
        }
    }, FunctionParameterType::Singles(vec![("lhs".to_string(), TY_INT), ("rhs".to_string(), TY_INT)]), TY_INT);

//--------------------------------------------------------------------------------------//
//                                   Implementations                                    //
//--------------------------------------------------------------------------------------//

    // Built-in operators, these are not overloadable and are reserved for the language
    add_static(lexer, "assign", "=", OperatorPosition::Infix, 100, OperatorAssociativity::Right, false, assign_handler);
    add_runtime(lexer, "add", "+", OperatorPosition::Infix, 100, OperatorAssociativity::Left, false, Box::new(rt_add));
}


//--------------------------------------------------------------------------------------//
//                                                                                      //
//                                   Init Environment                                   //
//                                                                                      //
//--------------------------------------------------------------------------------------//


pub fn init_environment(env: &mut Environment) -> Failable<RuntimeError> {
    let add_value = |env: &mut Environment, name: &str, val: Value| {
        env.add_value(Str::String(name.to_string()), val)
    };
    let add_func = |env: &mut Environment, name: &str, variations: Vec<FunctionVariation>| {
        add_value(env, name, Value::Function(Function::new(name.to_string(), variations)))
    };

//--------------------------------------------------------------------------------------//
//                               Native Runtime Functions                               //
//--------------------------------------------------------------------------------------//

    let rt_print: FunctionVariation = FunctionVariation::Native(|vals| {
        let vals = if let NativeFunctionParameters::Variadic(_, v) = vals { v }
        else { panic!("A native function with Variadic function parameter type should not be able to receive a Singles function parameter type") };
        for val in vals { println!("{}", val); }
        Ok(Value::Unit)
    }, FunctionParameterType::Variadic(vec![], ("params".to_string(), TY_ANY)), TY_UNIT);

//--------------------------------------------------------------------------------------//
//                                      Constants                                       //
//--------------------------------------------------------------------------------------//

    add_value(env, "true", Value::Boolean(true))?;
    add_value(env, "false", Value::Boolean(false))?;
    add_value(env, "pi", Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::PI))))?;
    add_value(env, "tau", Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::TAU))))?;
    add_value(env, "e", Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::E))))?;
    add_value(env, "phi", Value::Number(Number::FloatingPoint(FloatingPoint::Float64(1.6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374))))?;
    add_value(env, "sqrt2", Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::SQRT_2))))?;

//--------------------------------------------------------------------------------------//
//                                   Implementations                                    //
//--------------------------------------------------------------------------------------//

    add_func(env, "print", vec![rt_print])?;
    Ok(())
}
