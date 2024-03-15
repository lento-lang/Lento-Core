use std::{
    io::{BufRead, Seek},
    vec,
};

use crate::{
    interpreter::{
        environment::Environment,
        value::{
            FloatingPoint, Function, FunctionVariation, NativeFunctionParameters, Number, Value,
        },
    },
    lexer::op::{
        Operator, OperatorAssociativity, OperatorPosition, RuntimeOperator, StaticOperator,
        StaticOperatorAst, StaticOperatorHandler,
    },
    parser::ast::Ast,
    stdlib::arithmetic,
    type_checker::types::{std_primitive_types, CheckedType, FunctionParameterType, Type},
    util::str::Str,
};

use super::super::lexer::lexer::Lexer;

const TY_ANY: Type = std_primitive_types::ANY;
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

    let add_static = |lexer: &mut Lexer<R>,
                      name: &str,
                      sym: &str,
                      pos: OperatorPosition,
                      prec: u16,
                      assoc: OperatorAssociativity,
                      overloadable: bool,
                      handler: StaticOperatorHandler| {
        if let Err(e) = lexer.define_op(Operator::Static(StaticOperator::new_str(
            name,
            sym,
            pos,
            prec,
            assoc,
            overloadable,
            handler,
        ))) {
            panic!(
                "Failed to initialize lexer when adding static operator '{}': {:?}",
                name, e
            );
        }
    };
    let add_runtime = |lexer: &mut Lexer<R>, operator: RuntimeOperator| {
        let name = operator.name.clone();
        if let Err(e) = lexer.define_op(Operator::Runtime(operator)) {
            panic!(
                "Failed to initialize lexer when adding runtime operator '{}': {:?}",
                name.clone(),
                e
            );
        }
    };

    //--------------------------------------------------------------------------------------//
    //                               Native Static Functions                                //
    //--------------------------------------------------------------------------------------//

    fn assign_handler(op: StaticOperatorAst) -> Ast {
        if let StaticOperatorAst::Infix(lhs, rhs) = op {
            Ast::Assignment(Box::new(lhs), Box::new(rhs), CheckedType::Unchecked)
        } else {
            panic!("assign_handler() expects an infix operator")
        }
    }

    //--------------------------------------------------------------------------------------//
    //                                   Implementations                                    //
    //--------------------------------------------------------------------------------------//

    // Built-in operators, these are not overloadable and are reserved for the language
    add_static(
        lexer,
        "assign",
        "=",
        OperatorPosition::Infix,
        100,
        OperatorAssociativity::Right,
        false,
        assign_handler,
    );
    add_runtime(lexer, arithmetic::op_add());
}

//--------------------------------------------------------------------------------------//
//                                                                                      //
//                                   Init Environment                                   //
//                                                                                      //
//--------------------------------------------------------------------------------------//

pub fn init_environment(env: &mut Environment) {
    let add_value = |env: &mut Environment, name: &str, val: Value| {
        if let Err(e) = env.add_value(Str::String(name.to_string()), val) {
            panic!(
                "Failed to initialize environment when adding value '{}': {:?}",
                name, e
            );
        }
    };
    let add_func = |env: &mut Environment, name: &str, variations: Vec<FunctionVariation>| {
        add_value(
            env,
            name,
            Value::Function(Function::new(name.to_string(), variations)),
        )
    };

    //--------------------------------------------------------------------------------------//
    //                               Native Runtime Functions                               //
    //--------------------------------------------------------------------------------------//

    let rt_print: FunctionVariation = FunctionVariation::Native(
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
    );

    //--------------------------------------------------------------------------------------//
    //                                      Constants                                       //
    //--------------------------------------------------------------------------------------//

    add_value(env, "true", Value::Boolean(true));
    add_value(env, "false", Value::Boolean(false));
    add_value(
        env,
        "pi",
        Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
            std::f64::consts::PI,
        ))),
    );
    add_value(
        env,
        "tau",
        Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
            std::f64::consts::TAU,
        ))),
    );
    add_value(
        env,
        "e",
        Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
            std::f64::consts::E,
        ))),
    );
    add_value(
        env,
        "phi",
        Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
            1.618_033_988_749_895,
        ))),
    );
    add_value(
        env,
        "sqrt2",
        Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
            std::f64::consts::SQRT_2,
        ))),
    );

    //--------------------------------------------------------------------------------------//
    //                                   Implementations                                    //
    //--------------------------------------------------------------------------------------//

    add_func(env, "print", vec![rt_print]);
    add_func(env, "add", vec![arithmetic::add()]);
}
