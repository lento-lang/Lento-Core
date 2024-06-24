use std::io::Read;

use crate::{
    interpreter::{
        environment::Environment,
        value::{FloatingPoint, Function, FunctionVariation, Number, Value},
    },
    lexer::op::{
        default_operator_precedence, Operator, OperatorAssociativity, OperatorPosition,
        RuntimeOperator, StaticOperator, StaticOperatorAst, StaticOperatorHandler,
    },
    parser::ast::Ast,
    stdlib::arithmetic,
    type_checker::types::{std_primitive_types, CheckedType, GetType, Type},
    util::str::Str,
};

use super::{super::lexer::lexer::Lexer, system};

//--------------------------------------------------------------------------------------//
//                                                                                      //
//                                      Init Lexer                                      //
//                                                                                      //
//--------------------------------------------------------------------------------------//

pub fn init_lexer<R: Read>(lexer: &mut Lexer<R>) {
    //--------------------------------------------------------------------------------------//
    //                                       Helpers                                        //
    //--------------------------------------------------------------------------------------//

    let add_op_static = |lexer: &mut Lexer<R>,
                         name: &str,
                         symbol: &str,
                         pos: OperatorPosition,
                         precedence: u16,
                         associativity: OperatorAssociativity,
                         overloadable: bool,
                         allow_trailing: bool,
                         handler: StaticOperatorHandler| {
        if let Err(e) = lexer.define_op(Operator::Static(StaticOperator {
            name: name.into(),
            symbol: symbol.into(),
            position: pos,
            precedence,
            associativity,
            overloadable,
            allow_trailing,
            handler,
        })) {
            panic!(
                "Failed to initialize lexer when adding static operator '{}': {:?}",
                name, e
            );
        }
    };
    let add_op_runtime = |lexer: &mut Lexer<R>, operator: RuntimeOperator| {
        let name = operator.name.clone();
        if let Err(e) = lexer.define_op(Operator::Runtime(operator)) {
            panic!(
                "Failed to initialize lexer when adding runtime operator '{}': {:?}",
                name.clone(),
                e
            );
        }
    };

    let add_literal_type = |lexer: &mut Lexer<R>, type_: Type| {
        if let Type::Literal(name) = type_ {
            if let Err(e) = lexer.define_type(name.to_string()) {
                panic!(
                    "Failed to initialize lexer when adding type '{}': {:?}",
                    name, e
                );
            }
        } else {
            panic!("add_literal_type() expects a literal type")
        }
    };

    fn static_infix(name: &'static str, op: StaticOperatorAst, callback: fn(Ast, Ast) -> Ast) -> Ast {
        if let StaticOperatorAst::Infix(lhs, rhs) = op {
            callback(lhs, rhs)
        } else {
            panic!("{} expects an infix operator", name)
        }
    }

    fn static_accum(name: &'static str, op: StaticOperatorAst, callback: fn(Vec<Ast>) -> Ast) -> Ast {
        if let StaticOperatorAst::Accumulate(elems) = op {
            callback(elems)
        } else {
            panic!("{} expects an accumulator operator", name)
        }
    }

    //--------------------------------------------------------------------------------------//
    //                                   Implementations                                    //
    //--------------------------------------------------------------------------------------//

    // Built-in operators, these are not overloadable and are reserved for the language
    add_op_static(
        lexer,
        "assign",
        "=",
        OperatorPosition::Infix,
        default_operator_precedence::ASSIGNMENT,
        OperatorAssociativity::Right,
        false,
        false,
        |op| static_infix("assign", op, |lhs, rhs| {
            let assign_type = rhs.get_type(); // Inherit the type of the right-hand side
            Ast::Assignment(Box::new(lhs), Box::new(rhs), assign_type)
        }),
    );
    add_op_static(
        lexer,
        "tuple",
        ",",
        OperatorPosition::InfixAccumulate,
        default_operator_precedence::TUPLE,
        OperatorAssociativity::Right,
        false,
        true,
        |op| static_accum("tuple", op, |elems| {
            // Inherit the type of all elements as a new tuple type
            let tuple_type = if elems.iter().any(|e| e.get_type() == CheckedType::Unchecked) {
                CheckedType::Unchecked
            } else {
                CheckedType::Checked(
                    Type::Tuple(elems.iter().map(|e| e.get_type().unwrap_checked().clone()).collect())
                )
            };
            Ast::Tuple(elems, tuple_type)
        }),
    );
    add_op_runtime(lexer, arithmetic::op_add());

    //--------------------------------------------------------------------------------------//
    //                                  Built-in Types                                      //
    //--------------------------------------------------------------------------------------//

    add_literal_type(lexer, Type::Literal(Str::Str("any")));
    add_literal_type(lexer, Type::Literal(Str::Str("unit")));
    add_literal_type(lexer, std_primitive_types::STRING);
    add_literal_type(lexer, std_primitive_types::CHAR);
    add_literal_type(lexer, std_primitive_types::BOOL);
    add_literal_type(lexer, std_primitive_types::UINT1);
    add_literal_type(lexer, std_primitive_types::UINT8);
    add_literal_type(lexer, std_primitive_types::UINT16);
    add_literal_type(lexer, std_primitive_types::UINT32);
    add_literal_type(lexer, std_primitive_types::UINT64);
    add_literal_type(lexer, std_primitive_types::UINT128);
    add_literal_type(lexer, std_primitive_types::UINTBIG);
    add_literal_type(lexer, std_primitive_types::INT8);
    add_literal_type(lexer, std_primitive_types::INT16);
    add_literal_type(lexer, std_primitive_types::INT32);
    add_literal_type(lexer, std_primitive_types::INT64);
    add_literal_type(lexer, std_primitive_types::INT128);
    add_literal_type(lexer, std_primitive_types::INTBIG);
    add_literal_type(lexer, std_primitive_types::FLOAT32);
    add_literal_type(lexer, std_primitive_types::FLOAT64);
    add_literal_type(lexer, std_primitive_types::FLOATBIG);
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
    let add_literal_type = |env: &mut Environment, type_: Type| {
        if let Type::Literal(name) = type_.clone() {
            if let Err(e) = env.add_type(name.clone(), type_) {
                panic!(
                    "Failed to initialize environment when adding type '{}': {:?}",
                    name, e
                );
            }
        } else {
            panic!("add_literal_type() expects a literal type")
        }
    };

    //--------------------------------------------------------------------------------------//
    //                                      Constants                                       //
    //--------------------------------------------------------------------------------------//

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

    add_func(env, "add", vec![arithmetic::add()]);
    add_func(env, "print", vec![system::print()]);
    add_func(env, "exit", vec![system::exit()]);

    //--------------------------------------------------------------------------------------//
    //								  Built-in Types                                      //
    //--------------------------------------------------------------------------------------//

    add_literal_type(env, Type::Literal(Str::Str("any")));
    add_literal_type(env, Type::Literal(Str::Str("unit")));
    add_literal_type(env, std_primitive_types::STRING);
    add_literal_type(env, std_primitive_types::CHAR);
    add_literal_type(env, std_primitive_types::BOOL);
    add_literal_type(env, std_primitive_types::UINT1);
    add_literal_type(env, std_primitive_types::UINT8);
    add_literal_type(env, std_primitive_types::UINT16);
    add_literal_type(env, std_primitive_types::UINT32);
    add_literal_type(env, std_primitive_types::UINT64);
    add_literal_type(env, std_primitive_types::UINT128);
    add_literal_type(env, std_primitive_types::UINTBIG);
    add_literal_type(env, std_primitive_types::INT8);
    add_literal_type(env, std_primitive_types::INT16);
    add_literal_type(env, std_primitive_types::INT32);
    add_literal_type(env, std_primitive_types::INT64);
    add_literal_type(env, std_primitive_types::INT128);
    add_literal_type(env, std_primitive_types::INTBIG);
    add_literal_type(env, std_primitive_types::FLOAT32);
    add_literal_type(env, std_primitive_types::FLOAT64);
    add_literal_type(env, std_primitive_types::FLOATBIG);
}
