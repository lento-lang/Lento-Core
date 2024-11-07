use std::{collections::HashMap, io::Read};

use crate::{
    interpreter::{
        environment::Environment,
        number::{FloatingPoint, Number},
        value::{Function, Value},
    },
    lexer::lexer::Lexer,
    parser::{
        ast::Ast,
        op::{
            default_operator_precedence, Operator, OperatorAssociativity, OperatorHandler,
            OperatorInfo, OperatorPosition, OperatorSignature, StaticOperatorAst,
        },
        parser::Parser,
    },
    stdlib::arithmetic,
    type_checker::{
        checker::TypeChecker,
        types::{std_primitive_types, FunctionParameterType, Type},
    },
    util::str::Str,
};

use super::system;

pub struct Initializer {
    operators: Vec<Operator>,
    types: HashMap<String, Type>,
    values: Vec<(Str, Value)>,
    functions: Vec<(&'static str, Function)>,
}

impl Initializer {
    pub fn init_lexer(&self, lexer: &mut Lexer<impl Read>) {
        for op in &self.operators {
            if let OperatorHandler::Static(_, _handler) = &op.handler {
                lexer.operators.insert(op.info.symbol.clone());
            }
        }
    }

    pub fn init_parser(&self, parser: &mut Parser<impl Read>) {
        for op in &self.operators {
            if let Err(e) = parser.define_op(op.info.clone()) {
                panic!(
                    "Parser initialization failed when adding operator '{:?}': {:?}",
                    op, e
                );
            }
        }
    }

    pub fn init_type_checker(&self, type_checker: &mut TypeChecker) {
        for op in &self.operators {
            type_checker.add_operator(op.clone());
        }
        for (name, ty) in &self.types {
            type_checker.add_type(name, ty.clone());
        }
        for (name, func) in &self.functions {
            for variation in func.get_variations() {
                type_checker.add_function(name, variation.get_type());
            }
        }
    }

    pub fn init_environment(&self, env: &mut Environment) {
        for (name, val) in &self.values {
            if let Err(e) = env.add_value(name.clone(), val.clone()) {
                panic!(
                    "Environment initialization failed when adding value '{}': {:?}",
                    name, e
                );
            }
        }
        for (name, func) in &self.functions {
            if let Err(e) =
                env.add_value(Str::String(name.to_string()), Value::Function(func.clone()))
            {
                panic!(
                    "Environment initialization failed when adding function '{}': {:?}",
                    name, e
                );
            }
        }
        for op in &self.operators {
            match &op.handler {
                OperatorHandler::Runtime {
                    function_name,
                    handler,
                } => {
                    if let Err(e) = env.add_function_variation(function_name, *handler.clone()) {
                        panic!(
                            "Environment initialization failed when adding operator '{}': {:?}",
                            op.info.name, e
                        );
                    }
                }
                OperatorHandler::Static(_, _) => {}
            }
        }
    }
}

pub fn stdlib() -> Initializer {
    Initializer {
        //--------------------------------------------------------------------------------------//
        //                                       Operators                                      //
        //--------------------------------------------------------------------------------------//
        operators: vec![
            Operator {
                info: OperatorInfo {
                    name: "assign".into(),
                    symbol: "=".into(),
                    position: OperatorPosition::Infix,
                    precedence: default_operator_precedence::ASSIGNMENT,
                    associativity: OperatorAssociativity::Right,
                    overloadable: false,
                    allow_trailing: false,
                },
                handler: OperatorHandler::Static(
                    OperatorSignature {
                        params: FunctionParameterType::Singles(vec![
                            ("lhs".into(), std_primitive_types::ANY),
                            ("rhs".into(), std_primitive_types::ANY),
                        ]),
                        returns: std_primitive_types::ANY,
                    },
                    |op| {
                        if let StaticOperatorAst::Infix(lhs, rhs) = op {
                            Ast::Assignment(Box::new(lhs), Box::new(rhs))
                        } else {
                            panic!("assign expects an infix operator");
                        }
                    },
                ),
            },
            Operator {
                info: OperatorInfo {
                    name: "add".into(),
                    symbol: "+".into(),
                    position: OperatorPosition::Infix,
                    precedence: default_operator_precedence::ADDITIVE,
                    associativity: OperatorAssociativity::Left,
                    overloadable: true,
                    allow_trailing: false,
                },
                handler: OperatorHandler::Runtime {
                    function_name: "add".into(),
                    handler: Box::new(arithmetic::add()),
                },
            },
        ],

        //--------------------------------------------------------------------------------------//
        //						            Built-in Types                                      //
        //--------------------------------------------------------------------------------------//
        types: vec![
            Type::Literal(Str::Str("any")),
            Type::Literal(Str::Str("unit")),
            std_primitive_types::STRING,
            std_primitive_types::CHAR,
            std_primitive_types::BOOL,
            std_primitive_types::UINT1,
            std_primitive_types::UINT8,
            std_primitive_types::UINT16,
            std_primitive_types::UINT32,
            std_primitive_types::UINT64,
            std_primitive_types::UINT128,
            std_primitive_types::UINTBIG,
            std_primitive_types::INT8,
            std_primitive_types::INT16,
            std_primitive_types::INT32,
            std_primitive_types::INT64,
            std_primitive_types::INT128,
            std_primitive_types::INTBIG,
            std_primitive_types::FLOAT32,
            std_primitive_types::FLOAT64,
            std_primitive_types::FLOATBIG,
        ]
        .into_iter()
        .map(|ty| {
            if let Type::Literal(ref name) = ty {
                (name.to_string(), ty)
            } else {
                panic!("stdlib() expects a literal type");
            }
        })
        .collect(),

        //--------------------------------------------------------------------------------------//
        //                                      Constants                                       //
        //--------------------------------------------------------------------------------------//
        values: vec![
            (
                Str::String("pi".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::PI,
                ))),
            ),
            (
                Str::String("tau".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::TAU,
                ))),
            ),
            (
                Str::String("e".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::E,
                ))),
            ),
            (
                Str::String("phi".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    1.618_033_988_749_895,
                ))),
            ),
            (
                Str::String("sqrt2".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::SQRT_2,
                ))),
            ),
            (
                Str::String("ln2".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::LN_2,
                ))),
            ),
            (
                Str::String("ln10".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::LN_10,
                ))),
            ),
            (
                Str::String("inf".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(f64::INFINITY))),
            ),
            (
                Str::String("nan".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(f64::NAN))),
            ),
        ],

        //--------------------------------------------------------------------------------------//
        //                                       Functions                                      //
        //--------------------------------------------------------------------------------------//
        functions: vec![
            ("add", Function::new(vec![arithmetic::add()])),
            ("print", Function::new(vec![system::print()])),
            ("typeof", Function::new(vec![system::type_of()])),
            ("exit", Function::new(vec![system::exit()])),
        ],
    }
}
