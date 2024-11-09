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
        types::{std_types, FunctionParameterType, Type},
    },
    util::str::Str,
};

use super::{logical, system};

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
            // Also add runtime handler functions to the type checker
            if let OperatorHandler::Runtime {
                function_name,
                handler,
            } = &op.handler
            {
                type_checker.add_function(function_name, handler.get_type());
            }
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
                            "Environment initialization failed when adding operator '{}': {}",
                            op.info.name, e.message
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
            // Assignment operator, native to the language
            // TODO: Implement this operator statically in the parser instead of using an operator handler
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
                            ("lhs".into(), std_types::ANY),
                            ("rhs".into(), std_types::ANY),
                        ]),
                        returns: std_types::ANY,
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
            // Addition operator
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
            Operator {
                info: OperatorInfo {
                    name: "sub".into(),
                    symbol: "-".into(),
                    position: OperatorPosition::Infix,
                    precedence: default_operator_precedence::ADDITIVE,
                    associativity: OperatorAssociativity::Left,
                    overloadable: true,
                    allow_trailing: false,
                },
                handler: OperatorHandler::Runtime {
                    function_name: "sub".into(),
                    handler: Box::new(arithmetic::sub()),
                },
            },
            Operator {
                info: OperatorInfo {
                    name: "mul".into(),
                    symbol: "*".into(),
                    position: OperatorPosition::Infix,
                    precedence: default_operator_precedence::MULTIPLICATIVE,
                    associativity: OperatorAssociativity::Left,
                    overloadable: true,
                    allow_trailing: false,
                },
                handler: OperatorHandler::Runtime {
                    function_name: "mul".into(),
                    handler: Box::new(arithmetic::mul()),
                },
            },
            Operator {
                info: OperatorInfo {
                    name: "div".into(),
                    symbol: "/".into(),
                    position: OperatorPosition::Infix,
                    precedence: default_operator_precedence::MULTIPLICATIVE,
                    associativity: OperatorAssociativity::Left,
                    overloadable: true,
                    allow_trailing: false,
                },
                handler: OperatorHandler::Runtime {
                    function_name: "div".into(),
                    handler: Box::new(arithmetic::div()),
                },
            },
            // Equality operator
            Operator {
                info: OperatorInfo {
                    name: "eq".into(),
                    symbol: "==".into(),
                    position: OperatorPosition::Infix,
                    precedence: default_operator_precedence::EQUALITY,
                    associativity: OperatorAssociativity::Left,
                    overloadable: true,
                    allow_trailing: false,
                },
                handler: OperatorHandler::Runtime {
                    function_name: "eq".into(),
                    handler: Box::new(logical::eq()),
                },
            },
        ],

        //--------------------------------------------------------------------------------------//
        //						            Built-in Types                                      //
        //--------------------------------------------------------------------------------------//
        types: vec![
            Type::Literal(Str::Str("any")),
            Type::Literal(Str::Str("unit")),
            std_types::STRING,
            std_types::CHAR,
            std_types::BOOL,
            std_types::UINT1,
            std_types::UINT8,
            std_types::UINT16,
            std_types::UINT32,
            std_types::UINT64,
            std_types::UINT128,
            std_types::UINTBIG,
            std_types::INT8,
            std_types::INT16,
            std_types::INT32,
            std_types::INT64,
            std_types::INT128,
            std_types::INTBIG,
            std_types::FLOAT32,
            std_types::FLOAT64,
            std_types::FLOATBIG,
        ]
        .into_iter()
        .map(|ty| {
            if let Type::Literal(ref name) = ty {
                (name.to_string(), ty)
            } else {
                panic!("stdlib() expects a literal type");
            }
        })
        .chain([
            ("uint".into(), std_types::UINT()),
            ("int".into(), std_types::INT()),
            ("float".into(), std_types::FLOAT()),
            ("num".into(), std_types::NUM()),
        ])
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
            ("print", Function::new(vec![system::print()])),
            ("typeof", Function::new(vec![system::type_of()])),
            ("exit", Function::new(vec![system::exit()])),
        ],
    }
}
