use std::io::Read;

use crate::{
    interpreter::{
        environment::Environment,
        value::{FloatingPoint, Function, Number, Value},
    }, lexer::lexer::Lexer, parser::{ast::Ast, op::{
        default_operator_precedence, Operator, OperatorAssociativity, OperatorHandler, OperatorPosition, OperatorSignature, StaticOperatorAst
    }, parser::Parser}, stdlib::arithmetic, type_checker::types::{std_primitive_types, CheckedType, FunctionParameterType, GetType, Type}, util::str::Str
};

use super::system;

pub struct Initializer {
    operators: Vec<Operator>,
    types: Vec<Type>,
    values: Vec<(Str, Value)>,
    functions: Vec<(Str, Function)>,
}

impl Initializer {
    pub fn init_lexer(&self, lexer: &mut Lexer<impl Read>) {
        for op in &self.operators {
            if let OperatorHandler::Static(_, _handler) = &op.handler {
                lexer.operators.insert(op.symbol.clone());
            }
        }
        for type_ in &self.types {
            if let Type::Literal(name) = type_ {
                lexer.types.insert(name.to_string());
            } else {
                panic!("init_lexer() expects a literal type");
            }
        }
    }

    pub fn init_parser(&self, parser: &mut Parser<impl Read>) {
        for op in &self.operators {
            if let Err(e) = parser.define_op(op.clone()) {
                panic!("Parser initialization failed when adding operator '{:?}': {:?}", op, e);
            }
        }
        for type_ in &self.types {
            if let Type::Literal(name) = type_ {
                if let Err(e) = parser.define_literal_type(type_.clone()) {
                    panic!("Parser initialization failed when adding type '{}': {:?}", name, e);
                }
            } else {
                panic!("init_parser() expects a literal type");
            }
        }
    }

    pub fn init_environment(&self, env: &mut Environment) {
        for (name, val) in &self.values {
            if let Err(e) = env.add_value(name.clone(), val.clone()) {
                panic!("Environment initialization failed when adding value '{}': {:?}", name, e);
            }
        }
        for (name, func) in &self.functions {
            if let Err(e) = env.add_value(name.clone(), Value::Function(func.clone())) {
                panic!("Environment initialization failed when adding function '{}': {:?}", name, e);
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
                name: "assign".into(),
                symbol: "=".into(),
                position: OperatorPosition::Infix,
                precedence: default_operator_precedence::ASSIGNMENT,
                associativity: OperatorAssociativity::Right,
                overloadable: false,
                allow_trailing: false,
                handler: OperatorHandler::Static(
                    OperatorSignature {
                        params: FunctionParameterType::Singles(vec![("lhs".into(), Type::Any), ("rhs".into(), Type::Any)]),
                        returns: Type::Any,
                    },
                    |op| {
                        if let StaticOperatorAst::Infix(lhs, rhs) = op {
                            let assign_type = rhs.get_type();
                            Ast::Assignment(Box::new(lhs), Box::new(rhs), assign_type)
                        } else {
                            panic!("assign expects an infix operator");
                        }
                    },
                ),
            },
            Operator {
                name: "tuple".into(),
                symbol: ",".into(),
                position: OperatorPosition::InfixAccumulate,
                precedence: default_operator_precedence::TUPLE,
                associativity: OperatorAssociativity::Left,
                overloadable: false,
                allow_trailing: true,
                handler: OperatorHandler::Static(
                    OperatorSignature {
                        params: FunctionParameterType::Variadic(vec![], ("elements".into(), Type::Any)),
                        returns: Type::Any,
                    },
                    |op| {
                        if let StaticOperatorAst::Accumulate(elems) = op {
                            let tuple_type = if elems.iter().any(|e| e.get_type() == CheckedType::Unchecked) {
                                CheckedType::Unchecked
                            } else {
                                CheckedType::Checked(
                                    Type::Tuple(elems.iter().map(|e| e.get_type().unwrap_checked().clone()).collect())
                                )
                            };
                            Ast::Tuple(elems, tuple_type)
                        } else {
                            panic!("tuple expects an accumulator operator");
                        }
                    },
                ),
            },
            Operator {
                name: "add".into(),
                symbol: "+".into(),
                position: OperatorPosition::Infix,
                precedence: default_operator_precedence::ADDITIVE,
                associativity: OperatorAssociativity::Left,
                overloadable: true,
                allow_trailing: false,
                handler: OperatorHandler::Runtime(Box::new(arithmetic::add())),
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
        ],

        //--------------------------------------------------------------------------------------//
        //                                      Constants                                       //
        //--------------------------------------------------------------------------------------//

        values: vec![
            (Str::String("pi".to_string()), Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::PI)))),
            (Str::String("tau".to_string()), Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::TAU)))),
            (Str::String("e".to_string()), Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::E)))),
            (Str::String("phi".to_string()), Value::Number(Number::FloatingPoint(FloatingPoint::Float64(1.618_033_988_749_895)))),
            (Str::String("sqrt2".to_string()), Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::SQRT_2)))),
            (Str::String("ln2".to_string()), Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::LN_2)))),
            (Str::String("ln10".to_string()), Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::consts::LN_10)))),
            (Str::String("inf".to_string()), Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::INFINITY)))),
            (Str::String("nan".to_string()), Value::Number(Number::FloatingPoint(FloatingPoint::Float64(std::f64::NAN)))),
        ],

        //--------------------------------------------------------------------------------------//
        //                                       Functions                                      //
        //--------------------------------------------------------------------------------------//
        functions: vec![
            (Str::String("add".to_string()), Function::new("add".to_string(), vec![arithmetic::add()])),
            (Str::String("print".to_string()), Function::new("print".to_string(), vec![system::print()])),
            (Str::String("exit".to_string()), Function::new("exit".to_string(), vec![system::exit()])),
        ],
    }
}
