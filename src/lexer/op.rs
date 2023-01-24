use crate::{parser::ast::Ast, interpreter::value::FunctionVariation};


//--------------------------------------------------------------------------------------//
//                               Execution Agnostic Data                                //
//--------------------------------------------------------------------------------------//

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OperatorPosition {
    Prefix,     // Unary operator
    Infix,      // Binary operator
    Postfix,    // Unary operator
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}

type OperatorPrecedence = u16;


//--------------------------------------------------------------------------------------//
//                                   Runtime Operator                                   //
//--------------------------------------------------------------------------------------//


pub type RuntimeOperatorHandler = Box<FunctionVariation>; // Function reference, verify arity is the same as the operator position

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeOperator {
    pub name: String,           // Descriptive name of the operator
    pub symbol: String,         // The symbol of the operator
    pub pos: OperatorPosition,  // The position of the operator
    pub precedence: OperatorPrecedence,        // The precedence of the operator
    pub associativity: OperatorAssociativity, // The associativity of the operator
    pub overloadable: bool,     // If the operator is overloadable (false for built-in operators)
    pub handler: RuntimeOperatorHandler, // The runtime handler for the operator
}

impl RuntimeOperator {
    pub fn new(name: String, symbol: String, pos: OperatorPosition, precedence: u16, associativity: OperatorAssociativity, overloadable: bool, handler: RuntimeOperatorHandler) -> Self {
        Self {
            name,
            symbol,
            pos,
            precedence,
            associativity,
            overloadable,
            handler,
        }
    }

    pub fn new_str(name: &str, symbol: &str, pos: OperatorPosition, precedence: u16, associativity: OperatorAssociativity, overloadable: bool, handler: RuntimeOperatorHandler) -> Self {
        Self::new(name.to_string(), symbol.to_string(), pos, precedence, associativity, overloadable, handler)
    }
}


//--------------------------------------------------------------------------------------//
//                                   Static Operator                                    //
//--------------------------------------------------------------------------------------//


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StaticOperatorAst {
    Prefix(Ast),
    Infix(Ast, Ast),
    Postfix(Ast),
}

pub type StaticOperatorHandler = fn(StaticOperatorAst) -> Ast;

#[derive(Clone, Debug)]
pub struct StaticOperator {
    pub name: String,           // Descriptive name of the operator
    pub symbol: String,         // The symbol of the operator
    pub pos: OperatorPosition,  // The position of the operator
    pub precedence: OperatorPrecedence,        // The precedence of the operator
    pub associativity: OperatorAssociativity, // The associativity of the operator
    pub overloadable: bool,     // If the operator is overloadable (false for built-in operators)
    pub handler: StaticOperatorHandler, // The compile-time handler for the operator
}

impl StaticOperator {
    pub fn new(name: String, symbol: String, pos: OperatorPosition, precedence: u16, associativity: OperatorAssociativity, overloadable: bool, handler: StaticOperatorHandler) -> Self {
        Self {
            name,
            symbol,
            pos,
            precedence,
            associativity,
            overloadable,
            handler,
        }
    }

    pub fn new_str(name: &str, symbol: &str, pos: OperatorPosition, precedence: u16, associativity: OperatorAssociativity, overloadable: bool, handler: StaticOperatorHandler) -> Self {
        Self::new(name.to_string(), symbol.to_string(), pos, precedence, associativity, overloadable, handler)
    }
}


//--------------------------------------------------------------------------------------//
//                                       Prelude                                        //
//--------------------------------------------------------------------------------------//


#[derive(Clone, Debug)]
pub enum Operator {
    Runtime(RuntimeOperator),   // Runtime operators (functions)
    Static(StaticOperator),     // Compile-time operators (macros or syntax extensions/sugar)
}

impl Operator {
    pub fn name(&self) -> String {
        match self {
            Operator::Runtime(op) => op.name.clone(),
            Operator::Static(op) => op.name.clone(),
        }
    }

    pub fn symbol(&self) -> String {
        match self {
            Operator::Runtime(op) => op.symbol.clone(),
            Operator::Static(op) => op.symbol.clone(),
        }
    }

    pub fn precedence(&self) -> OperatorPrecedence {
        match self {
            Operator::Runtime(op) => op.precedence,
            Operator::Static(op) => op.precedence,
        }
    }

    pub fn associativity(&self) -> OperatorAssociativity {
        match self {
            Operator::Runtime(op) => op.associativity.clone(),
            Operator::Static(op) => op.associativity.clone(),
        }
    }

    pub fn overloadable(&self) -> bool {
        match self {
            Operator::Runtime(op) => op.overloadable,
            Operator::Static(op) => op.overloadable,
        }
    }
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Operator::Runtime(op) => op.symbol == other.symbol(),
            Operator::Static(op) => op.symbol == other.symbol(),
        }
    }
}

impl Eq for Operator {}
