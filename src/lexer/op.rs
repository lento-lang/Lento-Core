use crate::{interpreter::value::FunctionVariation, parser::ast::Ast};

//--------------------------------------------------------------------------------------//
//                               Execution Agnostic Data                                //
//--------------------------------------------------------------------------------------//

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OperatorPosition {
    Prefix,  // Unary operator
    Infix,   // Binary operator
    Postfix, // Unary operator
    /// If the binary operator should accumulate the arguments
    /// when there are more than two arguments in the expression
    ///
    /// ## Example
    /// The comma operator (`,`) accumulates the arguments into a tuple
    /// ```ignore
    /// a, b, c // Accumulate into a tuple
    /// ```
    /// Gives a tuple `(a, b, c)`, not `(a, (b, c))`
    ///
    /// The assignment operator (`=`) does not accumulate the arguments
    /// ```ignore
    /// a = b // Does not accumulate
    /// ```
    /// Gives a single assignment `a = b`
    InfixAccumulate,
}

impl OperatorPosition {
    pub fn is_prefix(&self) -> bool {
        matches!(self, OperatorPosition::Prefix)
    }

    pub fn is_infix(&self) -> bool {
        matches!(self, OperatorPosition::Infix | OperatorPosition::InfixAccumulate)
    }

    pub fn is_postfix(&self) -> bool {
        matches!(self, OperatorPosition::Postfix)
    }

    pub fn is_accumulate(&self) -> bool {
        matches!(self, OperatorPosition::InfixAccumulate)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
}

pub type OperatorPrecedence = u16;

pub mod default_operator_precedence {
    use super::OperatorPrecedence;

    pub const ASSIGNMENT: OperatorPrecedence = 100;
    pub const CONDITIONAL: OperatorPrecedence = 200;
    pub const LOGICAL_OR: OperatorPrecedence = 300;
    pub const LOGICAL_AND: OperatorPrecedence = 400;
    pub const EQUALITY: OperatorPrecedence = 500;
    pub const TUPLE: OperatorPrecedence = 600;
    pub const ADDITIVE: OperatorPrecedence = 700;
    pub const MULTIPLICATIVE: OperatorPrecedence = 800;
    pub const EXPONENTIAL: OperatorPrecedence = 900;
    pub const PREFIX: OperatorPrecedence = 1000;
    pub const POSTFIX: OperatorPrecedence = 1100;
}

//--------------------------------------------------------------------------------------//
//                                      Operators                                       //
//--------------------------------------------------------------------------------------//

pub type RuntimeOperatorHandler = Box<FunctionVariation>; // Function reference, verify arity is the same as the operator position

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeOperator {
    /// Descriptive name of the operator
    pub name: String,
    /// The symbol of the operator
    pub symbol: String,
    /// The position of the operator
    pub position: OperatorPosition,
    /// The precedence of the operator
    pub precedence: OperatorPrecedence,
    /// The associativity of the operator
    pub associativity: OperatorAssociativity,
    /// If the operator is overloadable (false for built-in operators)
    pub overloadable: bool,
    /// If the operator allows trailing arguments
    ///
    /// ## Note
    /// **Only applicable for infix accumulate operators!**
    ///
    /// ## Example
    /// Addition operator (`+`) does usually **not** allow trailing arguments, while the comma operator (`,`) does.
    /// ```ignore
    /// a + b + c   // OK
    /// a + b + c + // Error `+` does not allow trailing arguments
    /// a, b, c     // OK
    /// a, b, c,    // OK `,` allow trailing arguments
    /// ```
    pub allow_trailing: bool,
    /// The runtime handler for the operator
    pub handler: RuntimeOperatorHandler,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StaticOperatorAst {
    Prefix(Ast),
    Infix(Ast, Ast),
    Postfix(Ast),
    Accumulate(Vec<Ast>)
}

pub type StaticOperatorHandler = fn(StaticOperatorAst) -> Ast;

#[derive(Clone, Debug)]
pub struct StaticOperator {
    /// Descriptive name of the operator
    pub name: String,
    /// The symbol of the operator
    pub symbol: String,
    /// The position of the operator
    pub position: OperatorPosition,
    /// The precedence of the operator
    pub precedence: OperatorPrecedence,
    /// The associativity of the operator
    pub associativity: OperatorAssociativity,
    /// If the operator is overloadable (false for built-in operators)
    pub overloadable: bool,
    /// If the operator allows trailing arguments
    ///
    /// ## Note
    /// **Only applicable for infix accumulate operators!**
    ///
    /// ## Example
    /// Addition operator (`+`) does usually **not** allow trailing arguments, while the comma operator (`,`) does.
    /// ```ignore
    /// a + b + c   // OK
    /// a + b + c + // Error `+` does not allow trailing arguments
    /// a, b, c     // OK
    /// a, b, c,    // OK `,` allow trailing arguments
    /// ```
    pub allow_trailing: bool,
    /// The compile-time handler for the operator
    pub handler: StaticOperatorHandler,
}

//--------------------------------------------------------------------------------------//
//                                       Prelude                                        //
//--------------------------------------------------------------------------------------//

#[derive(Clone, Debug)]
pub enum Operator {
    Runtime(RuntimeOperator), // Runtime operators (functions)
    Static(StaticOperator),   // Compile-time operators (macros or syntax extensions/sugar)
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

    pub fn pos(&self) -> OperatorPosition {
        match self {
            Operator::Runtime(op) => op.position.clone(),
            Operator::Static(op) => op.position.clone(),
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

    pub fn allow_trailing(&self) -> bool {
        match self {
            Operator::Runtime(op) => op.allow_trailing,
            Operator::Static(op) => op.allow_trailing,
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
