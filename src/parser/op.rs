use crate::{
    interpreter::value::FunctionVariation,
    parser::ast::Ast,
    type_checker::types::{FunctionParameterType, Type},
};

//--------------------------------------------------------------------------------------//
//                               Execution Agnostic Data                                //
//--------------------------------------------------------------------------------------//

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
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
        matches!(
            self,
            OperatorPosition::Infix | OperatorPosition::InfixAccumulate
        )
    }

    pub fn is_postfix(&self) -> bool {
        matches!(self, OperatorPosition::Postfix)
    }

    pub fn is_accumulate(&self) -> bool {
        matches!(self, OperatorPosition::InfixAccumulate)
    }
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
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

#[derive(Clone, Debug)]
pub enum StaticOperatorAst {
    Prefix(Ast),
    Infix(Ast, Ast),
    Postfix(Ast),
    Accumulate(Vec<Ast>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct OperatorSignature {
    pub params: FunctionParameterType,
    pub returns: Type,
}

//--------------------------------------------------------------------------------------//
//                                       Prelude                                        //
//--------------------------------------------------------------------------------------//

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct OperatorInfo {
    /// Descriptive name of the operator.
    /// Used for:
    /// - handler function definition
    /// - error messages
    /// - introspection.
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
}

#[derive(Clone, Debug)]
pub enum OperatorHandler {
    /// Runtime operators (functions)
    Runtime {
        function_name: String,
        handler: Box<FunctionVariation>,
    },
    /// The compile-time handler for the operator
    /// (macros or syntax extensions/sugar)
    /// 1. The signature of the operator. This is used for type checking and inference on the operator in expressions.
    /// 2. The native handler function for the operator called at compile-time
    Static(OperatorSignature, fn(StaticOperatorAst) -> Ast),
}

#[derive(Clone, Debug)]
pub struct Operator {
    /// Basic information about the operator
    /// required for parsing and type checking.
    pub info: OperatorInfo,
    /// The handler for the operator
    pub handler: OperatorHandler,
}

impl Operator {
    pub fn signature(&self) -> OperatorSignature {
        match self.handler {
            OperatorHandler::Runtime { ref handler, .. } => {
                let params = handler.get_params().clone();
                let returns = handler.get_return_type().clone();
                OperatorSignature { params, returns }
            }
            OperatorHandler::Static(ref signature, _) => signature.clone(),
        }
    }
}
