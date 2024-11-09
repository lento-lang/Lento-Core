use std::{borrow::Borrow, collections::HashMap};

use crate::{
    interpreter::value::{RecordKey, Value},
    parser::{
        ast::{Ast, Module},
        op::{Operator, OperatorHandler, OperatorInfo, StaticOperatorAst},
    },
};

use super::{
    checked_ast::{CheckedAst, CheckedModule},
    types::{std_types, FunctionParameterType, GetType, Type, TypeTrait, VariationType},
};

/// A type error is an error that occurs during type checking.
#[derive(Debug)]
pub struct TypeError {
    pub message: String,
}

// The result of the type checker stage
pub type TypeResult<T> = Result<T, TypeError>;

/// The type environment contains all the types and functions in the program.
/// It is used to check the types of expressions and functions.
#[derive(Debug, Default, Clone)]
struct TypeEnv {
    // The variable environment
    variables: HashMap<String, Type>,

    // The function environment
    functions: HashMap<String, Vec<VariationType>>,

    // The type environment
    types: HashMap<String, Type>,

    // The operators environment
    operators: Vec<Operator>,
}

impl TypeEnv {
    // Add a function to the type environment
    pub fn add_function(&mut self, name: String, variation: VariationType) {
        self.functions.entry(name).or_default().push(variation);
    }

    pub fn lookup_function(&self, name: &str) -> Option<&[VariationType]> {
        self.functions.get(name).map(Vec::as_ref)
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }

    pub fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    // Add a type to the type environment
    pub fn add_type(&mut self, name: &str, ty: Type) {
        self.types.insert(name.to_string(), ty);
    }

    // Add a variable to the type environment
    pub fn add_variable(&mut self, name: &str, ty: Type) {
        self.variables.insert(name.to_string(), ty);
    }

    // Add an operator to the type environment
    pub fn add_operator(&mut self, op: Operator) {
        self.operators.push(op);
    }
}

enum IdentifierType<'a> {
    Variable(&'a Type),
    Type(&'a Type),
    Function(&'a [VariationType]),
}

/// The type checker is used to check the types of expressions and functions.
#[derive(Debug, Default)]
pub struct TypeChecker<'a> {
    // The type environment
    env: TypeEnv,
    parent: Option<&'a TypeChecker<'a>>,
}

impl<'a> TypeChecker<'a> {
    // ================== Type environment functions ==================

    pub fn reset(&mut self) {
        self.env = TypeEnv::default();
    }

    pub fn add_type(&mut self, name: &str, ty: Type) {
        self.env.add_type(name, ty);
    }

    pub fn add_operator(&mut self, op: Operator) {
        self.env.add_operator(op);
    }

    pub fn add_function(&mut self, name: &str, variation: VariationType) {
        self.env.add_function(name.to_string(), variation);
    }

    fn new_scope(&self) -> TypeChecker {
        TypeChecker {
            env: TypeEnv::default(),
            parent: Some(self),
        }
    }

    fn lookup_function(&self, name: &str) -> Option<&[VariationType]> {
        self.env
            .lookup_function(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_function(name)))
    }

    fn lookup_variable(&self, name: &str) -> Option<&Type> {
        self.env
            .lookup_variable(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_variable(name)))
    }

    fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.env
            .lookup_type(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_type(name)))
    }

    fn lookup_identifier(&self, name: &str) -> Option<IdentifierType> {
        Some(if let Some(ty) = self.lookup_type(name) {
            IdentifierType::Type(ty)
        } else if let Some(variants) = self.lookup_function(name) {
            IdentifierType::Function(variants)
        } else if let Some(ty) = self.lookup_variable(name) {
            IdentifierType::Variable(ty)
        } else {
            return None;
        })
    }

    fn lookup_local_identifier(&self, name: &str) -> Option<IdentifierType> {
        Some(if let Some(ty) = self.env.lookup_type(name) {
            IdentifierType::Type(ty)
        } else if let Some(variants) = self.env.lookup_function(name) {
            IdentifierType::Function(variants)
        } else if let Some(ty) = self.env.lookup_variable(name) {
            IdentifierType::Variable(ty)
        } else {
            return None;
        })
    }

    fn lookup_operator(&self, symbol: &str) -> Vec<&Operator> {
        self.env
            .operators
            .iter()
            .filter(|o| o.info.symbol == symbol)
            .chain(self.parent.iter().flat_map(|p| p.lookup_operator(symbol)))
            .collect()
    }

    // ================== Scanning functions ==================

    fn scan_forward(&mut self, expr: &[Ast]) -> TypeResult<()> {
        for e in expr {
            if let Ast::Assignment(target, expr) = e {
                let Ast::Identifier(name) = target.borrow() else {
                    continue;
                };
                match expr.borrow() {
                    Ast::Function {
                        params,
                        body,
                        return_type,
                    } => {
                        let checked = self.check_function(params, body, return_type)?;
                        let variation = VariationType {
                            params: params.clone(),
                            ret: checked.get_type().clone(),
                        };
                        self.env.add_function(name.clone(), variation);
                    }
                    _ => {
                        let checked = self.check_expr(expr)?;
                        self.env.add_variable(name, checked.get_type().clone());
                    }
                }
            }
        }
        Ok(())
    }

    // ================== Type checking functions ==================

    pub fn check_module(&mut self, module: &Module) -> TypeResult<CheckedModule> {
        Ok(CheckedModule {
            name: module.name.clone(),
            expressions: self.check_top_exprs(&module.expressions)?,
            source: module.source.clone(),
        })
    }

    pub fn check_top_exprs(&mut self, exprs: &[Ast]) -> TypeResult<Vec<CheckedAst>> {
        self.scan_forward(exprs)?;
        let mut res = vec![];
        for e in exprs {
            res.push(self.check_expr(e)?);
        }
        Ok(res)
    }

    /// Check the type of an expression
    pub fn check_expr(&mut self, expr: &Ast) -> TypeResult<CheckedAst> {
        Ok(match expr {
            Ast::Function {
                params,
                body,
                return_type,
            } => self.check_function(params, body, return_type)?,
            Ast::Literal(v) => CheckedAst::Literal(v.clone()),
            Ast::Tuple(elems) => self.check_tuple(elems)?,
            Ast::List(elems) => self.check_list(elems)?,
            Ast::Record(pairs) => self.check_record(pairs)?,
            Ast::Identifier(i) => self.check_identifier(i)?,
            Ast::FunctionCall(name, args) => self.check_call(name, args)?,
            Ast::Accumulate(info, operands) => self.check_accumulate(info, operands)?,
            Ast::Binary(lhs, info, rhs) => self.check_binary(lhs, info, rhs)?,
            Ast::Unary(info, operand) => self.check_unary(info, operand)?,
            Ast::Assignment(target, expr) => self.check_assignment(target, expr)?,
            Ast::Block(exprs) => self.check_block(exprs)?,
        })
    }

    fn check_function(
        &mut self,
        params: &FunctionParameterType,
        body: &Ast,
        return_type: &Option<Type>,
    ) -> TypeResult<CheckedAst> {
        let body = self.new_scope().check_expr(body)?;
        let body_type = body.get_type().clone();
        let return_type = if let Some(ty) = &return_type {
            if !ty.subtype(&body_type) {
                return Err(TypeError {
                    message: format!(
						"Function body type does not match the return type. Expected '{}', found '{}'",
						ty, &body_type
					),
                });
            }
            ty.clone()
        } else {
            // Infer the return type from the body
            body_type
        };
        Ok(CheckedAst::Function {
            params: params.clone(),
            body: Box::new(body),
            return_type,
        })
    }

    fn check_tuple(&mut self, elems: &[Ast]) -> TypeResult<CheckedAst> {
        if elems.is_empty() {
            return Ok(CheckedAst::Tuple(vec![], std_types::UNIT));
        }
        let checked_elems = self.check_top_exprs(elems)?;
        let elem_types = checked_elems
            .iter()
            .map(|e| e.get_type())
            .cloned()
            .collect::<Vec<_>>();
        Ok(CheckedAst::Tuple(checked_elems, Type::Tuple(elem_types)))
    }

    fn check_list(&mut self, elems: &[Ast]) -> TypeResult<CheckedAst> {
        let checked_elems = self.check_top_exprs(elems)?;
        let mut elem_types = checked_elems
            .iter()
            .map(|e| e.get_type())
            .cloned()
            .collect::<Vec<_>>();
        elem_types.dedup();
        let elem_type = if elem_types.len() == 1 {
            elem_types[0].clone()
        } else {
            Type::Sum(elem_types)
        };
        Ok(CheckedAst::List(
            checked_elems,
            Type::List(Box::new(elem_type)),
        ))
    }

    fn check_record(&mut self, pairs: &[(RecordKey, Ast)]) -> TypeResult<CheckedAst> {
        let pairs = pairs
            .iter()
            .map(|(k, v)| Ok((k.clone(), self.check_expr(v)?)))
            .collect::<TypeResult<Vec<_>>>()?;
        let record_type = Type::Record(
            pairs
                .iter()
                .map(|(k, v)| (k.clone(), v.get_type().clone()))
                .collect(),
        );
        Ok(CheckedAst::Record(pairs, record_type))
    }

    fn check_identifier(&self, name: &str) -> TypeResult<CheckedAst> {
        Ok(match self.lookup_identifier(name) {
            Some(IdentifierType::Variable(ty)) => {
                CheckedAst::Identifier(name.to_string(), ty.clone())
            }
            Some(IdentifierType::Type(ty)) => CheckedAst::Literal(Value::Type(ty.clone())),
            Some(IdentifierType::Function(variants)) => {
                CheckedAst::Identifier(name.to_string(), Type::Function(variants.to_vec()))
            }
            None => {
                return Err(TypeError {
                    message: format!("Unknown variable: {}", name),
                })
            }
        })
    }

    fn check_assignment(&mut self, target: &Ast, expr: &Ast) -> TypeResult<CheckedAst> {
        let target = match target {
            Ast::Identifier(name) => name,
            _ => {
                return Err(TypeError {
                    message: "Assignment expects an identifier".to_string(),
                })
            }
        };
        if let Some(existing) = self.lookup_local_identifier(target) {
            let ty_name = match existing {
                IdentifierType::Variable(_) => "Variable",
                IdentifierType::Type(_) => "Type",
                IdentifierType::Function(_) => "Function",
            };
            return Err(TypeError {
                message: format!("{} '{}' already exists", ty_name, target),
            });
        }
        let expr = self.check_expr(expr)?;
        let ty = expr.get_type().clone();
        self.env.add_variable(target, ty.clone());
        Ok(CheckedAst::Assignment(
            Box::new(CheckedAst::Identifier(target.to_string(), ty.clone())),
            Box::new(expr),
            ty,
        ))
    }

    fn check_block(&mut self, exprs: &[Ast]) -> TypeResult<CheckedAst> {
        let mut scope = self.new_scope();
        let exprs = scope.check_top_exprs(exprs)?;
        let ty = if let Some(expr) = exprs.last() {
            expr.get_type().clone()
        } else {
            std_types::UNIT
        };
        Ok(CheckedAst::Block(exprs, ty))
    }

    fn check_call(&mut self, name: &str, args: &[Ast]) -> TypeResult<CheckedAst> {
        let args = args
            .iter()
            .map(|a| self.check_expr(a))
            .collect::<TypeResult<Vec<_>>>()?;
        let arg_types = args.iter().map(|a| a.get_type()).collect::<Vec<_>>();
        let variants = self.lookup_function(name).ok_or_else(|| TypeError {
            message: format!("Unknown function: {}", name),
        })?;
        for variant in variants {
            if variant.get_params().match_args_types(&arg_types) {
                return Ok(CheckedAst::Call {
                    function: name.to_string(),
                    variation: Box::new(variant.clone()),
                    args,
                });
            }
        }
        Err(TypeError {
            message: format!(
                "Function '{}' has no variant with {} arguments of types: ({})",
                name,
                args.len(),
                arg_types
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        })
    }

    /// Check the type of an accumulate expression.
    /// An accumulate expression results in a function variation-specific call.
    fn check_accumulate(
        &mut self,
        info: &OperatorInfo,
        operands: &[Ast],
    ) -> TypeResult<CheckedAst> {
        let checked_operands = operands
            .iter()
            .map(|a| self.check_expr(a))
            .collect::<TypeResult<Vec<_>>>()?;
        let operand_types = checked_operands
            .iter()
            .map(|a| a.get_type())
            .collect::<Vec<_>>();
        for op in self.lookup_operator(&info.symbol) {
            if op.signature().params.match_args_types(&operand_types) {
                match &op.handler {
                    OperatorHandler::Runtime { handler, .. } => {
                        return Ok(CheckedAst::DirectCall {
                            variation: Box::new(*handler.clone()),
                            args: checked_operands,
                        });
                    }
                    OperatorHandler::Static(_, handler) => {
                        // Evaluate the handler at compile-time
                        let ast = handler(StaticOperatorAst::Accumulate(operands.to_vec()));
                        return self.check_expr(&ast);
                    }
                }
            }
        }

        Err(TypeError {
            message: format!("Unknown operator: {}", info.symbol),
        })
    }

    fn check_binary(
        &mut self,
        lhs: &Ast,
        info: &OperatorInfo,
        rhs: &Ast,
    ) -> TypeResult<CheckedAst> {
        let ops = self.lookup_operator(&info.symbol);
        if ops.len() == 1 {
            if let OperatorHandler::Static(_, handler) = ops[0].handler {
                return self
                    .check_expr(&handler(StaticOperatorAst::Infix(lhs.clone(), rhs.clone())));
            }
        }
        let checked_lhs = self.check_expr(lhs)?;
        let checked_rhs = self.check_expr(rhs)?;
        let lhs_type = checked_lhs.get_type();
        let rhs_type = checked_rhs.get_type();
        for op in self.lookup_operator(&info.symbol) {
            if op
                .signature()
                .params
                .match_args_types(&[lhs_type, rhs_type])
            {
                match &op.handler {
                    OperatorHandler::Runtime { handler, .. } => {
                        return Ok(CheckedAst::DirectCall {
                            variation: Box::new(*handler.clone()),
                            args: vec![checked_lhs, checked_rhs],
                        });
                    }
                    OperatorHandler::Static(_, handler) => {
                        // Evaluate the handler at compile-time
                        let ast = handler(StaticOperatorAst::Infix(lhs.clone(), rhs.clone()));
                        return self.check_expr(&ast);
                    }
                }
            }
        }
        Err(TypeError {
            message: format!("Unknown operator: {}", info.symbol),
        })
    }

    fn check_unary(&mut self, info: &OperatorInfo, operand: &Ast) -> TypeResult<CheckedAst> {
        let checked_operand = self.check_expr(operand)?;
        let operand_type = checked_operand.get_type();
        for op in self.lookup_operator(&info.symbol) {
            if op.signature().params.match_args_types(&[operand_type]) {
                match &op.handler {
                    OperatorHandler::Runtime { handler, .. } => {
                        return Ok(CheckedAst::DirectCall {
                            variation: Box::new(*handler.clone()),
                            args: vec![checked_operand],
                        });
                    }
                    OperatorHandler::Static(_, handler) => {
                        // Evaluate the handler at compile-time
                        let ast = handler(StaticOperatorAst::Prefix(operand.clone()));
                        return self.check_expr(&ast);
                    }
                }
            }
        }
        Err(TypeError {
            message: format!("Unknown operator: {}", info.symbol),
        })
    }

    // ================== Type inference functions ==================
}
