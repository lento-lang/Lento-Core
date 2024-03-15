#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{
            environment::global_env,
            interpreter::interpret_ast,
            value::{Number, UnsignedInteger, Value},
        },
        lexer::op::Operator,
        parser::ast::Ast,
        stdlib::arithmetic,
        type_checker::types::{std_primitive_types, CheckedType, GetType, Type},
    };

    fn make_u8(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(n)))
    }

    fn add(lhs: Ast, rhs: Ast) -> Ast {
        Ast::Binary(
            Box::new(lhs),
            Operator::Runtime(arithmetic::op_add()),
            Box::new(rhs),
            CheckedType::Unchecked, // Not required for this test
        )
    }

    #[test]
    fn test_interpret_binary_add() {
        let ast = add(Ast::Literal(make_u8(1)), Ast::Literal(make_u8(2)));
        // TODO: let checked_ast = type_check(&ast).unwrap();
        let result = interpret_ast(&ast, &mut global_env()).unwrap();
        assert!(result.get_type().is_exact_type(&std_primitive_types::UINT8));
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn test_interpret_tuple() {
        let ast = Ast::Tuple(
            vec![
                Ast::Literal(make_u8(1)),
                Ast::Literal(make_u8(2)),
                Ast::Literal(make_u8(3)),
            ],
            CheckedType::Unchecked,
        );
        let result = interpret_ast(&ast, &mut global_env()).unwrap();
        assert!(result
            .get_type()
            .is_exact_type(&Type::Tuple(vec![std_primitive_types::UINT8; 3])));
        assert_eq!(
            result,
            Value::Tuple(
                vec![make_u8(1), make_u8(2), make_u8(3)],
                Type::Tuple(vec![std_primitive_types::UINT8; 3])
            )
        );
    }

    #[test]
    fn test_interpret_function_call() {
        let ast = Ast::FunctionCall(
            "add".to_string(),
            vec![Ast::Literal(make_u8(1)), Ast::Literal(make_u8(2))],
            CheckedType::Unchecked,
        );
        let result = interpret_ast(&ast, &mut global_env()).unwrap();
        assert!(result.get_type().is_exact_type(&std_primitive_types::UINT8));
        assert_eq!(result, make_u8(3));
    }
}
