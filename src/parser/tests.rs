#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{
        interpreter::value::{Number, UnsignedInteger, Value},
        parser::{
            ast::Ast,
            parser::{parse_path_one, parse_str_all, parse_str_one},
        },
        stdlib::arithmetic::op_add,
        type_checker::types::CheckedType,
    };

    fn make_u8(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(n)))
    }

    #[test]
    fn test_parser_call_paren_apply() {
        let result = parse_str_one("println(\"Hello, World!\")");
        let expected = Ast::FunctionCall(
            "println".to_string(),
            vec![Ast::Literal(Value::String("Hello, World!".to_string()))],
            CheckedType::Unchecked,
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == expected);
    }

    // #[test]
    // fn test_parser_call_no_paren_apply() {
    //     let result = parse_str("println \"Hello, World!\"");
    //     let expected = Ast::FunctionCall(
    //         "println".to_string(),
    //         vec![Ast::Literal(Value::String("Hello, World!".to_string()))],
    //         CheckedType::Unchecked,
    //     );
    //     assert!(result.is_ok());
    //     assert!(result.unwrap() == expected);
    // }

    #[test]
    fn test_parser_call_tuple_apply() {
        let result = parse_str_one("println (\"Hello, World!\")");
        let expected = Ast::FunctionCall(
            "println".to_string(),
            vec![Ast::Literal(Value::String("Hello, World!".to_string()))],
            CheckedType::Unchecked,
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == expected);
    }

    #[test]
    fn test_parser_hello_world_file() {
        let result = parse_path_one(Path::new("./examples/basic/hello_world.lt"));
        let expected = Ast::FunctionCall(
            "println".to_string(),
            vec![Ast::Literal(Value::String("Hello, World!".to_string()))],
            CheckedType::Unchecked,
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == expected);
    }

    #[test]
    fn test_parser_arithmetic() {
        let result = parse_str_one("1 + 2");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(
            result.expressions[0],
            Ast::Binary(_, _, _, CheckedType::Unchecked)
        ));
        // Assert "add"
        if let Ast::Binary(_, op, _, _) = &result.expressions[0] {
            assert_eq!(op, &op_add());
        }
        if let Ast::Binary(lhs, _, rhs, _) = &result.expressions[0] {
            // Always true
            assert!(matches!(*lhs.to_owned(), Ast::Literal(Value::Number(_))));
            assert!(matches!(*rhs.to_owned(), Ast::Literal(Value::Number(_))));
        }
    }

    #[test]
    fn test_parser_assignment() {
        let result = parse_str_one("x = 1");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(
            result.expressions[0],
            Ast::Assignment(_, _, CheckedType::Unchecked)
        ));
    }

    #[test]
    fn test_parser_sequence() {
        let result = parse_str_all("1 2 3");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 3);
        assert!(result.expressions[0] == Ast::Literal(make_u8(1)));
        assert!(result.expressions[1] == Ast::Literal(make_u8(2)));
        assert!(result.expressions[2] == Ast::Literal(make_u8(3)));
    }

    #[test]
    fn test_parser_sequence_semicolon() {
        let result = parse_str_all("1; 2; 3;");
        dbg!(&result);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 3);
        assert!(result.expressions[0] == Ast::Literal(make_u8(1)));
        assert!(result.expressions[1] == Ast::Literal(make_u8(2)));
        assert!(result.expressions[2] == Ast::Literal(make_u8(3)));
    }
}
