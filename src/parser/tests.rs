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
    fn number() {
        let result = parse_str_one("1");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == Ast::Literal(make_u8(1)));
    }

    #[test]
    fn number_many() {
        let result = parse_str_all("1 2 3 4 5");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 5);
        assert!(result.expressions[0] == Ast::Literal(make_u8(1)));
        assert!(result.expressions[1] == Ast::Literal(make_u8(2)));
        assert!(result.expressions[2] == Ast::Literal(make_u8(3)));
        assert!(result.expressions[3] == Ast::Literal(make_u8(4)));
        assert!(result.expressions[4] == Ast::Literal(make_u8(5)));
    }

    #[test]
    fn number_many_semicolon() {
        let result = parse_str_all("1; 2; 3;");
        dbg!(&result);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 3);
        assert!(result.expressions[0] == Ast::Literal(make_u8(1)));
        assert!(result.expressions[1] == Ast::Literal(make_u8(2)));
        assert!(result.expressions[2] == Ast::Literal(make_u8(3)));
    }

    #[test]
    fn number_par() {
        let result = parse_str_one("(1)");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == Ast::Literal(make_u8(1)));
    }

    #[test]
    fn tuple_1() {
        let result = parse_str_one("(1,)");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_, _)));
        if let Ast::Tuple(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 1);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
        }
    }

    #[test]
    fn tuple_1_no_par() {
        let result = parse_str_one("1,");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_, _)));
        if let Ast::Tuple(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 1);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
        }
    }

    #[test]
    fn tuple_2() {
        let result = parse_str_one("(1, 2)");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_, _)));
        if let Ast::Tuple(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 2);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
        }
    }

    #[test]
    fn tuple_2_no_par() {
        let result = parse_str_one("1, 2");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_, _)));
        if let Ast::Tuple(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 2);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
        }
    }

    #[test]
    fn tuple_3() {
        let result = parse_str_one("(1, 2, 3)");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        dbg!(&result.expressions[0]);
        assert!(matches!(result.expressions[0], Ast::Tuple(_, _)));
        if let Ast::Tuple(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
            assert_eq!(elems[2], Ast::Literal(make_u8(3)));
        }
    }

    #[test]
    fn tuple_3_no_par() {
        let result = parse_str_one("1, 2, 3");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        dbg!(&result.expressions[0]);
        assert!(matches!(result.expressions[0], Ast::Tuple(_, _)));
        if let Ast::Tuple(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
            assert_eq!(elems[2], Ast::Literal(make_u8(3)));
        }
    }

    #[test]
    fn tuple_3_trailing() {
        let result = parse_str_one("(1, 2, 3,)");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_, _)));
        if let Ast::Tuple(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
            assert_eq!(elems[2], Ast::Literal(make_u8(3)));
        }
    }

    #[test]
    fn tuple_3_no_par_trailing() {
        let result = parse_str_one("1, 2, 3,");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_, _)));
        if let Ast::Tuple(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
            assert_eq!(elems[2], Ast::Literal(make_u8(3)));
        }
    }

    #[test]
    fn tuple_addition() {
        let result = parse_str_one("(1, 2) + (3, 4)");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Binary(_, _, _, CheckedType::Unchecked)));
        if let Ast::Binary(lhs, op, rhs, _) = &result.expressions[0] {
            assert_eq!(op, &op_add());
            assert!(matches!(*lhs.to_owned(), Ast::Tuple(_, _)));
            assert!(matches!(*rhs.to_owned(), Ast::Tuple(_, _)));
        }
    }

    #[test]
    fn tuple_with_addition() {
        let result = parse_str_one("1, 2 + 3, 4");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_, _)));
        if let Ast::Tuple(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
            assert!(matches!(&elems[1], Ast::Binary(_, _, _, CheckedType::Unchecked)));
            assert_eq!(elems[2], Ast::Literal(make_u8(4)));
            if let Ast::Binary(lhs, op, rhs, _) = &elems[1] {
                assert_eq!(op, &op_add());
                assert_eq!(**lhs, Ast::Literal(make_u8(2)));
                assert_eq!(**rhs, Ast::Literal(make_u8(3)));
            }
        }
    }

    #[test]
    fn list_3() {
        let result = parse_str_one("[1, 2, 3]");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::List(_, _)));
        if let Ast::List(elems, _) = &result.expressions[0] {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], Ast::Literal(make_u8(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
            assert_eq!(elems[2], Ast::Literal(make_u8(3)));
        }
    }

    #[test]
    fn call_paren_apply() {
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
    // fn call_no_paren_apply() {
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
    fn call_tuple_apply() {
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
    fn hello_world_file() {
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
    fn arithmetic() {
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
    fn arithmetic_tree() {
        let result = parse_str_one("1 + 2 + 3");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(
            result.expressions[0],
            Ast::Binary(_, _, _, CheckedType::Unchecked)
        ));
        // Assert left side
        if let Ast::Binary(lhs, _, _, _) = &result.expressions[0] {
            assert!(matches!(
                **lhs,
                Ast::Binary(_, _, _, CheckedType::Unchecked)
            ));
        }
        // dbg!(&result.expressions[0].print_sexpr());
    }

    #[test]
    fn assignment() {
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
    fn assign_add() {
        let result = parse_str_one("x = 1 + 2");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(
            result.expressions[0],
            Ast::Assignment(_, _, CheckedType::Unchecked)
        ));
        if let Ast::Assignment(lhs, rhs, _) = &result.expressions[0] {
            assert!(matches!(
                *lhs.to_owned(),
                Ast::Identifier(_, CheckedType::Unchecked)
            ));
            assert!(matches!(
                *rhs.to_owned(),
                Ast::Binary(_, _, _, CheckedType::Unchecked)
            ));
        }
    }

    #[test]
    fn comment() {
        let result = parse_str_all("1; // This is a comment");
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == Ast::Literal(make_u8(1)));
    }

    #[test]
    fn comment_newline() {
        let result = parse_str_all(
            r#"
			// This is a comment
			1; // This is a comment
			2;
			// This is a comment
		"#,
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 2);
        assert!(result.expressions[0] == Ast::Literal(make_u8(1)));
        assert!(result.expressions[1] == Ast::Literal(make_u8(2)));
    }

    // #[test]
    // fn arithmetic_complex() {
    //     let result = parse_str_one("5 * (10 - 2) / 2 + 1");
    //     assert!(result.is_ok());
    //     let result = result.unwrap();
    //     assert!(result.expressions.len() == 1);
    //     assert!(matches!(
    //         result.expressions[0],
    //         Ast::Binary(_, _, _, CheckedType::Unchecked)
    //     ));
    // }
}
