#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{
            environment::{global_env, Environment},
            interpreter::{interpret_ast, interpret_module},
            number::{Number, UnsignedInteger},
            value::Value,
        },
        parser::{ast::Ast, op::RuntimeOperator, parser},
        stdlib::arithmetic,
        type_checker::types::{std_primitive_types, CheckedType, GetType, Type},
        util::str::Str,
    };

    fn make_u8(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(n)))
    }

    fn add(lhs: Ast, rhs: Ast) -> Ast {
        Ast::Binary(
            Box::new(lhs),
            RuntimeOperator {
                name: "add".into(),
                symbol: "+".into(),
                handler: Box::new(arithmetic::add()),
            },
            Box::new(rhs),
            CheckedType::Unchecked, // Not required for this test
        )
    }

    #[test]
    fn binary_add() {
        let ast = add(Ast::Literal(make_u8(1)), Ast::Literal(make_u8(2)));
        // TODO: let checked_ast = type_check(&ast).unwrap();
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &std_primitive_types::UINT8);
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn tuple() {
        let ast = Ast::Tuple(
            vec![
                Ast::Literal(make_u8(1)),
                Ast::Literal(make_u8(2)),
                Ast::Literal(make_u8(3)),
            ],
            CheckedType::Unchecked,
        );
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &Type::Tuple(vec![std_primitive_types::UINT8; 3]));
        assert_eq!(
            result,
            Value::Tuple(
                vec![make_u8(1), make_u8(2), make_u8(3)],
                Type::Tuple(vec![std_primitive_types::UINT8; 3])
            )
        );
    }

    #[test]
    fn function_call() {
        let ast = Ast::FunctionCall(
            "add".to_string(),
            vec![Ast::Literal(make_u8(1)), Ast::Literal(make_u8(2))],
            CheckedType::Unchecked,
        );
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &std_primitive_types::UINT8);
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn invalid_function() {
        let ast = Ast::FunctionCall(
            "invalid_function_7194389034783682712840186".to_string(),
            vec![],
            CheckedType::Unchecked,
        );
        let result = interpret_ast(&ast, &mut Environment::new(Str::Str("empty")));
        assert!(result.is_err());
    }

    #[test]
    fn assignment() {
        let ast = Ast::Assignment(
            Box::new(Ast::Identifier("x".to_string(), CheckedType::Unchecked)),
            Box::new(Ast::Literal(make_u8(1))),
            CheckedType::Unchecked,
        );
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &std_primitive_types::UINT8);
        assert_eq!(result, make_u8(1));
    }

    #[test]
    fn module_assign_add() {
        let module = parser::parse_str_all(
            r#"
			x = 1;
			y = 2;
			z = x + y;
		"#,
        )
        .expect("Failed to parse module");
        let result = interpret_module(&module, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &std_primitive_types::UINT8);
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn function_decl_paren_explicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add(x: u8, y: u8, z: u8) -> u8 {
				x + y + z
			}
		"#,
        )
        .expect("Failed to parse module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.get_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 -> u8 {
				x + y + z
			}
		"#,
        )
        .expect("Failed to parse module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.get_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_args() {
        let module = parser::parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 {
				x + y + z
			}
		"#,
        )
        .expect("Failed to parse module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.get_function("add").is_some());
    }

    #[test]
    fn function_decl_paren_implicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add(x, y, z) {
				x + y + z
			}
		"#,
        )
        .expect("Failed to parse module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.get_function("add").is_some());
    }

    #[test]
    fn function_decl_implicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add x y z {
				x + y + z
			}
		"#,
        )
        .expect("Failed to parse module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.get_function("add").is_some());
    }

    #[test]
    fn function_decl_implicit_random_parens() {
        let module = parser::parse_str_all(
            r#"
			add x y (z) a (b) (c) -> u8 {
				x + y + z + a + b + c
			}
		"#,
        )
        .expect("Failed to parse module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.get_function("add").is_some());
    }

    #[test]
    fn function_decl_paren_explicit_signature_oneline() {
        let module = parser::parse_str_all(
            r#"
			add(x: u8, y: u8, z: u8) -> u8 = x + y + z;
		"#,
        )
        .expect("Failed to parse module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.get_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_signature_oneline() {
        let module = parser::parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 -> u8 = x + y + z;
		"#,
        )
        .expect("Failed to parse module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.get_function("add").is_some());
    }
}
