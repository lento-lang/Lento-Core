#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{
            environment::{global_env, Environment},
            interpreter::{interpret_ast, interpret_module},
            number::{Number, UnsignedInteger},
            value::{FunctionVariation, Value},
        },
        parser::parser,
        stdlib::arithmetic,
        type_checker::{
            checked_ast::CheckedAst,
            checker::TypeChecker,
            types::{std_types, FunctionParameterType, GetType, Type},
        },
        util::str::Str,
    };

    fn empty_env() -> Environment<'static> {
        Environment::new(Str::Str("empty"))
    }

    fn make_u8(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(n)))
    }

    fn add(lhs: CheckedAst, rhs: CheckedAst) -> CheckedAst {
        CheckedAst::DirectCall {
            variation: Box::new(arithmetic::add()),
            args: vec![lhs, rhs],
        }
    }

    #[test]
    fn binary_add() {
        let ast = add(
            CheckedAst::Literal(make_u8(1)),
            CheckedAst::Literal(make_u8(2)),
        );
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &std_types::UINT8);
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn tuple() {
        let ast = CheckedAst::Tuple(
            vec![
                CheckedAst::Literal(make_u8(1)),
                CheckedAst::Literal(make_u8(2)),
                CheckedAst::Literal(make_u8(3)),
            ],
            Type::Tuple(vec![std_types::UINT8; 3]),
        );
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &Type::Tuple(vec![std_types::UINT8; 3]));
        assert_eq!(
            result,
            Value::Tuple(
                vec![make_u8(1), make_u8(2), make_u8(3)],
                Type::Tuple(vec![std_types::UINT8; 3])
            )
        );
    }

    #[test]
    fn function_call() {
        let ast = CheckedAst::DirectCall {
            variation: Box::new(arithmetic::add()),
            args: vec![
                CheckedAst::Literal(make_u8(1)),
                CheckedAst::Literal(make_u8(2)),
            ],
        };
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &std_types::UINT8);
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn unit_function() {
        let ast = CheckedAst::DirectCall {
            variation: Box::new(FunctionVariation::new_user(
                FunctionParameterType::Singles(vec![]),
                CheckedAst::Block(vec![], std_types::UNIT),
                empty_env(), // Empty environment
                std_types::UNIT,
            )),
            args: vec![],
        };
        let result = interpret_ast(&ast, &mut empty_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &std_types::UNIT);
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn invalid_function() {
        let ast = CheckedAst::DirectCall {
            variation: Box::new(FunctionVariation::new_user(
                FunctionParameterType::Singles(vec![]),
                CheckedAst::Block(vec![], std_types::UNIT),
                empty_env(), // Empty environment
                std_types::UNIT,
            )),
            args: vec![CheckedAst::Literal(make_u8(1))],
        };
        let result = interpret_ast(&ast, &mut empty_env());
        assert!(result.is_err());
    }

    #[test]
    fn assignment() {
        let ast = CheckedAst::Assignment(
            Box::new(CheckedAst::Identifier("x".to_string(), std_types::UINT8)),
            Box::new(CheckedAst::Literal(make_u8(1))),
            std_types::UINT8,
        );
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &std_types::UINT8);
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
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let result = interpret_module(&module, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type() == &std_types::UINT8);
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
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 -> u8 {
				x + y + z
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_args() {
        let module = parser::parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 {
				x + y + z
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_paren_implicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add(x, y, z) {
				x + y + z
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_implicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add x y z {
				x + y + z
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_implicit_random_parens() {
        let module = parser::parse_str_all(
            r#"
			add x y (z) a (b) (c) -> u8 {
				x + y + z + a + b + c
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_paren_explicit_signature_oneline() {
        let module = parser::parse_str_all(
            r#"
			add(x: u8, y: u8, z: u8) -> u8 = x + y + z;
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_signature_oneline() {
        let module = parser::parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 -> u8 = x + y + z;
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = global_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }
}
