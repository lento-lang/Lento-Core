#[cfg(test)]
mod tests {
    use std::io::{Read, Seek};

    use crate::{
        interpreter::value::Value, lexer::{
            lexer::{from_str, from_string, Lexer},
            op::{default_operator_precedence, Operator, OperatorAssociativity, OperatorPosition, OperatorPrecedence, StaticOperator},
            token::TokenKind,
        }, parser::ast::Ast, stdlib::init::init_lexer
    };

    fn assert_next_token_eq<R: Read + Seek>(lexer: &mut Lexer<R>, token: TokenKind) {
        assert_eq!(lexer.read_next_token().unwrap().token, token);
    }

    #[test]
    fn function() {
        let mut lexer = from_str("add a b = a + b");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("add".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("b".to_string()));
        assert!(matches!(
            lexer.read_next_token().unwrap().token,
            TokenKind::Op(Operator::Static(_))
        ));
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert!(matches!(
            lexer.read_next_token().unwrap().token,
            TokenKind::Op(Operator::Runtime(_))
        ));
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("b".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn assign() {
        let mut lexer = from_str("x = 1;");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("x".to_string()));
        assert!(matches!(
            lexer.read_next_token().unwrap().token,
            TokenKind::Op(Operator::Static(_))
        ));
        assert_next_token_eq(&mut lexer, TokenKind::Integer("1".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::SemiColon);
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn many_match_operators() {
        let mut lexer = from_str("== = === ====");
        let equals = Operator::Static(StaticOperator {
            name: "equals".into(),
            symbol: "==".into(),
            position: OperatorPosition::Infix,
            precedence: default_operator_precedence::EQUALITY,
            associativity: OperatorAssociativity::Left,
            overloadable: false,
            allow_trailing: false,
            handler: |_| Ast::Literal(Value::Unit),
        });
        let assignment = Operator::Static(StaticOperator {
            name: "assign".into(),
            symbol: "=".into(),
            position: OperatorPosition::Infix,
            precedence: default_operator_precedence::ASSIGNMENT,
            associativity: OperatorAssociativity::Right,
            overloadable: false,
            allow_trailing: false,
            handler: |_| Ast::Literal(Value::Unit),
        });
        let strict_equals = Operator::Static(StaticOperator {
            name: "strict_equals".into(),
            symbol: "===".into(),
            position: OperatorPosition::Infix,
            precedence: default_operator_precedence::EQUALITY,
            associativity: OperatorAssociativity::Left,
            overloadable: false,
            allow_trailing: false,
            handler: |_| Ast::Literal(Value::Unit),
        });
        lexer.define_op(equals.clone()).unwrap();
        lexer.define_op(strict_equals.clone()).unwrap();
        lexer.define_op(assignment.clone()).unwrap();
        // ==
        assert_next_token_eq(&mut lexer, TokenKind::Op(equals));
        // =
        assert_next_token_eq(&mut lexer, TokenKind::Op(assignment.clone()));
        // ===
        assert_next_token_eq(&mut lexer, TokenKind::Op(strict_equals.clone()));
        // ====
        assert_next_token_eq(&mut lexer, TokenKind::Op(strict_equals));
        assert_next_token_eq(&mut lexer, TokenKind::Op(assignment));

        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn string() {
        let mut lexer = from_str(r#""Hello, World!""#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::String("Hello, World!".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn string_escape() {
        let mut lexer = from_str(r#""Hello, \"World\"!""#);
        init_lexer(&mut lexer);
        assert_next_token_eq(
            &mut lexer,
            TokenKind::String("Hello, \\\"World\\\"!".to_string()),
        );
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn char() {
        let mut lexer = from_str(r#"'a'"#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Char('a'));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn char_escape() {
        let mut lexer = from_str(r#"'\\'"#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Char('\\'));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn number() {
        let mut lexer = from_str("123.456");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Float("123.456".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn identifier() {
        let mut lexer = from_str("abc_123");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("abc_123".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn keywords() {
        let mut lexer = from_str("true false let");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Boolean(true));
        assert_next_token_eq(&mut lexer, TokenKind::Boolean(false));
        assert_next_token_eq(&mut lexer, TokenKind::Let);
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn comment() {
        let mut lexer = from_str("// This is a comment");
        init_lexer(&mut lexer);
        assert_next_token_eq(
            &mut lexer,
            TokenKind::Comment(" This is a comment".to_string()),
        );
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn types() {
        let types = [
            "unit", "str", "char", "bool", "u1", "u8", "u16", "u32", "u64", "u128", "ubig", "i8",
            "i16", "i32", "i64", "i128", "ibig", "f32", "f64", "fbig"];
        let mut lexer = from_string(types.join(" "));
        init_lexer(&mut lexer);
        for ty in types.iter() {
            assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier(ty.to_string()));
        }
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }
}
