#[cfg(test)]
mod tests {
    use std::io::{Read, Seek};

    use crate::{
        lexer::{
            lexer::{from_str, Lexer},
            op::Operator,
            token::TokenKind,
        },
        stdlib::init::init_lexer,
    };

    fn assert_next_token_eq<R: Read + Seek>(lexer: &mut Lexer<R>, token: TokenKind) {
        assert_eq!(lexer.read_next_token().unwrap().token, token);
    }

    #[test]
    fn test_lexer_function() {
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
    fn test_lexer_assign() {
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
    fn test_lexer_string() {
        let mut lexer = from_str(r#""Hello, World!""#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::String("Hello, World!".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn test_lexer_string_escape() {
        let mut lexer = from_str(r#""Hello, \"World\"!""#);
        init_lexer(&mut lexer);
        assert_next_token_eq(
            &mut lexer,
            TokenKind::String("Hello, \"World\"!".to_string()),
        );
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn test_lexer_char() {
        let mut lexer = from_str(r#"'a'"#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Char('a'));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn test_lexer_char_escape() {
        let mut lexer = from_str(r#"'\\'"#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Char('\\'));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn test_lexer_number() {
        let mut lexer = from_str("123.456");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Float("123.456".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn test_lexer_identifier() {
        let mut lexer = from_str("abc_123");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("abc_123".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn test_lexer_operator_add() {
        let mut lexer = from_str("+");
        init_lexer(&mut lexer);
        assert!(matches!(
            lexer.read_next_token().unwrap().token,
            TokenKind::Op(Operator::Runtime(_))
        ));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn test_lexer_keywords() {
        let mut lexer = from_str("true false let");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Boolean(true));
        assert_next_token_eq(&mut lexer, TokenKind::Boolean(false));
        assert_next_token_eq(&mut lexer, TokenKind::Let);
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn test_lexer_comment() {
        let mut lexer = from_str("// This is a comment");
        init_lexer(&mut lexer);
        assert_next_token_eq(
            &mut lexer,
            TokenKind::Comment(" This is a comment".to_string()),
        );
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn test_lexer_types() {
        let mut lexer = from_str(
            "unit str char bool u1 u8 u16 u32 u64 u128 ubig i8 i16 i32 i64 i128 ibig f32 f64 fbig",
        );
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("unit".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("str".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("char".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("bool".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("u1".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("u8".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("u16".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("u32".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("u64".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("u128".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("ubig".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("i8".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("i16".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("i32".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("i64".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("i128".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("ibig".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("f32".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("f64".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier("fbig".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }
}
