#[cfg(test)]
mod tests {
    use std::io::{Read, Seek};

    use crate::{
        lexer::{
            lexer::{from_str, Lexer},
            op::Operator,
            token::Token,
        },
        stdlib::init::init_lexer,
    };

    fn assert_next_token_eq<R: Read + Seek>(lexer: &mut Lexer<R>, token: Token) {
        assert_eq!(lexer.read_next_token().unwrap().token, token);
    }

    #[test]
    fn test_lexer_function() {
        let mut lexer = from_str("add a b = a + b");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, Token::Identifier("add".to_string()));
        assert_next_token_eq(&mut lexer, Token::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, Token::Identifier("b".to_string()));
        assert!(matches!(
            lexer.read_next_token().unwrap().token,
            Token::Op(Operator::Static(_))
        ));
        assert_next_token_eq(&mut lexer, Token::Identifier("a".to_string()));
        assert!(matches!(
            lexer.read_next_token().unwrap().token,
            Token::Op(Operator::Runtime(_))
        ));
        assert_next_token_eq(&mut lexer, Token::Identifier("b".to_string()));
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }

    #[test]
    fn test_lexer_assign() {
        let mut lexer = from_str("x = 1;");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, Token::Identifier("x".to_string()));
        assert!(matches!(
            lexer.read_next_token().unwrap().token,
            Token::Op(Operator::Static(_))
        ));
        assert_next_token_eq(&mut lexer, Token::Integer("1".to_string()));
        assert_next_token_eq(&mut lexer, Token::SemiColon);
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }

    #[test]
    fn test_lexer_string() {
        let mut lexer = from_str(r#""Hello, World!""#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, Token::String("Hello, World!".to_string()));
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }

    #[test]
    fn test_lexer_string_escape() {
        let mut lexer = from_str(r#""Hello, \"World\"!""#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, Token::String("Hello, \"World\"!".to_string()));
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }

    #[test]
    fn test_lexer_char() {
        let mut lexer = from_str(r#"'a'"#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, Token::Char('a'));
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }

    #[test]
    fn test_lexer_char_escape() {
        let mut lexer = from_str(r#"'\\'"#);
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, Token::Char('\\'));
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }

    #[test]
    fn test_lexer_number() {
        let mut lexer = from_str("123.456");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, Token::Float("123.456".to_string()));
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }

    #[test]
    fn test_lexer_identifier() {
        let mut lexer = from_str("abc_123");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, Token::Identifier("abc_123".to_string()));
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }

    #[test]
    fn test_lexer_operator_add() {
        let mut lexer = from_str("+");
        init_lexer(&mut lexer);
        assert!(matches!(
            lexer.read_next_token().unwrap().token,
            Token::Op(Operator::Runtime(_))
        ));
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }

    #[test]
    fn test_lexer_keywords() {
        let mut lexer = from_str("true false let");
        init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, Token::Boolean(true));
        assert_next_token_eq(&mut lexer, Token::Boolean(false));
        assert_next_token_eq(&mut lexer, Token::Let);
        assert_next_token_eq(&mut lexer, Token::EndOfFile);
    }
}
