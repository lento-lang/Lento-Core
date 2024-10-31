#[cfg(test)]
mod tests {
    use std::io::{Read, Seek};

    use crate::{
        lexer::{
            lexer::{from_str, from_string, Lexer},
            token::TokenKind,
        },
        stdlib::init::stdlib,
    };

    fn assert_next_token_eq<R: Read + Seek>(lexer: &mut Lexer<R>, token: TokenKind) {
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn function() {
        let mut lexer = from_str("add a b = a + b");
        stdlib().init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("add".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("b".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Op("=".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Op("+".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("b".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn assign() {
        let mut lexer = from_str("x = 1;");
        stdlib().init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("x".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Op("=".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Integer("1".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::SemiColon);
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn many_match_operators() {
        let mut lexer = from_str("== = === ====");
        let equals = "==".to_string();
        let assignment = "=".to_string();
        let strict_equals = "===".to_string();
        lexer.operators.insert(equals.clone());
        lexer.operators.insert(strict_equals.clone());
        lexer.operators.insert(assignment.clone());
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
        stdlib().init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::String("Hello, World!".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn string_escape() {
        let mut lexer = from_str(r#""Hello, \"World\"!""#);
        stdlib().init_lexer(&mut lexer);
        assert_next_token_eq(
            &mut lexer,
            TokenKind::String("Hello, \\\"World\\\"!".to_string()),
        );
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn char() {
        let mut lexer = from_str(r#"'a'"#);
        stdlib().init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Char('a'));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn char_escape() {
        let mut lexer = from_str(r#"'\\'"#);
        stdlib().init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Char('\\'));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn float() {
        let mut lexer = from_str("123.456");
        assert_next_token_eq(&mut lexer, TokenKind::Number("123.456".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn integer() {
        let mut lexer = from_str("123");
        assert_next_token_eq(&mut lexer, TokenKind::Integer("123".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn identifier() {
        let mut lexer = from_str("abc_123");
        stdlib().init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("abc_123".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn keywords() {
        let mut lexer = from_str("true false let");
        stdlib().init_lexer(&mut lexer);
        assert_next_token_eq(&mut lexer, TokenKind::Boolean(true));
        assert_next_token_eq(&mut lexer, TokenKind::Boolean(false));
        assert_next_token_eq(&mut lexer, TokenKind::Let);
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn comment() {
        let mut lexer = from_str("// This is a comment");
        stdlib().init_lexer(&mut lexer);
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
            "i16", "i32", "i64", "i128", "ibig", "f32", "f64", "fbig",
        ];
        let mut lexer = from_string(types.join(" "));
        stdlib().init_lexer(&mut lexer);
        for ty in types.iter() {
            assert_next_token_eq(&mut lexer, TokenKind::TypeIdentifier(ty.to_string()));
        }
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn commas() {
        let mut lexer = from_str("a, b, c");
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Comma);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("b".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Comma);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("c".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn colon() {
        let mut lexer = from_str("a: b");
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Colon);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("b".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn parens() {
        let mut lexer = from_str("(a)");
        assert_next_token_eq(&mut lexer, TokenKind::LeftParen);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::RightParen);
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn braces() {
        let mut lexer = from_str("{a}");
        assert_next_token_eq(&mut lexer, TokenKind::LeftBrace);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::RightBrace);
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn brackets() {
        let mut lexer = from_str("[a]");
        assert_next_token_eq(&mut lexer, TokenKind::LeftBracket);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::RightBracket);
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn operators() {
        let mut lexer = from_str("a + b");
        lexer.operators.insert("+".to_string());
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Op("+".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("b".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }

    #[test]
    fn semicolon() {
        let mut lexer = from_str("a; b");
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("a".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::SemiColon);
        assert_next_token_eq(&mut lexer, TokenKind::Identifier("b".to_string()));
        assert_next_token_eq(&mut lexer, TokenKind::EndOfFile);
    }
}
