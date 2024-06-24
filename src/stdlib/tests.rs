#[cfg(test)]
mod tests {
    use crate::{
        interpreter::environment::Environment,
        lexer::lexer,
        stdlib::init::{init_environment, init_lexer},
        util::str::Str,
    };

    #[test]
    fn test_init_lexer() {
        let mut lexer = lexer::from_str("");
        init_lexer(&mut lexer);
        assert!(lexer.lookup_op("=").is_some());
        assert!(lexer.lookup_op("+").is_some());
        assert!(lexer.lookup_op(",").is_some());
        // Types
        assert!(lexer.is_type("any"));
        assert!(lexer.is_type("unit"));
        assert!(lexer.is_type("str"));
        assert!(lexer.is_type("char"));
        assert!(lexer.is_type("bool"));
        assert!(lexer.is_type("u1"));
        assert!(lexer.is_type("u8"));
        assert!(lexer.is_type("u16"));
        assert!(lexer.is_type("u32"));
        assert!(lexer.is_type("u64"));
        assert!(lexer.is_type("u128"));
        assert!(lexer.is_type("ubig"));
        assert!(lexer.is_type("i8"));
        assert!(lexer.is_type("i16"));
        assert!(lexer.is_type("i32"));
        assert!(lexer.is_type("i64"));
        assert!(lexer.is_type("i128"));
        assert!(lexer.is_type("ibig"));
        assert!(lexer.is_type("f32"));
        assert!(lexer.is_type("f64"));
        assert!(lexer.is_type("fbig"));
    }

    #[test]
    fn test_init_environment() {
        let mut env = Environment::new(Str::from("test"));
        init_environment(&mut env);
        assert!(env.get_value("pi").is_some());
        assert!(env.get_value("tau").is_some());
        assert!(env.get_value("e").is_some());
        assert!(env.get_value("phi").is_some());
        assert!(env.get_value("sqrt2").is_some());
        assert!(env.get_value("add").is_some());
        assert!(env.get_value("print").is_some());
        assert!(env.get_value("exit").is_some());
        // Types
        assert!(env.get_type("any").is_some());
        assert!(env.get_type("unit").is_some());
        assert!(env.get_type("str").is_some());
        assert!(env.get_type("char").is_some());
        assert!(env.get_type("bool").is_some());
        assert!(env.get_type("u1").is_some());
        assert!(env.get_type("u8").is_some());
        assert!(env.get_type("u16").is_some());
        assert!(env.get_type("u32").is_some());
        assert!(env.get_type("u64").is_some());
        assert!(env.get_type("u128").is_some());
        assert!(env.get_type("ubig").is_some());
        assert!(env.get_type("i8").is_some());
        assert!(env.get_type("i16").is_some());
        assert!(env.get_type("i32").is_some());
        assert!(env.get_type("i64").is_some());
        assert!(env.get_type("i128").is_some());
        assert!(env.get_type("ibig").is_some());
        assert!(env.get_type("f32").is_some());
        assert!(env.get_type("f64").is_some());
        assert!(env.get_type("fbig").is_some());
    }
}
