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
    }

    #[test]
    fn test_init_environment() {
        let mut env = Environment::new(Str::from("test"));
        init_environment(&mut env);
        assert!(env.get_value("true").is_some());
        assert!(env.get_value("false").is_some());
        assert!(env.get_value("pi").is_some());
        assert!(env.get_value("tau").is_some());
        assert!(env.get_value("e").is_some());
        assert!(env.get_value("phi").is_some());
        assert!(env.get_value("sqrt2").is_some());
        assert!(env.get_value("print").is_some());
        assert!(env.get_value("add").is_some());
    }
}
