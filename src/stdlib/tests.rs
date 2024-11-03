#[cfg(test)]
mod tests {
    use crate::{
        interpreter::environment::Environment, parser, stdlib::init::stdlib, util::str::Str,
    };

    #[test]
    fn test_init_parser() {
        let mut parser = parser::parser::from_str(""); // Initialize the parser (required for the lexer to work)
        stdlib().init_parser(&mut parser);
        assert!(parser.get_op("=").is_some());
        assert!(parser.get_op("+").is_some());
        assert!(parser.get_op(",").is_some());
        // Types
        assert!(parser.get_type("any").is_some());
        assert!(parser.get_type("unit").is_some());
        assert!(parser.get_type("str").is_some());
        assert!(parser.get_type("char").is_some());
        assert!(parser.get_type("bool").is_some());
        assert!(parser.get_type("u1").is_some());
        assert!(parser.get_type("u8").is_some());
        assert!(parser.get_type("u16").is_some());
        assert!(parser.get_type("u32").is_some());
        assert!(parser.get_type("u64").is_some());
        assert!(parser.get_type("u128").is_some());
        assert!(parser.get_type("ubig").is_some());
        assert!(parser.get_type("i8").is_some());
        assert!(parser.get_type("i16").is_some());
        assert!(parser.get_type("i32").is_some());
        assert!(parser.get_type("i64").is_some());
        assert!(parser.get_type("i128").is_some());
        assert!(parser.get_type("ibig").is_some());
        assert!(parser.get_type("f32").is_some());
        assert!(parser.get_type("f64").is_some());
        assert!(parser.get_type("fbig").is_some());
    }

    #[test]
    fn test_init_environment() {
        let mut env = Environment::new(Str::empty());
        stdlib().init_environment(&mut env);
        assert!(env.lookup_variable("pi").is_some());
        assert!(env.lookup_variable("tau").is_some());
        assert!(env.lookup_variable("e").is_some());
        assert!(env.lookup_variable("phi").is_some());
        assert!(env.lookup_variable("sqrt2").is_some());
        assert!(env.lookup_variable("add").is_some());
        assert!(env.lookup_variable("print").is_some());
        assert!(env.lookup_variable("exit").is_some());
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
