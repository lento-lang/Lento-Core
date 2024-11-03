#[cfg(test)]
mod tests {
    use crate::{
        interpreter::value::Value,
        parser::parser::from_string,
        stdlib::init::stdlib,
        type_checker::{checked_ast::CheckedAst, checker::TypeChecker},
    };

    #[test]
    fn types() {
        let types = [
            "unit", "str", "char", "bool", "u1", "u8", "u16", "u32", "u64", "u128", "ubig", "i8",
            "i16", "i32", "i64", "i128", "ibig", "f32", "f64", "fbig",
        ];
        let mut parser = from_string(types.join(" "));
        stdlib().init_parser(&mut parser);
        let mut checker = TypeChecker::default();
        stdlib().init_type_checker(&mut checker);
        let ast = parser.parse_all().unwrap();
        let checked_ast = checker.check_top_exprs(&ast).unwrap();
        assert!(checked_ast.iter().zip(types).all(|(ast, ty)| {
            if let CheckedAst::Literal(Value::Type(t)) = ast {
                t.to_string() == ty
            } else {
                false
            }
        }))
    }
}
