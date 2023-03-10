pub mod conf;
pub mod lexer;
pub mod parser;
pub mod type_checker;
pub mod interpreter;
pub mod compiler;
pub mod project;
pub mod printer;
pub mod doc;
pub mod stdlib;
pub mod util;

fn _add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = _add(2, 2);
        assert_eq!(result, 4);
    }
}
