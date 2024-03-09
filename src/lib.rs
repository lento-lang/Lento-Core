#![allow(clippy::module_inception)]
#![feature(box_patterns)]

pub mod compiler;
pub mod conf;
pub mod doc;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod printer;
pub mod project;
pub mod stdlib;
pub mod type_checker;
pub mod util;
