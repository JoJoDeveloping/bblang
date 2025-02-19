use lex::{Lexer, Position};
use parse::Parser;

use crate::{compile::highast::Program, utils::string_interner::intern};

pub mod lex;
pub mod parse;

pub type Span = (Position, Position);

pub fn parse_program(path: &str, input: &str) -> Program {
    Parser::new(Lexer::new(intern(path), input.chars().collect())).parse_program()
}
