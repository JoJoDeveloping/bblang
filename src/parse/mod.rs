use lex::Lexer;
use parse::Parser;

use crate::compile::highast::Program;

pub mod lex;
pub mod parse;

pub fn parse_program(input: &str) -> Program {
    Parser::new(Lexer::new(input.chars().collect())).parse_program()
}
