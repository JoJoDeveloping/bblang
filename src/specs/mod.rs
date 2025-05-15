use std::fmt::Display;

use exec::ExecCtx;
use num_bigint::BigInt;
use source_ast::SourceDef;
use typecheck::w::GlobalCtx;

use crate::{
    parse::{lex::Lexer, parse::Parser},
    utils::string_interner::intern,
};

mod builtin;
pub mod checked_ast;
pub mod exec;
pub mod source_ast;
pub mod typecheck;
pub mod values;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InfPointer(pub u32, pub BigInt);

pub fn parse_spec_program(path: &str, input: &str) -> Vec<SourceDef> {
    Parser::new(Lexer::new(intern(path), input.chars().collect())).parse_spec_program_eager()
}

pub fn run_spec_program(v: Vec<SourceDef>) {
    let mut global = GlobalCtx::new();
    let defs = global.check_defs(v).unwrap();
    let mut exec = ExecCtx::new();
    exec.consume_globals(&mut None, defs).unwrap();
    for ele in exec.globals_order {
        let ty = global.global_defs.get(&ele).unwrap();
        let val = exec.globals.get(&ele).unwrap();
        println!("{ele}: {ty} = {val}");
    }
}

impl Display for InfPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}
