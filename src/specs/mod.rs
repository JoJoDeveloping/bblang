use exec::ExecCtx;
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
