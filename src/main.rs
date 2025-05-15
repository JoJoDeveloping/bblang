#![feature(hash_raw_entry)]
#![feature(mixed_integer_ops_unsigned_sub)]
#![feature(unsigned_signed_diff)]
#![feature(mapped_lock_guards)]

use std::env;

use compile::highast;
use parse::parse_program;
use progs::{ast::Program, machine::MachineState};
use utils::{read_file, string_interner::intern};
pub mod compile;
mod examples;
pub mod ownership;
pub mod parse;
pub mod progs;
pub mod specs;
mod tests;
pub mod utils;

pub fn parse_and_compile_program(path: &str, input: &str) -> Program {
    parse_program(path, input).compile()
}

fn main() {
    let mut program = highast::Program::new();
    for filename in env::args() {
        if filename.ends_with(".sl") {
            program.merge(parse_program(&filename, &read_file(&filename)));
        } else if filename.ends_with(".spec") {
            let mut prog = specs::parse_spec_program(&filename, &read_file(&filename));
            program.spec_stuff.append(&mut prog);
        }
    }

    let prog = program.compile();

    let mut cfg = MachineState::start(prog, intern("main"), vec![]);
    let res = cfg.run();
    println!("{:?}", res);
    println!("{}", cfg);
}
