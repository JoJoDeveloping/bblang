#![feature(hash_raw_entry)]
#![feature(mixed_integer_ops_unsigned_sub)]
#![feature(unsigned_signed_diff)]

use std::env;

use parse::parse_program;
use progs::{ast::Program, machine::MachineState};
use utils::{read_file, string_interner::intern};
pub mod compile;
mod examples;
pub mod parse;
pub mod progs;
pub mod utils;

pub fn parse_and_compile_program(input: &str) -> Program {
    parse_program(input).compile()
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let prog = parse_and_compile_program(&read_file(&args[1]));

    let mut cfg = MachineState::start(prog, intern("main"), vec![]);
    let res = cfg.run();
    println!("{:?}", res);
    println!("{:#?}", cfg);
}
