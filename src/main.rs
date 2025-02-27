#![feature(hash_raw_entry)]
#![feature(mixed_integer_ops_unsigned_sub)]
#![feature(unsigned_signed_diff)]

use std::env;

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
    let args: Vec<_> = env::args().collect();
    let filename = &args[1];
    if filename.ends_with(".sl") {
        let prog = parse_and_compile_program(filename, &read_file(filename));

        let mut cfg = MachineState::start(prog, intern("main"), vec![]);
        let res = cfg.run();
        println!("{:?}", res);
        println!("{}", cfg);
    } else if filename.ends_with(".spec") {
        let prog = specs::parse_spec_program(filename, &read_file(filename));
        specs::run_spec_program(prog);
    }
}
