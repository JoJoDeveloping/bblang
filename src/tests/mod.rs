#[cfg(test)]
use crate::{
    parse_and_compile_program,
    progs::{ast::Value, machine::MachineState},
    utils::{read_file, string_interner::intern},
};

#[cfg(test)]
fn run_file_assert_output(file: &str, output: Value) {
    let prog = parse_and_compile_program("internal.sl", &read_file(file));

    let mut cfg = MachineState::start(prog, intern("main"), vec![]);
    let res = cfg.run();
    if let Ok(oo) = res {
        if oo == output {
            return;
        }
    }
    assert!(false, "return value {res:?} does not match Ok({output:?})")
}

// #[test]
// fn test_fibonacci() {
//     run_file_assert_output("inputs/fib_iter.sl", Value::Int(102334155));
// }

#[test]
fn test_fibonacci_rec() {
    run_file_assert_output("inputs/fib_rec.sl", Value::Int(55));
}
