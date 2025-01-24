#![feature(hash_raw_entry)]
#![feature(mixed_integer_ops_unsigned_sub)]
#![feature(unsigned_signed_diff)]

use std::collections::HashMap;

use compile::highast::{self, Expr};
use parse::parse_program;
use progs::{
    ast::{BasicBlock, BinOp, Function, Operand, Program, Statement, Terminator, Value},
    machine::MachineState,
};
use utils::string_interner::intern;
pub mod compile;
pub mod parse;
pub mod progs;
pub mod utils;

pub fn parse_and_compile_program(input: &str) -> Program {
    let pp = parse_program(input);
    // println!("Parsed: {pp:#?}");
    pp.compile()
}

fn fib_rec_prog() -> Program {
    let mut funcs = HashMap::new();
    funcs.insert(intern("fib"), Function {
        name: intern("fib"),
        blocks: vec![
            BasicBlock {
                insns: vec![Statement::BinOp(
                    1,
                    BinOp::Le,
                    Operand::Local(0),
                    Operand::Const(Value::Int(1)),
                )],
                term: Terminator::CondJump(Operand::Local(1), 1, 2),
            },
            BasicBlock {
                insns: vec![],
                term: Terminator::Return(Operand::Local(0)),
            },
            BasicBlock {
                insns: vec![
                    Statement::BinOp(
                        0,
                        BinOp::Sub,
                        Operand::Local(0),
                        Operand::Const(Value::Int(1)),
                    ),
                    Statement::FunCall(Some(1), intern("fib"), vec![Operand::Local(0)]),
                    Statement::BinOp(
                        0,
                        BinOp::Sub,
                        Operand::Local(0),
                        Operand::Const(Value::Int(1)),
                    ),
                    Statement::FunCall(Some(2), intern("fib"), vec![Operand::Local(0)]),
                    Statement::BinOp(0, BinOp::Add, Operand::Local(1), Operand::Local(2)),
                ],
                term: Terminator::Return(Operand::Local(0)),
            },
        ],
    });
    Program { funcs }
}

fn fib_rec_prog_compiled() -> Program {
    let mut funcs = HashMap::new();
    funcs.insert(
        intern("fib"),
        highast::Function {
            name: intern("fib"),
            args: vec![intern("n")],
            locals: Vec::new(),
            code: Box::new(Expr::If(
                Box::new(Expr::BinOp(
                    Box::new(Expr::ReadLocal(intern("n"))),
                    BinOp::Lt,
                    Box::new(Expr::Const(Value::Int(2))),
                )),
                Box::new(Expr::ReadLocal(intern("n"))),
                Box::new(Expr::BinOp(
                    Box::new(Expr::Call(intern("fib"), vec![Box::new(Expr::BinOp(
                        Box::new(Expr::ReadLocal(intern("n"))),
                        BinOp::Sub,
                        Box::new(Expr::Const(Value::Int(1))),
                    ))])),
                    BinOp::Add,
                    Box::new(Expr::Call(intern("fib"), vec![Box::new(Expr::BinOp(
                        Box::new(Expr::ReadLocal(intern("n"))),
                        BinOp::Sub,
                        Box::new(Expr::Const(Value::Int(2))),
                    ))])),
                )),
            )),
        }
        .compile(),
    );
    Program { funcs }
}

fn fib_rec_prog_compiled_parsed() -> Program {
    parse_and_compile_program(
        r#"
    fun fib(n) = if n < 2 then n else fib(n-1) + fib(n-2)
    fun main() = fib(10)
    "#,
    )
}

fn fib_iter() -> Program {
    parse_and_compile_program(
        r#"
    fun fib(n) locals i, j, t =
        set i = 0;
        set j = 1;
        while 0 < n do
            set t = i+j;
            set i = j;
            set j = t;
            set n = n - 1
        done;
        i
    fun main() = fib(40)
    "#,
    )
}

fn helloworld_prog() -> Program {
    parse_and_compile_program(
        r#"
    fun main() = putchar(72); putchar(101); putchar(108); putchar(108); putchar(111); putchar(44); putchar(32); putchar(87); putchar(111); putchar(114); putchar(108); putchar(100); putchar(33); putchar(10)
    "#,
    )
}

fn list_sort() -> Program {
    parse_and_compile_program(
        r#"
        fun nil() = nullptr
        fun cons(hd, tl) locals ptr =
            set ptr = alloc 2;
            ptr <- hd;
            ptr + 1 <- tl;
            ptr
        fun empty(lst) = lst == nullptr
        fun head(lst) = *lst
        fun tail(lst) = *tailptr(lst)
        fun tailptr(lst) = lst + 1
        fun app(lst1, lst2) locals tmp =
            if empty(lst1) then lst2 else
            set tmp = lst1;
            while !empty(tail(lst1)) do
                set lst1 = tail(lst1)
            done;
            tailptr(lst1) <- lst2;
            tmp
        fun qsort(lst) locals pivot, left, right, rest, tmp =
            if empty(lst) then lst else
            set pivot = head(lst); set rest = tail(lst);
            if empty(rest) then lst else
            set left = nil(); set right = nil();
            free lst;
            while !empty(rest) do
                (if head(rest) < pivot
                then set left = cons(head(rest), left)
                else set right = cons(head(rest), right));
                set tmp = rest;
                set rest = tail(rest);
                free tmp
            done;
            set left = qsort(left);
            set right = qsort(right);
            set tmp = cons(pivot, right);
            set tmp = app(left, tmp);
            tmp
        fun printall(lst) locals tmp =
            while !empty(lst) do
                putchar(head(lst));
                set tmp = lst;
                set lst = tail(lst);
                free tmp
            done;
            putchar(10)
        fun helloworldlist() = cons(72, cons(101, cons(108, cons(108, cons(111, cons(44, cons(32, cons(87, cons(111, cons(114, cons(108, cons(100, cons(33, nil())))))))))))))
        fun main() = printall(helloworldlist()); printall(qsort(helloworldlist()))
    "#,
    )
}

fn main() {
    let mut cfg = MachineState::start(list_sort(), intern("main"), vec![]);
    // println!("{:#?}", cfg);
    let res = cfg.run();
    println!("{:?}", res);
    println!("{:#?}", cfg);
}
