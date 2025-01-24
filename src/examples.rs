use std::collections::HashMap;

use crate::{
    compile::highast::{self, Expr},
    progs::ast::*,
    utils::string_interner::intern,
};

#[allow(unused)]
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

#[allow(unused)]
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
