use std::{collections::HashMap, vec};

use crate::{
    compile::highast::{self, BoxExpr, Expr},
    progs::ast::*,
    specs::typecheck::w::GlobalCtx,
    utils::string_interner::intern,
};

#[allow(unused)]
fn fib_rec_prog() -> Program {
    let mut funcs = HashMap::new();
    funcs.insert(
        intern("fib"),
        Function {
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
            debug_info: Default::default(),
            spec: None,
        },
    );
    Program {
        funcs,
        spec_stuff: (GlobalCtx::new(), vec![]),
    }
}

fn b(e: Expr) -> BoxExpr {
    Box::new((e, (Default::default(), Default::default())))
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
            code: b(Expr::If(
                b(Expr::BinOp(
                    b(Expr::ReadLocal(intern("n"))),
                    BinOp::Lt,
                    b(Expr::Const(Value::Int(2))),
                )),
                b(Expr::ReadLocal(intern("n"))),
                b(Expr::BinOp(
                    b(Expr::Call(
                        intern("fib"),
                        vec![b(Expr::BinOp(
                            b(Expr::ReadLocal(intern("n"))),
                            BinOp::Sub,
                            b(Expr::Const(Value::Int(1))),
                        ))],
                    )),
                    BinOp::Add,
                    b(Expr::Call(
                        intern("fib"),
                        vec![b(Expr::BinOp(
                            b(Expr::ReadLocal(intern("n"))),
                            BinOp::Sub,
                            b(Expr::Const(Value::Int(2))),
                        ))],
                    )),
                )),
            )),
            span: Default::default(),
            spec: None,
        }
        .compile(&mut GlobalCtx::new()),
    );
    Program {
        funcs,
        spec_stuff: (GlobalCtx::new(), vec![]),
    }
}
