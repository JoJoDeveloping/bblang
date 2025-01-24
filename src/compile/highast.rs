use std::collections::HashMap;

use crate::{
    progs::ast::{BinOp, Value},
    utils::string_interner::IStr,
};

#[derive(Debug)]
pub enum Expr {
    Const(Value),
    ReadLocal(IStr),
    SetLocal(IStr, Box<Expr>),
    Negate(Box<Expr>),
    BitNot(Box<Expr>),
    Load(Box<Expr>),
    Alloc(Box<Expr>),
    Free(Box<Expr>),
    Store(Box<Expr>, Box<Expr>),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    Call(IStr, Vec<Box<Expr>>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
    Return(Box<Expr>),
}

#[derive(Debug)]
pub struct Function {
    pub name: IStr,
    pub args: Vec<IStr>,
    pub locals: Vec<IStr>,
    pub code: Box<Expr>,
}

#[derive(Debug)]
pub struct Program {
    pub functions: HashMap<IStr, Function>,
}
