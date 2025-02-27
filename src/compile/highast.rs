use std::collections::HashMap;

use crate::{
    parse::Span,
    progs::ast::{BinOp, Value},
    specs::source_ast::{FunctionSpec, SourceDef},
    utils::string_interner::IStr,
};

pub type BoxExpr = Box<(Expr, Span)>;

#[derive(Debug)]
pub enum Expr {
    Const(Value),
    ReadLocal(IStr),
    SetLocal(IStr, BoxExpr),
    Negate(BoxExpr),
    BitNot(BoxExpr),
    Load(BoxExpr),
    Alloc(BoxExpr),
    Free(BoxExpr),
    Store(BoxExpr, BoxExpr),
    BinOp(BoxExpr, BinOp, BoxExpr),
    Call(IStr, Vec<BoxExpr>),
    If(BoxExpr, BoxExpr, BoxExpr),
    While(BoxExpr, BoxExpr),
    Seq(BoxExpr, BoxExpr),
    Return(BoxExpr),
}

#[derive(Debug)]
pub struct Function {
    pub name: IStr,
    pub args: Vec<IStr>,
    pub locals: Vec<IStr>,
    pub code: BoxExpr,
    pub span: Span,
    pub spec: Option<FunctionSpec>,
}

#[derive(Debug)]
pub struct Program {
    pub functions: HashMap<IStr, Function>,
    pub spec_stuff: Vec<SourceDef>,
}
