use std::collections::{BTreeMap, HashMap};

use crate::{parse::Span, utils::string_interner::IStr};

#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash, Debug)]
pub struct Pointer(pub u32, pub u32);

#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash, Debug)]
pub enum Value {
    Int(i32),
    Loc(Pointer),
    NullPointer,
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash, Debug)]
pub enum Operand {
    Const(Value),
    Local(u32),
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash, Debug)]
pub enum UnOp {
    Move,
    Negate,
    BitwiseNot,
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Eq,
    Lt,
    Le,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

#[derive(Clone, Debug)]
pub enum Statement {
    UnOp(u32, UnOp, Operand),
    BinOp(u32, BinOp, Operand, Operand),
    Load(u32, Operand),
    Store(Operand, Operand),
    FunCall(Option<u32>, IStr, Vec<Operand>),
    Alloc(u32, Operand),
    Free(Operand),
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Jump(usize),
    CondJump(Operand, usize, usize),
    Return(Operand),
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub insns: Vec<Statement>,
    pub term: Terminator,
}

#[derive(Debug)]
pub struct Function {
    pub name: IStr,
    pub blocks: Vec<BasicBlock>,
    pub debug_info: DebugInfo,
}

#[derive(Debug)]
pub struct Program {
    pub funcs: HashMap<IStr, Function>,
}

#[derive(Debug, Default)]
pub struct DebugInfo {
    pub data: HashMap<(usize, usize), Span>,
    pub local_names: BTreeMap<u32, IStr>,
}
