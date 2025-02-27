use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
    fmt::Display,
};

use crate::{
    parse::Span,
    specs::{
        checked_ast::expr::ConstDef,
        exec::ExecCtx,
        source_ast::{FunctionSpec, SourceConstDef, SourceDef},
        typecheck::w::GlobalCtx,
    },
    utils::string_interner::IStr,
};

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
    pub spec: Option<CompiledFunctionSpec>,
}

pub struct CompiledFunctionSpec {
    pub arg_names: Vec<IStr>,
    pub pre: Vec<ConstDef>,
    pub post: Vec<ConstDef>,
}

impl Debug for CompiledFunctionSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CompiledFunctionSpec").finish()
    }
}

// #[derive(Debug)]
pub struct Program {
    pub funcs: HashMap<IStr, Function>,
    pub spec_stuff: (GlobalCtx, Vec<ConstDef>),
}

#[derive(Debug, Default)]
pub struct DebugInfo {
    pub data: HashMap<(usize, usize), Span>,
    pub local_names: BTreeMap<u32, IStr>,
}

impl Display for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}
