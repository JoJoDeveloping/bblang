use std::{collections::HashMap, rc::Rc};

use crate::utils::string_interner::IStr;

use super::types::{PolyType, Type};

#[derive(Clone, Debug)]
pub enum Expr {
    Var(IStr),
    Lambda(Option<IStr>, IStr, Rc<Type>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(IStr, Box<PolyType>, Box<Expr>, Box<Expr>),
    IndConst(IStr, IStr, Vec<Rc<Type>>, Vec<Box<Expr>>),
    IndMatch(Box<Expr>, IStr, HashMap<IStr, MatchArm>),
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub constr: IStr,
    pub vars: Vec<IStr>,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct ConstDef {
    pub name: IStr,
    pub ty: PolyType,
    pub value: Box<Expr>,
}
