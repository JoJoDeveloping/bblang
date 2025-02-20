use std::{collections::HashMap, rc::Rc};

use crate::utils::string_interner::IStr;

use super::types::{PolyType, Type};

pub enum Expr {
    Var(IStr),
    Lambda(Option<IStr>, IStr, Rc<Type>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(IStr, Box<PolyType>, Box<Expr>, Box<Expr>),
    IndConst(IStr, IStr, Vec<Rc<Type>>, Vec<Box<Expr>>),
    IndMatch(Box<Expr>, IStr, HashMap<IStr, MatchArm>),
}

struct MatchArm {
    constr: IStr,
    var: Vec<IStr>,
    expr: Box<Expr>,
}

struct ConstDef {
    name: IStr,
    ty: PolyType,
    value: Box<Expr>,
}
