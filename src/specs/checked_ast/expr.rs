use std::{
    collections::HashMap, fmt::Debug, hash::Hash, ops::Deref, rc::Rc, sync::atomic::AtomicUsize,
};

use num_bigint::BigInt;

use crate::utils::string_interner::IStr;

use super::types::{PolyType, Type};

#[derive(Clone, Debug)]
pub enum Expr<TY = (), REC = ExprBox> {
    Var(IStr),
    Lambda(Option<IStr>, IStr, REC),
    App(REC, REC),
    Let(IStr, Box<PolyType<TY>>, REC, REC),
    IndConst(IStr, IStr, Vec<TY>, Vec<REC>),
    IndMatch(REC, IStr, TY, HashMap<IStr, MatchArm<REC>>),
    NumConst(BigInt),
    Builtin(IStr, Vec<REC>),
    PredicateBox(IStr, Vec<IStr>, REC),
}

pub struct SemiCheckedExpr(Expr<Rc<Type>, Box<SemiCheckedExpr>>);

#[derive(Clone, Debug)]
pub struct MatchArm<REC = ExprBox> {
    pub constr: IStr,
    pub vars: Vec<IStr>,
    pub expr: REC,
}

#[derive(Debug)]
pub struct ConstDef<REC = ExprBox> {
    pub name: IStr,
    pub ty: PolyType,
    pub value: REC,
}

#[derive(Clone)]
pub struct ExprBox(Box<Expr<(), ExprBox>>, usize);

static FRESH: AtomicUsize = AtomicUsize::new(0);

impl SemiCheckedExpr {
    pub fn new(e: Expr<Rc<Type>, Box<SemiCheckedExpr>>) -> Self {
        Self(e)
    }

    pub fn into(self) -> Expr<Rc<Type>, Box<SemiCheckedExpr>> {
        self.0
    }
}

impl ExprBox {
    pub fn new(e: Expr) -> ExprBox {
        Self(
            Box::new(e),
            FRESH.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        )
    }

    pub fn into(self) -> Expr {
        *self.0
    }
}

impl Deref for ExprBox {
    type Target = Expr;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}
impl PartialEq for ExprBox {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl Eq for ExprBox {}
impl Hash for ExprBox {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.1.hash(state);
    }
}

impl Debug for ExprBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}
