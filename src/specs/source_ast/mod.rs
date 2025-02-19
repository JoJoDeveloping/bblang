use std::collections::HashMap;

use crate::utils::string_interner::IStr;

pub struct SourceGenerics {
    pub names: Vec<IStr>,
}

impl SourceGenerics {
    pub fn arity(&self) -> usize {
        self.names.len()
    }
}

pub struct SourceInductive {
    pub generics: SourceGenerics,
    pub name: IStr,
    pub constrs: HashMap<IStr, SourceType>,
}

pub struct SourceInductives(pub Vec<SourceInductive>);

#[derive(Clone, Debug)]
pub enum SourceType {
    Inductive(IStr, Vec<Box<SourceType>>),
    BoundVar(IStr),
    Arrow(Box<SourceType>, Box<SourceType>),
}

pub struct SourcePolyType {
    pub binders: SourceGenerics,
    pub ty: Box<SourceType>,
}
pub enum SourceExpr {
    Var(IStr),
    Lambda(Option<IStr>, IStr, Option<Box<SourceType>>, Box<SourceExpr>),
    App(Box<SourceExpr>, Box<SourceExpr>),
    Let(
        IStr,
        Option<Box<SourcePolyType>>,
        Box<SourceExpr>,
        Box<SourceExpr>,
    ),
    IndConst(
        IStr,
        IStr,
        Option<Vec<Option<Box<SourceType>>>>,
        Vec<Box<SourceExpr>>,
    ),
    IndMatch(Box<SourceExpr>, Option<IStr>, HashMap<IStr, SourceMatchArm>),
}

pub struct SourceMatchArm {
    constr: IStr,
    var: Vec<IStr>,
    expr: Box<SourceExpr>,
}

pub struct SourceConstDef {
    name: IStr,
    ty: SourcePolyType,
    value: Box<SourceExpr>,
}
