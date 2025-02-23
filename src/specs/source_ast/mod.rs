use std::collections::HashMap;

use crate::{parse::Span, utils::string_interner::IStr};

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
    pub constrs: HashMap<IStr, SourceConstructor>,
    pub pos: Span,
}

pub struct SourceConstructor {
    pub args: Vec<SourceTypeBox>,
}

pub struct SourceInductives(pub Vec<SourceInductive>);

pub type SourceTypeBox = Box<(SourceType, Span)>;
#[derive(Clone, Debug)]
pub enum SourceType {
    Inductive(IStr, Vec<SourceTypeBox>),
    BoundVar(IStr),
    Arrow(SourceTypeBox, SourceTypeBox),
}

pub struct SourcePolyType {
    pub binders: SourceGenerics,
    pub ty: SourceTypeBox,
}

pub type SourceExprBox = Box<(SourceExpr, Span)>;
pub enum SourceExpr {
    Var(IStr),
    Lambda(Option<IStr>, IStr, Option<SourceTypeBox>, SourceExprBox),
    App(SourceExprBox, SourceExprBox),
    Let(
        IStr,
        Option<Box<SourcePolyType>>,
        SourceExprBox,
        SourceExprBox,
    ),
    IndConst(
        IStr,
        IStr,
        Option<Vec<Option<SourceTypeBox>>>,
        Vec<SourceExprBox>,
    ),
    IndMatch(SourceExprBox, Option<IStr>, HashMap<IStr, SourceMatchArm>),
}

pub struct SourceMatchArm {
    pub constr: IStr,
    pub vars: Vec<IStr>,
    pub expr: SourceExprBox,
}

pub struct SourceConstDef {
    pub name: IStr,
    pub ty: SourcePolyType,
    pub value: SourceExprBox,
    pub pos: Span,
}

pub enum SourceDef {
    Inductives(SourceInductives),
    Const(SourceConstDef),
}
