use std::{collections::HashMap, fmt::Debug};

use num_bigint::BigInt;

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
    BuiltinInt,
    BuiltinPtr,
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
    NumLiteral(BigInt),
    PredicateBox(IStr, Vec<IStr>, SourceExprBox),
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

pub struct FunctionSpec {
    pub arg_names: Vec<IStr>,
    pub pre: Vec<SourceConstDef>,
    pub post: Vec<SourceConstDef>,
}

impl Debug for FunctionSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionSpec").finish()
    }
}

impl Debug for SourceDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceDef").finish()
    }
}
