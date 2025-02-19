use std::{collections::HashMap, rc::Rc};

use crate::{specs::typecheck::w::utils::TypeVar, utils::string_interner::IStr};

#[derive(Clone, Debug)]
pub struct Generics {
    pub names: Vec<(TypeVar, Option<IStr>)>,
}

pub struct Inductive {
    pub generics: Generics,
    pub name: IStr,
    pub constrs: HashMap<IStr, Rc<Type>>,
}

pub struct Inductives(pub Vec<Inductive>);

#[derive(Clone, Debug)]
pub enum Type {
    Inductive(IStr, Vec<Rc<Type>>),
    TypeVar(TypeVar),
    Arrow(Rc<Type>, Rc<Type>),
}

#[derive(Clone, Debug)]
pub struct PolyType {
    pub binders: Generics,
    pub ty: Rc<Type>,
}
