use std::{collections::HashSet, rc::Rc};

use crate::{
    parse::Span,
    specs::checked_ast::types::{Generics, Type},
    utils::string_interner::IStr,
};

use super::w::utils::TypeVar;

#[derive(Debug)]
pub enum TypeError {
    InvalidInductive(IStr),
    UndefinedInductive(IStr),
    IllegalInductiveInstantiation(IStr, usize),
    UndefinedType(IStr),
    DuplicateInductive(IStr),
    DuplicateConstructor(IStr, IStr),
    DuplicateTypeVariableInGenerics(IStr),
    OccursFailure(TypeVar, Rc<Type>),
    UnificationFailure(Rc<Type>, Rc<Type>),
    UndefinedVar(IStr),
    IllegalGenericsDefinition(Generics, Rc<Type>),
    UndefinedConstr(IStr),
    IllegalGenericsInstantiation(Generics, usize),
    IllegalConstructorApplication(IStr, IStr, usize),
    MatchNotOnInductive(Rc<Type>),
    MatchError(),
    DuplicateGlobal(IStr),
}

pub type Result<T> = core::result::Result<T, (TypeError, Span)>;
