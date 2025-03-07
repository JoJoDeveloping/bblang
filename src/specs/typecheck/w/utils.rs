use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
};

use crate::specs::checked_ast::types::{Generics, PolyType, Type};

pub trait TypeLike {
    type ApplyResult<'a>
    where
        Self: 'a;
    /// Find the set of free variables in a type.
    fn fold_vars<R, F: Copy + Fn(TypeVar) -> R, RF: Fn() -> R + Copy, C: Copy + Fn(R, R) -> R>(
        &self,
        f: F,
        r: RF,
        c: C,
        globals: &GlobalSubst,
    ) -> R;

    fn occurs(&self, tv: TypeVar, globals: &GlobalSubst) -> bool {
        self.fold_vars(|x| x == tv, || false, |x, y| x || y, globals)
    }

    fn fvs(&self, globals: &GlobalSubst) -> HashSet<TypeVar> {
        self.fold_vars(
            |x| [x].into(),
            || HashSet::new(),
            |mut a, b| {
                b.into_iter().for_each(|y| {
                    a.insert(y);
                });
                a
            },
            globals,
        )
    }
}

impl<T: TypeLike> TypeLike for Box<T> {
    type ApplyResult<'a>
        = Box<T::ApplyResult<'a>>
    where
        Self: 'a;

    fn fold_vars<R, F: Copy + Fn(TypeVar) -> R, RF: Fn() -> R + Copy, C: Copy + Fn(R, R) -> R>(
        &self,
        f: F,
        r: RF,
        c: C,
        globals: &GlobalSubst,
    ) -> R {
        (**self).fold_vars(f, r, c, globals)
    }
}

pub struct GlobalSubst {
    pub(super) bindings: HashMap<TypeVar, Rc<Type>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(usize);

pub struct TypeVarGen {
    supply: usize,
}

impl TypeVarGen {
    pub fn new() -> TypeVarGen {
        TypeVarGen { supply: 0 }
    }
    pub fn next(&mut self) -> TypeVar {
        let v = TypeVar(self.supply);
        self.supply += 1;
        v
    }
}

impl GlobalSubst {
    /// Construct an empty substitution.
    pub fn new() -> GlobalSubst {
        GlobalSubst {
            bindings: HashMap::new(),
        }
    }
}

impl PolyType {
    pub fn without_binders(ty: Rc<Type>) -> PolyType {
        Self {
            binders: Generics { names: vec![] },
            ty,
        }
    }
}

impl TypeVar {
    pub fn number(self) -> usize {
        self.0
    }
}
