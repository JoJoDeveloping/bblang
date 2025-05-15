// much is taken from https://github.com/nwoeanhinnogaehr/algorithmw-rust/blob/master/src/w.rs

use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use utils::{GlobalSubst, TypeLike, TypeVar, TypeVarGen};

use crate::{
    parse::Span,
    specs::{
        builtin::populate_tc_globals,
        checked_ast::{
            expr::ConstDef,
            types::{Inductive, Inductives, PolyType, Type},
        },
        source_ast::SourceConstDef,
    },
    utils::string_interner::IStr,
};

use super::error::TypeError;

pub mod utils;

impl TypeLike for Type {
    type ApplyResult<'a> = Self;
    fn fold_vars<R, F: Copy + Fn(TypeVar) -> R, RF: Fn() -> R + Copy, C: Copy + Fn(R, R) -> R>(
        &self,
        f: F,
        r: RF,
        c: C,
        globals: &GlobalSubst,
    ) -> R {
        match self {
            Type::Inductive(_, subs) => subs
                .iter()
                .map(|x| x.fold_vars(f, r, c, globals))
                .fold(r(), c),
            // Type::BoundVar(istr) => r,
            Type::TypeVar(x) => match globals.bindings.get(x) {
                Some(t) => t.fold_vars(f, r, c, globals),
                None => f(*x),
            },
            Type::Arrow(a, b) => c(a.fold_vars(f, r, c, globals), b.fold_vars(f, r, c, globals)),
            Type::Builtin(_) => r(),
        }
    }
}

impl TypeLike for PolyType {
    type ApplyResult<'a> = Self;
    fn fold_vars<R, F: Copy + Fn(TypeVar) -> R, RF: Fn() -> R + Copy, C: Copy + Fn(R, R) -> R>(
        &self,
        f: F,
        r: RF,
        c: C,
        globals: &GlobalSubst,
    ) -> R {
        let fvs: HashSet<TypeVar> = self.binders.names.iter().map(|x| x.0).collect();
        self.ty.fold_vars(
            |x| {
                if fvs.contains(&x) { r() } else { f(x) }
            },
            r,
            c,
            globals,
        )
    }
}

impl GlobalSubst {
    pub fn unify(&mut self, ty1: Rc<Type>, ty2: Rc<Type>) -> Result<(), TypeError> {
        // println!("Unifying {ty1:?} with {ty2:?}");
        match (&*ty1, &*ty2) {
            (Type::Arrow(in1, out1), Type::Arrow(in2, out2)) => {
                self.unify(in1.clone(), in2.clone())?;
                self.unify(out1.clone(), out2.clone())?;
                Ok(())
            }

            (Type::Inductive(name1, tys1), Type::Inductive(name2, tys2))
                if name1 == name2 && tys1.len() == tys2.len() =>
            {
                for (ty1, ty2) in tys1.iter().zip(tys2.iter()) {
                    self.unify(ty1.clone(), ty2.clone())?;
                }
                Ok(())
            }
            (Type::Builtin(b1), Type::Builtin(b2)) if b1 == b2 => Ok(()),

            // (Type::BoundVar(v1), Type::BoundVar(v2)) if v1 == v2 => Ok(Subst::new()),
            (Type::TypeVar(v), _) => self.bind(*v, ty2),
            (_, Type::TypeVar(v)) => self.bind(*v, ty1),

            _ => Err(TypeError::UnificationFailure(ty1, ty2)),
        }
    }

    fn bind(&mut self, tv: TypeVar, ty: Rc<Type>) -> Result<(), TypeError> {
        let ty = Rc::new(self.resolve_fully(&ty));
        // Check for binding a variable to itself
        if let Type::TypeVar(u) = &*ty {
            if *u == tv {
                return Ok(());
            }
        }

        match self.bindings.get(&tv) {
            Some(ty2) => self.unify(ty, ty2.clone()),
            None => {
                if ty.occurs(tv, self) {
                    return Err(TypeError::OccursFailure(tv, ty));
                }
                self.bindings.insert(tv, ty);
                Ok(())
            }
        }

        // The occurs check prevents illegal recursive types.
    }

    pub fn resolve_fully(&self, t: &Type) -> Type {
        match t {
            Type::Inductive(istr, items) => Type::Inductive(
                *istr,
                items
                    .iter()
                    .map(|x| self.resolve_fully(&**x))
                    .map(Rc::new)
                    .collect(),
            ),
            Type::TypeVar(tv) => match self.bindings.get(tv) {
                Some(x) => self.resolve_fully(&**x),
                None => Type::TypeVar(*tv),
            },
            Type::Arrow(t1, t2) => Type::Arrow(
                Rc::new(self.resolve_fully(&**t1)),
                Rc::new(self.resolve_fully(&**t2)),
            ),
            Type::Builtin(b) => Type::Builtin(*b),
        }
    }
}
#[derive(Debug)]
pub enum InductiveDef {
    Defined(usize, usize),
    UnderConstruction(usize),
}

#[derive(Debug)]
pub struct TypeCtx {
    pub type_defs: Vec<Inductives>,
    pub type_locations: HashMap<IStr, InductiveDef>,
}

impl TypeCtx {
    pub fn new() -> Self {
        Self {
            type_defs: Vec::new(),
            type_locations: HashMap::new(),
        }
    }
    pub fn get_inductive(&self, name: IStr) -> Option<Rc<Inductive>> {
        let InductiveDef::Defined(idx, off) = *self.type_locations.get(&name)? else {
            return None;
        };
        Some(self.type_defs[idx].0[off].clone())
    }
    pub fn get_inductive_arity(&self, name: IStr) -> Option<usize> {
        match *self.type_locations.get(&name)? {
            InductiveDef::Defined(idx, off) => {
                Some(self.type_defs[idx].0[off].generics.names.len())
            }
            InductiveDef::UnderConstruction(arity) => Some(arity),
        }
    }
}

pub struct GlobalCtx {
    pub type_defs: TypeCtx,
    pub global_defs: HashMap<IStr, PolyType>,
    pub name_generator: TypeVarGen,
    pub subst: GlobalSubst,
    pub predicates: HashSet<IStr>,
}

impl GlobalCtx {
    pub fn new() -> Self {
        let mut this = Self {
            type_defs: TypeCtx::new(),
            global_defs: HashMap::new(),
            name_generator: TypeVarGen::new(),
            subst: GlobalSubst::new(),
            predicates: HashSet::new(),
        };
        populate_tc_globals(&mut this);
        this
    }
}

#[derive(Debug)]
pub struct LocalCtx<'a> {
    parent: Option<&'a LocalCtx<'a>>,
    term_vars: HashMap<IStr, PolyType>,
    type_vars: HashMap<IStr, TypeVar>,
    filter: Option<HashSet<IStr>>,
}

impl<'a> LocalCtx<'a> {
    pub fn lookup_type(&'a self, name: IStr) -> Option<TypeVar> {
        if let Some(x) = &self.filter {
            if !x.contains(&name) {
                return None;
            }
        }
        self.type_vars
            .get(&name)
            .copied()
            .or_else(|| self.parent.and_then(|p| p.lookup_type(name)))
    }

    pub fn lookup_ident(&'a self, name: IStr) -> Option<PolyType> {
        self.term_vars
            .get(&name)
            .cloned()
            .or_else(|| self.parent.and_then(|p| p.lookup_ident(name)))
    }

    pub fn new() -> Self {
        Self {
            parent: None,
            term_vars: HashMap::new(),
            type_vars: HashMap::new(),
            filter: None,
        }
    }

    pub fn push(
        &'a self,
        term_vars: HashMap<IStr, PolyType>,
        type_vars: HashMap<IStr, TypeVar>,
    ) -> Self {
        Self {
            parent: Some(self),
            term_vars,
            type_vars,
            filter: None,
        }
    }

    pub fn push_vars(&'a self, vars: HashMap<IStr, PolyType>) -> Self {
        self.push(vars, HashMap::new())
    }

    pub fn push_type_vars(&'a self, vars: HashMap<IStr, TypeVar>) -> Self {
        self.push(HashMap::new(), vars)
    }

    pub fn push_filter(&'a self, filter: HashSet<IStr>) -> Self {
        Self {
            parent: Some(self),
            term_vars: HashMap::new(),
            type_vars: HashMap::new(),
            filter: Some(filter),
        }
    }

    pub fn check_const_defs_as_let_chain(
        &mut self,
        globals: &mut GlobalCtx,
        defs: Vec<SourceConstDef>,
    ) -> Result<Vec<ConstDef>, (TypeError, Span)> {
        let mut res = Vec::new();
        for def in defs {
            let pos = def.pos;
            let (ty, name, value) = self.check_constant(globals, def)?;
            if let Some(_) = self.term_vars.insert(name, ty.clone()) {
                return Err((TypeError::DuplicateGlobal(name), pos));
            }
            res.push(ConstDef {
                name,
                ty,
                value: value,
            });
        }
        Ok(res)
    }
}

impl<'a> TypeLike for LocalCtx<'a> {
    type ApplyResult<'b>
        = LocalCtx<'b>
    where
        Self: 'b;
    fn fold_vars<R, F: Copy + Fn(TypeVar) -> R, RF: Fn() -> R + Copy, C: Copy + Fn(R, R) -> R>(
        &self,
        f: F,
        r: RF,
        c: C,
        globals: &GlobalSubst,
    ) -> R {
        let r1 = match self.parent {
            Some(x) => x.fold_vars(f, r, c, globals),
            None => r(),
        };
        self.term_vars
            .iter()
            .map(|(_, y)| y.fold_vars(f, r, c, globals))
            .fold(r1, c)
    }
}
