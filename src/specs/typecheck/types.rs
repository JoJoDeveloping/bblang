use std::{collections::HashMap, rc::Rc};

use crate::specs::{
    checked_ast::types::{Generics, Inductive, Inductives, PolyType, Type},
    source_ast::{SourceGenerics, SourceInductive, SourceInductives, SourceType},
    typecheck::w::InductiveDef,
};

use super::{
    error::{Result, TypeError},
    w::{
        GlobalCtx, LocalCtx,
        utils::{GlobalSubst, TypeVar, TypeVarGen},
    },
};

impl GlobalCtx {
    pub fn check_inductives(&mut self, def: SourceInductives) -> Result<()> {
        for idx in &def.0 {
            if let Some(ov) = self.type_locations.insert(
                idx.name,
                InductiveDef::UnderConstruction(idx.generics.arity()),
            ) {
                return Err(TypeError::DuplicateInductive(idx.name));
            }
        }
        let mut res = Inductives(Vec::new());
        let es = LocalCtx::new();
        {
            for inductive in def.0 {
                let (generics, scope) = es.enter_generics(self, inductive.generics)?;
                let mut constrs = HashMap::new();
                for (cn, ty) in inductive.constrs {
                    if constrs.insert(cn, scope.check_type(self, ty)?).is_some() {
                        return Err(TypeError::DuplicateConstructor(inductive.name, cn));
                    }
                }
                res.0.push(Inductive {
                    generics,
                    name: inductive.name,
                    constrs,
                });
            }
        }
        let idx = self.type_defs.len();
        for (off, inductive) in res.0.iter().enumerate() {
            self.type_locations
                .insert(inductive.name, InductiveDef::Defined(idx, off));
        }
        self.type_defs.push(res);
        Ok(())
    }
}

impl<'a> LocalCtx<'a> {
    pub fn check_type(&'a self, globals: &GlobalCtx, ty: SourceType) -> Result<Rc<Type>> {
        Ok(Rc::new(match ty {
            SourceType::Inductive(istr, args) => {
                let ind = globals
                    .get_inductive_arity(istr)
                    .ok_or_else(|| TypeError::UndefinedInductive(istr))?;
                if ind != args.len() {
                    return Err(TypeError::IllegalInductiveInstantiation(istr, args.len()));
                }
                let x: Result<Vec<Rc<_>>> = args
                    .into_iter()
                    .map(|ty| self.check_type(globals, *ty))
                    .collect();
                Type::Inductive(istr, x?)
            }
            SourceType::BoundVar(istr) => {
                if let Some(x) = self.lookup_type(istr) {
                    Type::TypeVar(x)
                } else {
                    let ind = globals
                        .get_inductive_arity(istr)
                        .ok_or_else(|| TypeError::UndefinedType(istr))?;
                    if ind != 0 {
                        return Err(TypeError::IllegalInductiveInstantiation(istr, 0));
                    }
                    Type::Inductive(istr, vec![])
                }
            }
            SourceType::Arrow(dom, cod) => Type::Arrow(
                (self.check_type(globals, *dom)?),
                (self.check_type(globals, *cod)?),
            ),
        }))
    }

    pub fn enter_generics(
        &'a self,
        globals: &mut GlobalCtx,
        generics: SourceGenerics,
    ) -> Result<(Generics, LocalCtx<'a>)> {
        let mut map = HashMap::new();
        let mut names = Vec::new();
        for var in generics.names {
            let tv = globals.name_generator.next();
            if map.insert(var, tv).is_some() {
                return Err(TypeError::DuplicateTypeVariableInGenerics(var));
            }
            names.push((tv, Some(var)));
        }
        Ok((Generics { names }, self.push_type_vars(map)))
    }
}

impl PolyType {
    pub fn instantiate(&self, globals: &GlobalSubst, r#gen: &mut TypeVarGen) -> Rc<Type> {
        globals.resolve_fully(&*self.ty).subst(
            &self
                .binders
                .names
                .iter()
                .map(|(b, _)| (*b, Type::TypeVar(r#gen.next())))
                .collect(),
        )
    }
}

impl Type {
    fn subst(&self, apply: &HashMap<TypeVar, Type>) -> Rc<Type> {
        match self {
            Type::Inductive(istr, items) => Rc::new(Type::Inductive(
                *istr,
                items.iter().map(|x| x.subst(apply)).collect(),
            )),
            Type::TypeVar(tv) => match apply.get(tv) {
                Some(x) => Rc::new(x.clone()),
                None => Rc::new(Type::TypeVar(*tv)),
            },
            Type::Arrow(t1, t2) => Rc::new(Type::Arrow(t1.subst(apply), t2.subst(apply))),
        }
    }
}
