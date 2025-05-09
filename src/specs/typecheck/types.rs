use std::{collections::HashMap, rc::Rc};

use crate::{
    parse::Span,
    specs::{
        checked_ast::types::{
            Builtin, Constructor, Generics, Inductive, Inductives, PolyType, Type,
        },
        source_ast::{
            SourceConstructor, SourceGenerics, SourceInductives, SourceType, SourceTypeBox,
        },
        typecheck::w::InductiveDef,
    },
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
            if let Some(_) = self.type_defs.type_locations.insert(
                idx.name,
                InductiveDef::UnderConstruction(idx.generics.arity()),
            ) {
                return Err((TypeError::DuplicateInductive(idx.name), idx.pos));
            }
        }
        // println!("foo bar {:?}", self.type_defs);
        let mut res = Inductives(Vec::new());
        let es = LocalCtx::new();
        {
            for inductive in def.0 {
                let (generics, scope) =
                    es.enter_generics(inductive.pos, self, inductive.generics)?;
                let mut constrs = HashMap::new();
                for (cn, ctr) in inductive.constrs {
                    if constrs
                        .insert(cn, scope.check_constructor(self, ctr)?)
                        .is_some()
                    {
                        return Err((
                            TypeError::DuplicateConstructor(inductive.name, cn),
                            inductive.pos,
                        ));
                    }
                }
                res.0.push(Rc::new(Inductive {
                    generics,
                    name: inductive.name,
                    constrs,
                }));
            }
        }
        let idx = self.type_defs.type_defs.len();
        for (off, inductive) in res.0.iter().enumerate() {
            self.type_defs
                .type_locations
                .insert(inductive.name, InductiveDef::Defined(idx, off));
        }
        self.type_defs.type_defs.push(res);
        Ok(())
    }
}

impl<'a> LocalCtx<'a> {
    fn check_constructor(
        &'a self,
        globals: &GlobalCtx,
        ctr: SourceConstructor,
    ) -> Result<Constructor> {
        let mut args = Vec::new();
        for arg in ctr.args {
            args.push(self.check_type(globals, arg)?);
        }
        Ok(Constructor { args })
    }

    pub fn check_type(&'a self, globals: &GlobalCtx, inty: SourceTypeBox) -> Result<Rc<Type>> {
        Ok(Rc::new(match inty.0 {
            SourceType::Inductive(istr, args) => {
                let ind = globals
                    .type_defs
                    .get_inductive_arity(istr)
                    .ok_or_else(|| (TypeError::UndefinedInductive(istr), inty.1))?;
                if ind != args.len() {
                    // panic!("Illegal {istr} {ind} {:?}", args);
                    return Err((
                        TypeError::IllegalInductiveInstantiation(istr, args.len()),
                        inty.1,
                    ));
                }
                let x: Result<Vec<Rc<_>>> = args
                    .into_iter()
                    .map(|ty| self.check_type(globals, ty))
                    .collect();
                Type::Inductive(istr, x?)
            }
            SourceType::BoundVar(istr) => {
                if let Some(x) = self.lookup_type(istr) {
                    Type::TypeVar(x)
                } else {
                    let ind = globals
                        .type_defs
                        .get_inductive_arity(istr)
                        .ok_or_else(|| (TypeError::UndefinedType(istr), inty.1))?;
                    if ind != 0 {
                        // panic!("Illegal {ind} to 0");
                        return Err((TypeError::IllegalInductiveInstantiation(istr, 0), inty.1));
                    }
                    Type::Inductive(istr, vec![])
                }
            }
            SourceType::Arrow(dom, cod) => Type::Arrow(
                self.check_type(globals, dom)?,
                self.check_type(globals, cod)?,
            ),
            SourceType::BuiltinInt => Type::Builtin(Builtin::Int),
            SourceType::BuiltinPtr => Type::Builtin(Builtin::Ptr),
        }))
    }

    pub fn enter_generics(
        &'a self,
        pos: Span,
        globals: &mut GlobalCtx,
        generics: SourceGenerics,
    ) -> Result<(Generics, LocalCtx<'a>)> {
        let mut map = HashMap::new();
        let mut names = Vec::new();
        for var in generics.names {
            let tv = globals.name_generator.next();
            if map.insert(var, tv).is_some() {
                return Err((TypeError::DuplicateTypeVariableInGenerics(var), pos));
            }
            names.push((tv, Some(var)));
        }
        Ok((Generics { names }, self.push_type_vars(map)))
    }
}

impl PolyType {
    pub fn instantiate(&self, globals: &GlobalSubst, tvg: &mut TypeVarGen) -> Rc<Type> {
        globals.resolve_fully(&*self.ty).subst(
            &self
                .binders
                .names
                .iter()
                .map(|(b, _)| (*b, Rc::new(Type::TypeVar(tvg.next()))))
                .collect(),
        )
    }
}

impl Type {
    pub fn subst(&self, apply: &HashMap<TypeVar, Rc<Type>>) -> Rc<Type> {
        match self {
            Type::Inductive(istr, items) => Rc::new(Type::Inductive(
                *istr,
                items.iter().map(|x| x.subst(apply)).collect(),
            )),
            Type::TypeVar(tv) => match apply.get(tv) {
                Some(x) => x.clone(),
                None => Rc::new(Type::TypeVar(*tv)),
            },
            Type::Arrow(t1, t2) => Rc::new(Type::Arrow(t1.subst(apply), t2.subst(apply))),
            Type::Builtin(x) => Rc::new(Type::Builtin(*x)),
        }
    }
}
