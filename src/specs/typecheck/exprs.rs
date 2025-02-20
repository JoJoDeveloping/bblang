use std::{collections::HashMap, rc::Rc};

use crate::specs::{
    checked_ast::{
        expr::Expr,
        types::{Generics, PolyType, Type},
    },
    source_ast::SourceExpr,
    typecheck::w::utils::TypeVar,
};

use super::{
    error::{Result, TypeError},
    w::{GlobalCtx, LocalCtx, utils::TypeLike},
};

impl<'a> LocalCtx<'a> {
    pub fn check_expr(
        &'a self,
        globals: &mut GlobalCtx,
        expr: SourceExpr,
    ) -> Result<(Rc<Type>, Expr)> {
        Ok(match expr {
            SourceExpr::Var(istr) => {
                if let Some(x) = self.lookup_ident(istr) {
                    (
                        x.instantiate(&globals.subst, &mut globals.name_generator),
                        Expr::Var(istr),
                    )
                } else if let Some(x) = globals.global_defs.get(&istr) {
                    (
                        x.instantiate(&globals.subst, &mut globals.name_generator),
                        Expr::Var(istr),
                    )
                } else {
                    return Err(TypeError::UndefinedVar(istr));
                }
            }
            SourceExpr::Lambda(rec, arg, arg_sty, body) => {
                let arg_ty = match arg_sty {
                    Some(x) => self.check_type(globals, *x)?,
                    None => Rc::new(Type::TypeVar(globals.name_generator.next())),
                };
                let res_ty = Rc::new(Type::TypeVar(globals.name_generator.next()));
                let ty = Rc::new(Type::Arrow(arg_ty.clone(), res_ty.clone()));
                let mut newscope = HashMap::new();
                newscope.insert(arg, PolyType::without_binders(arg_ty.clone()));
                if let Some(rec) = rec {
                    newscope.insert(rec, PolyType::without_binders(ty.clone()));
                }
                let newscope = self.push_vars(newscope);
                let (res_ty2, body) = newscope.check_expr(globals, *body)?;
                globals.subst.unify(res_ty.clone(), res_ty2)?;
                (ty, Expr::Lambda(rec, arg, arg_ty, Box::new(body)))
            }
            SourceExpr::App(efn, earg) => {
                let (t1, efn) = self.check_expr(globals, *efn)?;
                let (t2, earg) = self.check_expr(globals, *earg)?;
                let tr = Rc::new(Type::TypeVar(globals.name_generator.next()));
                globals
                    .subst
                    .unify(t1, Rc::new(Type::Arrow(t2, tr.clone())))?;
                (tr, Expr::App(Box::new(efn), Box::new(earg)))
            }
            SourceExpr::Let(istr, ty, bound, rest) => {
                let f = |gen2: Option<Generics>,
                         lctx: &LocalCtx<'_>,
                         should_ty: Rc<Type>,
                         globals: &mut GlobalCtx|
                 -> Result<(Rc<Type>, Expr)> {
                    let (ty, expr_bound) = lctx.check_expr(globals, *bound)?;
                    globals.subst.unify(ty.clone(), should_ty)?;
                    let fv_ctx = lctx.fvs(&globals.subst);
                    let mut fv_ty = ty.fvs(&globals.subst);
                    for fv in fv_ctx {
                        fv_ty.remove(&fv);
                    }
                    let binders = match gen2 {
                        Some(gen2)
                            if fv_ty.len() != gen2.names.len()
                                || gen2.names.iter().any(|(x, _)| !fv_ty.contains(x)) =>
                        {
                            return Err(TypeError::IllegalGenericsDefinition(gen2, fv_ty));
                        }
                        Some(gen2) => gen2,
                        None => Generics {
                            names: fv_ty.into_iter().map(|x| (x, None)).collect(),
                        },
                    };
                    let polyty = PolyType { binders, ty };
                    let lctx = self.push_vars([(istr, polyty.clone())].into_iter().collect());
                    let (ty3, rest) = lctx.check_expr(globals, *rest)?;
                    Ok((
                        ty3,
                        Expr::Let(istr, Box::new(polyty), Box::new(expr_bound), Box::new(rest)),
                    ))
                };
                return match ty {
                    Some(st) => {
                        let (gen2, lctx) = self.enter_generics(globals, st.binders)?;
                        let should_ty = lctx.check_type(globals, *st.ty)?;
                        f(Some(gen2), &lctx, should_ty, globals)
                    }
                    None => f(
                        None,
                        self,
                        Rc::new(Type::TypeVar(globals.name_generator.next())),
                        globals,
                    ),
                };
            }
            SourceExpr::IndConst(indname, ctrname, spec, args) => {
                let ind = globals
                    .get_inductive(indname)
                    .ok_or_else(|| (TypeError::UndefinedInductive(indname)))?;
                let ctr = ind
                    .constrs
                    .get(&ctrname)
                    .ok_or_else(|| (TypeError::UndefinedConstr(ctrname)))?
                    .clone();
                let ctr: Vec<_> = ctr
                    .args
                    .iter()
                    .map(|x| globals.subst.resolve_fully(x))
                    .collect();
                let data: Vec<_> = ind.generics.names.iter().map(|(x, _)| *x).collect();
                let res_ty = ind.uninstantiated_type();
                let (data, instanti) = match spec {
                    None => {
                        let mut instanti = Vec::new();
                        let mut substi = HashMap::new();
                        for x in data {
                            let ty = Rc::new(Type::TypeVar(globals.name_generator.next()));
                            instanti.push(ty.clone());
                            substi.insert(x, ty);
                        }
                        (substi, instanti)
                    }
                    Some(vec) => {
                        if vec.len() != data.len() {
                            return Err(TypeError::IllegalGenericsInstantiation(
                                ind.generics.clone(),
                                vec.len(),
                            ));
                        }
                        let mut substi = HashMap::new();
                        let mut instanti = Vec::new();
                        for (x, styo) in data.into_iter().zip(vec.into_iter()) {
                            let ty = match styo {
                                Some(ty) => self.check_type(globals, *ty)?,
                                None => Rc::new(Type::TypeVar(globals.name_generator.next())),
                            };
                            instanti.push(ty.clone());
                            substi.insert(x, ty);
                        }
                        (substi, instanti)
                    }
                };
                let ctr: Vec<_> = ctr.into_iter().map(|x| x.subst(&data)).collect();
                let res_ty = res_ty.subst(&data);
                if args.len() != ctr.len() {
                    return Err(TypeError::IllegalConstructorApplication(
                        indname,
                        ctrname,
                        args.len(),
                    ));
                }
                let mut new_args = Vec::new();
                for (should_ty, expr) in ctr.into_iter().zip(args.into_iter()) {
                    let (is_ty, expr) = self.check_expr(globals, *expr)?;
                    globals.subst.unify(should_ty, is_ty)?;
                    new_args.push(Box::new(expr));
                }
                (res_ty, Expr::IndConst(indname, ctrname, instanti, new_args))
            }
            SourceExpr::IndMatch(source_expr, istr, _hash_map) => todo!(),
        })
    }
}
