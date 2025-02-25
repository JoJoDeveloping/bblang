use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    parse::Span,
    specs::{
        checked_ast::{
            expr::{ConstDef, Expr, MatchArm},
            types::{Builtin, Generics, PolyType, Type},
        },
        source_ast::{SourceConstDef, SourceExpr, SourceExprBox},
        typecheck::w::utils::TypeVar,
    },
};

use super::{
    error::{Result, TypeError},
    w::{GlobalCtx, LocalCtx, utils::TypeLike},
};

impl<'a> LocalCtx<'a> {
    pub fn check_let(
        &'a self,
        pos: Span,
        gen2: Option<Generics>,
        should_ty: Rc<Type>,
        globals: &mut GlobalCtx,
        bound: SourceExprBox,
    ) -> Result<(PolyType, Expr)> {
        let lctx = self;
        let (ty, expr_bound) = lctx.check_expr(globals, bound)?;
        globals
            .subst
            .unify(ty.clone(), should_ty)
            .map_err(|x| (x, pos))?;
        let fv_ctx = lctx.fvs(&globals.subst);
        let mut fv_ty = ty.fvs(&globals.subst);
        for fv in fv_ctx {
            fv_ty.remove(&fv);
        }
        let frt = Rc::new(globals.subst.resolve_fully(&ty));
        let binders = match gen2 {
            Some(gen2) => {
                let mut gen2_ftvs = Vec::new();
                for (tv, istr) in gen2.names.iter().copied() {
                    //TODO these error messages suck
                    let Type::TypeVar(tv) = globals.subst.resolve_fully(&Type::TypeVar(tv)) else {
                        return Err((TypeError::IllegalGenericsDefinition(gen2, frt), pos));
                    };
                    gen2_ftvs.push((tv, istr));
                    if !fv_ty.remove(&tv) {
                        return Err((TypeError::IllegalGenericsDefinition(gen2, frt), pos));
                    }
                }
                if !fv_ty.is_empty() {
                    return Err((TypeError::IllegalGenericsDefinition(gen2, frt), pos));
                }
                Generics { names: gen2_ftvs }
            }
            None => Generics {
                names: fv_ty.into_iter().map(|x| (x, None)).collect(),
            },
        };
        let polyty = PolyType { binders, ty };
        Ok((polyty, expr_bound))
    }

    pub fn check_expr(
        &'a self,
        globals: &mut GlobalCtx,
        inexpr: SourceExprBox,
    ) -> Result<(Rc<Type>, Expr)> {
        Ok(match inexpr.0 {
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
                    return Err((TypeError::UndefinedVar(istr), inexpr.1));
                }
            }
            SourceExpr::Lambda(rec, arg, arg_sty, body) => {
                let arg_ty = match arg_sty {
                    Some(x) => self.check_type(globals, x)?,
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
                let (res_ty2, body) = newscope.check_expr(globals, body)?;
                globals
                    .subst
                    .unify(res_ty.clone(), res_ty2)
                    .map_err(|x| (x, inexpr.1))?;
                (ty, Expr::Lambda(rec, arg, Box::new(body)))
            }
            SourceExpr::App(efn, earg) => {
                let (t1, efn) = self.check_expr(globals, efn)?;
                let (t2, earg) = self.check_expr(globals, earg)?;
                let tr = Rc::new(Type::TypeVar(globals.name_generator.next()));
                globals
                    .subst
                    .unify(t1, Rc::new(Type::Arrow(t2, tr.clone())))
                    .map_err(|x| (x, inexpr.1))?;
                (tr, Expr::App(Box::new(efn), Box::new(earg)))
            }
            SourceExpr::Let(istr, ty, bound, rest) => {
                let f = |gen2: Option<Generics>,
                         lctx: &LocalCtx<'_>,
                         should_ty: Rc<Type>,
                         globals: &mut GlobalCtx|
                 -> Result<(Rc<Type>, Expr)> {
                    let (polyty, expr_bound) =
                        lctx.check_let(inexpr.1, gen2, should_ty, globals, bound)?;
                    let lctx = self.push_vars([(istr, polyty.clone())].into_iter().collect());
                    let (ty3, rest) = lctx.check_expr(globals, rest)?;
                    Ok((
                        ty3,
                        Expr::Let(istr, Box::new(polyty), Box::new(expr_bound), Box::new(rest)),
                    ))
                };
                return match ty {
                    Some(st) => {
                        let (gen2, lctx) = self.enter_generics(inexpr.1, globals, st.binders)?;
                        let should_ty = lctx.check_type(globals, st.ty)?;
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
                    .type_defs
                    .get_inductive(indname)
                    .ok_or_else(|| (TypeError::UndefinedInductive(indname), inexpr.1))?;
                let ctr = ind
                    .constrs
                    .get(&ctrname)
                    .ok_or_else(|| (TypeError::UndefinedConstr(ctrname), inexpr.1))?;
                let ctr: Vec<_> = ctr
                    .args
                    .iter()
                    .map(|x| globals.subst.resolve_fully(x))
                    .collect();
                let res_ty = ind.uninstantiated_type();
                let (data, instanti) = match spec {
                    None => {
                        let mut instanti = Vec::new();
                        let mut substi = HashMap::new();
                        for x in ind.generics.names.iter().map(|(x, _)| *x) {
                            let ty = Rc::new(Type::TypeVar(globals.name_generator.next()));
                            instanti.push(ty.clone());
                            substi.insert(x, ty);
                        }
                        (substi, instanti)
                    }
                    Some(vec) => {
                        if vec.len() != ind.generics.names.len() {
                            return Err((
                                TypeError::IllegalGenericsInstantiation(
                                    ind.generics.clone(),
                                    vec.len(),
                                ),
                                inexpr.1,
                            ));
                        }
                        let mut substi = HashMap::new();
                        let mut instanti = Vec::new();
                        for (x, styo) in ind
                            .generics
                            .names
                            .iter()
                            .map(|(x, _)| *x)
                            .zip(vec.into_iter())
                        {
                            let ty = match styo {
                                Some(ty) => self.check_type(globals, ty)?,
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
                    return Err((
                        TypeError::IllegalConstructorApplication(indname, ctrname, args.len()),
                        inexpr.1,
                    ));
                }
                let mut new_args = Vec::new();
                for (should_ty, expr) in ctr.into_iter().zip(args.into_iter()) {
                    let (is_ty, expr) = self.check_expr(globals, expr)?;
                    globals
                        .subst
                        .unify(should_ty, is_ty)
                        .map_err(|x| (x, inexpr.1))?;
                    new_args.push(Box::new(expr));
                }
                (res_ty, Expr::IndConst(indname, ctrname, instanti, new_args))
            }
            SourceExpr::IndMatch(discriminee, inductive_ty, arms) => {
                let res_ty = Rc::new(Type::TypeVar(globals.name_generator.next()));
                let (match_ty, discriminee) = self.check_expr(globals, discriminee)?;
                let (ind, substi) = match inductive_ty {
                    Some(indname) => {
                        let ind = globals
                            .type_defs
                            .get_inductive(indname)
                            .ok_or_else(|| (TypeError::UndefinedInductive(indname), inexpr.1))?;
                        let res_ty = ind.uninstantiated_type();
                        let mut substi = HashMap::new();
                        for x in ind
                            .generics
                            .names
                            .iter()
                            .map(|(x, _)| *x)
                            .collect::<Vec<_>>()
                        {
                            let ty = Rc::new(Type::TypeVar(globals.name_generator.next()));
                            substi.insert(x, ty);
                        }
                        let res_ty = res_ty.subst(&substi);
                        globals
                            .subst
                            .unify(match_ty, res_ty)
                            .map_err(|x| (x, inexpr.1))?;
                        (ind, substi)
                    }
                    None => {
                        let match_ty = globals.subst.resolve_fully(&match_ty);
                        if let Type::Inductive(indname, presubsti) = match_ty {
                            let ind =
                                globals.type_defs.get_inductive(indname).ok_or_else(|| {
                                    (TypeError::UndefinedInductive(indname), inexpr.1)
                                })?;
                            let substi = ind
                                .generics
                                .names
                                .iter()
                                .map(|(x, _)| *x)
                                .zip(presubsti.into_iter())
                                .collect::<HashMap<_, _>>();

                            (ind, substi)
                        } else {
                            return Err((
                                TypeError::MatchNotOnInductive(Rc::new(match_ty)),
                                inexpr.1,
                            ));
                        }
                    }
                };
                if arms.len() != ind.constrs.len() {
                    return Err((TypeError::MatchError(), inexpr.1));
                }
                let mut new_arms = HashMap::new();
                for (constr, arm) in arms {
                    assert_eq!(arm.constr, constr);
                    let constr = ind
                        .constrs
                        .get(&constr)
                        .ok_or_else(|| (TypeError::MatchError(), inexpr.1))?;
                    if constr.args.len() != arm.vars.len() {
                        return Err((TypeError::MatchError(), inexpr.1));
                    }
                    let mut envsubst = HashMap::new();
                    for (var, arg) in arm.vars.iter().zip(constr.args.iter()) {
                        let argty = globals.subst.resolve_fully(&**arg).subst(&substi);
                        if let Some(_) = envsubst.insert(*var, PolyType::without_binders(argty)) {
                            return Err((TypeError::MatchError(), inexpr.1));
                        }
                    }
                    let scope = self.push_vars(envsubst);
                    let (resty2, expr) = scope.check_expr(globals, arm.expr)?;
                    globals
                        .subst
                        .unify(resty2, res_ty.clone())
                        .map_err(|x| (x, inexpr.1))?;
                    new_arms.insert(
                        arm.constr,
                        MatchArm {
                            constr: arm.constr,
                            vars: arm.vars,
                            expr: Box::new(expr),
                        },
                    );
                }
                (
                    res_ty,
                    Expr::IndMatch(Box::new(discriminee), ind.name, new_arms),
                )
            }
            SourceExpr::NumLiteral(x) => (Rc::new(Type::Builtin(Builtin::Int)), Expr::NumConst(x)),
        })
    }
}

impl GlobalCtx {
    pub fn resolve_expr_fully(&self, expr: &mut Expr, allowed_free_vars: &HashSet<TypeVar>) {
        match expr {
            Expr::Var(_) => (),
            Expr::Lambda(_, _, body) => {
                self.resolve_expr_fully(body, allowed_free_vars);
            }
            Expr::App(expr1, expr2) => {
                self.resolve_expr_fully(expr1, allowed_free_vars);
                self.resolve_expr_fully(expr2, allowed_free_vars);
            }
            Expr::Let(_, ty, expr1, expr2) => {
                let mut new_fvs = allowed_free_vars.clone();
                ty.binders.names.iter().for_each(|(x, _)| {
                    new_fvs.insert(*x);
                });
                self.resolve_ty_fully(&mut ty.ty, &new_fvs);
                self.resolve_expr_fully(expr1, &new_fvs);
                self.resolve_expr_fully(expr2, allowed_free_vars);
            }
            Expr::IndConst(_, _, tys, exprs) => {
                exprs
                    .iter_mut()
                    .for_each(|e| self.resolve_expr_fully(e, allowed_free_vars));
                tys.iter_mut()
                    .for_each(|t| self.resolve_ty_fully(t, allowed_free_vars));
            }
            Expr::IndMatch(_, _, rows) => {
                rows.iter_mut().for_each(|(_, arm)| {
                    self.resolve_expr_fully(&mut *arm.expr, allowed_free_vars)
                });
            }
            Expr::NumConst(_) => {}
            Expr::Builtin(_, exprs) => {
                exprs
                    .iter_mut()
                    .for_each(|e| self.resolve_expr_fully(e, allowed_free_vars));
            }
        }
    }

    pub fn resolve_ty_fully(&self, ty: &mut Rc<Type>, allowed_free_vars: &HashSet<TypeVar>) {
        for ele in (*ty).fvs(&self.subst) {
            if !allowed_free_vars.contains(&ele) {
                eprintln!("Type variable {ele:?} is free/unresolved!");
            }
        }
        *ty = Rc::new(self.subst.resolve_fully(&*ty))
    }

    pub fn check_constant(&mut self, cnst: SourceConstDef) -> Result<ConstDef> {
        let lctx = LocalCtx::new();
        let st = cnst.ty;
        let (gen2, lctx) = lctx.enter_generics(cnst.pos, self, st.binders)?;
        let should_ty = lctx.check_type(self, st.ty)?;
        let (mut ty, mut value) =
            lctx.check_let(cnst.pos, Some(gen2), should_ty, self, cnst.value)?;
        let polytybinders = ty.binders.names.iter().map(|x| x.0).collect();
        self.resolve_expr_fully(&mut value, &polytybinders);
        self.resolve_ty_fully(&mut ty.ty, &polytybinders);
        if let Some(_) = self.global_defs.insert(cnst.name, ty.clone()) {
            return Err((TypeError::DuplicateGlobal(cnst.name), cnst.pos));
        }
        Ok(ConstDef {
            name: cnst.name,
            ty,
            value: Box::new(value),
        })
    }
}
