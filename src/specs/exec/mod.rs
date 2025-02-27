use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    rc::Rc,
};

use monad::{MonadArg, SpecMonad};
use num_bigint::BigInt;

use crate::utils::string_interner::IStr;

use super::{
    builtin::{populate_exec_builtins, run_builtin},
    checked_ast::expr::{ConstDef, Expr},
};

pub mod monad;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InfPointer(pub u32, pub BigInt);

#[derive(Clone, Debug)]
pub enum Value {
    InductiveVal {
        ty: IStr,
        constr: IStr,
        args: Vec<Rc<Value>>,
    },
    Lambda {
        rec: Option<IStr>,
        arg: IStr,
        body: Box<Expr>,
        captures: HashMap<IStr, Rc<Value>>,
    },
    NumValue(BigInt),
    PtrValue(InfPointer),
}
#[derive(Debug)]
pub struct ExecCtx {
    pub globals: HashMap<IStr, Rc<Value>>,
    pub globals_order: Vec<IStr>,
}

impl ExecCtx {
    pub fn new() -> Self {
        let mut this = Self {
            globals: HashMap::new(),
            globals_order: Vec::new(),
        };
        populate_exec_builtins(&mut this);
        this
    }

    pub fn consume_global<'b>(
        &mut self,
        monad: &mut MonadArg<'b>,
        global: ConstDef,
    ) -> SpecMonad<()> {
        let localctx = ExecLocalCtx::new();
        let res = localctx.exec(&self, monad, &global.value)?;
        self.globals.insert(global.name, res);
        self.globals_order.push(global.name);
        Ok(())
    }

    pub fn consume_globals<'b>(
        &mut self,
        monad: &mut MonadArg<'b>,
        res: Vec<ConstDef>,
    ) -> SpecMonad<()> {
        res.into_iter()
            .try_for_each(|g| self.consume_global(monad, g))
    }
}

#[derive(Debug)]
pub struct ExecLocalCtx<'a> {
    pub parent: Option<&'a ExecLocalCtx<'a>>,
    pub bindings: HashMap<IStr, Rc<Value>>,
}

impl<'a> ExecLocalCtx<'a> {
    fn lookup(&self, name: IStr) -> Option<Rc<Value>> {
        self.bindings
            .get(&name)
            .cloned()
            .or_else(|| self.parent.and_then(|p| p.lookup(name)))
    }

    fn push(&'a self, bindings: HashMap<IStr, Rc<Value>>) -> Self {
        Self {
            parent: Some(self),
            bindings,
        }
    }

    pub fn new() -> Self {
        Self {
            parent: None,
            bindings: HashMap::new(),
        }
    }

    pub fn exec<'b>(
        &self,
        globals: &ExecCtx,
        monad: &mut MonadArg<'b>,
        expr: &Expr,
    ) -> SpecMonad<Rc<Value>> {
        Ok(match expr {
            Expr::Var(istr) => match self.lookup(*istr) {
                Some(k) => k,
                None => globals.globals.get(istr).unwrap().clone(),
            },
            Expr::Lambda(rec, arg, body) => Rc::new(Value::Lambda {
                rec: *rec,
                arg: *arg,
                body: body.clone(),
                captures: self.capture_environment(body),
            }),
            Expr::App(fun, arge) => {
                let fun = self.exec(globals, monad, fun)?;
                let Value::Lambda {
                    rec,
                    arg,
                    body,
                    mut captures,
                } = (*fun).clone()
                else {
                    panic!("apply to non-function!")
                };
                let argv = self.exec(globals, monad, arge)?;
                captures.insert(arg, argv.clone());
                if let Some(rec) = rec {
                    // println!("Evaluating recursive call to {rec} with arg {argv}");
                    captures.insert(rec, fun);
                }
                let subscope = self.push(captures);
                subscope.exec(globals, monad, &body)?
            }
            Expr::Let(var, _, bound, expr) => {
                let bound = self.exec(globals, monad, bound)?;
                let mut subscope = HashMap::new();
                subscope.insert(*var, bound);
                self.push(subscope).exec(globals, monad, expr)?
            }
            Expr::IndConst(ty, constr, _, args) => Rc::new(Value::InductiveVal {
                ty: *ty,
                constr: *constr,
                args: args
                    .iter()
                    .map(|e| self.exec(globals, monad, e))
                    .collect::<SpecMonad<Vec<_>>>()?,
            }),
            Expr::IndMatch(discrimnee, _, arms) => {
                let Value::InductiveVal {
                    ty: _,
                    constr,
                    args,
                } = &*self.exec(globals, monad, &discrimnee)?
                else {
                    panic!()
                };
                let arm = arms.get(constr).unwrap();
                assert_eq!(args.len(), arm.vars.len());
                let substi = arm.vars.iter().copied().zip(args.iter().cloned()).collect();
                self.push(substi).exec(globals, monad, &arm.expr)?
            }
            Expr::NumConst(x) => Rc::new(Value::NumValue(x.clone())),
            Expr::Builtin(name, args) => {
                let args: Vec<_> = args
                    .iter()
                    .map(|x: &Box<Expr>| self.exec(globals, monad, x))
                    .collect::<SpecMonad<Vec<_>>>()?;
                return run_builtin(*name, args, monad);
            }
        })
    }

    fn capture_environment(&self, expr: &Expr) -> HashMap<IStr, Rc<Value>> {
        expr.free_vars()
            .into_iter()
            .filter_map(|x| self.lookup(x).map(|y| (x, y)))
            .collect()
    }

    pub fn push_arg(&mut self, name: IStr, arg: Rc<Value>) {
        self.bindings.insert(name, arg);
    }

    pub fn execute_const_defs_as_lets(
        &mut self,
        globals: &ExecCtx,
        monad: &mut MonadArg,
        defs: &Vec<ConstDef>,
    ) -> SpecMonad<()> {
        for def in defs {
            let v = self.exec(globals, monad, &def.value)?;
            self.push_arg(def.name, v);
        }
        Ok(())
    }
}

impl Expr {
    fn free_vars(&self) -> HashSet<IStr> {
        match self {
            Expr::Var(istr) => [*istr].iter().copied().collect(),
            Expr::Lambda(rec, arg, expr) => {
                let mut res = expr.free_vars();
                res.remove(arg);
                if let Some(rec) = rec {
                    res.remove(rec);
                }
                res
            }
            Expr::App(expr, expr1) => {
                let mut res = expr.free_vars();
                expr1.free_vars().into_iter().for_each(|x| {
                    res.insert(x);
                });
                res
            }
            Expr::Let(var, _, bound, rest) => {
                let mut res = rest.free_vars();
                res.remove(var);
                bound.free_vars().into_iter().for_each(|x| {
                    res.insert(x);
                });
                res
            }
            Expr::IndConst(_, _, _, exprs) | Expr::Builtin(_, exprs) => {
                let mut res = HashSet::new();
                for e in exprs {
                    e.free_vars().into_iter().for_each(|x| {
                        res.insert(x);
                    });
                }
                res
            }
            Expr::IndMatch(expr, _, arms) => {
                let mut res = expr.free_vars();
                for arm in arms {
                    let mut sub = arm.1.expr.free_vars();
                    for b in &arm.1.vars {
                        sub.remove(&b);
                    }
                    sub.into_iter().for_each(|x| {
                        res.insert(x);
                    });
                }
                res
            }
            Expr::NumConst(_) => HashSet::new(),
        }
    }
}

impl Display for InfPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::InductiveVal { ty, constr, args } => {
                write!(f, "{ty}::{constr}(")?;
                let mut comma = false;
                for v in args {
                    if comma {
                        write!(f, ", ")?
                    } else {
                        comma = true
                    }
                    v.fmt(f)?;
                }
                write!(f, ")")
            }
            Value::Lambda {
                rec: _,
                arg: _,
                body: _,
                captures: _,
            } => write!(f, "fn"),
            Value::NumValue(x) => Display::fmt(x, f),
            Value::PtrValue(x) => Display::fmt(x, f),
        }
    }
}
