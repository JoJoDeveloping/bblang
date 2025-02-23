use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    rc::Rc,
};

use crate::utils::string_interner::IStr;

use super::{
    checked_ast::expr::{ConstDef, Expr},
    typecheck::w::{TypeCtx, utils::GlobalSubst},
};

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
}
#[derive(Debug)]
pub struct ExecCtx {
    pub types: TypeCtx,
    pub globals: HashMap<IStr, Rc<Value>>,
    pub globals_order: Vec<IStr>,
}
impl ExecCtx {
    pub fn new(types: TypeCtx) -> Self {
        Self {
            types,
            globals: HashMap::new(),
            globals_order: Vec::new(),
        }
    }

    pub fn consume_global(&mut self, global: ConstDef) {
        let localctx = ExecLocalCtx::new();
        let res = localctx.exec(&self, &global.value);
        self.globals.insert(global.name, res);
        self.globals_order.push(global.name);
    }

    pub fn consume_globals(&mut self, res: Vec<ConstDef>) {
        res.into_iter().for_each(|g| self.consume_global(g));
    }
}

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

    pub fn exec(&self, globals: &ExecCtx, expr: &Expr) -> Rc<Value> {
        match expr {
            Expr::Var(istr) => match self.lookup(*istr) {
                Some(k) => k,
                None => globals.globals.get(istr).unwrap().clone(),
            },
            Expr::Lambda(rec, arg, _, body) => Rc::new(Value::Lambda {
                rec: *rec,
                arg: *arg,
                body: body.clone(),
                captures: self.capture_environment(body),
            }),
            Expr::App(fun, arge) => {
                let fun = self.exec(globals, fun);
                let Value::Lambda {
                    rec,
                    arg,
                    body,
                    mut captures,
                } = (*fun).clone()
                else {
                    panic!("apply to non-function!")
                };
                let argv = self.exec(globals, arge);
                captures.insert(arg, argv);
                if let Some(rec) = rec {
                    captures.insert(rec, fun);
                }
                let subscope = self.push(captures);
                subscope.exec(globals, &body)
            }
            Expr::Let(var, _, bound, expr) => {
                let bound = self.exec(globals, bound);
                let mut subscope = HashMap::new();
                subscope.insert(*var, bound);
                self.push(subscope).exec(globals, expr)
            }
            Expr::IndConst(ty, constr, _, args) => Rc::new(Value::InductiveVal {
                ty: *ty,
                constr: *constr,
                args: args.iter().map(|e| self.exec(globals, e)).collect(),
            }),
            Expr::IndMatch(discrimnee, _, arms) => {
                let Value::InductiveVal {
                    ty: _,
                    constr,
                    args,
                } = &*self.exec(globals, &discrimnee)
                else {
                    panic!()
                };
                let arm = arms.get(constr).unwrap();
                assert_eq!(args.len(), arm.vars.len());
                let substi = arm.vars.iter().copied().zip(args.iter().cloned()).collect();
                self.push(substi).exec(globals, &arm.expr)
            }
        }
    }

    fn capture_environment(&self, expr: &Expr) -> HashMap<IStr, Rc<Value>> {
        expr.free_vars()
            .into_iter()
            .filter_map(|x| self.lookup(x).map(|y| (x, y)))
            .collect()
    }
}

impl Expr {
    fn free_vars(&self) -> HashSet<IStr> {
        match self {
            Expr::Var(istr) => [*istr].iter().copied().collect(),
            Expr::Lambda(rec, arg, _, expr) => {
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
                let mut res = bound.free_vars();
                res.remove(var);
                res
            }
            Expr::IndConst(_, _, _, exprs) => {
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
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::InductiveVal { ty, constr, args } => {
                write!(f, "{ty}::{constr}(")?;
                let mut comma = false;
                for v in args {
                    v.fmt(f)?;
                    if comma {
                        write!(f, ", ")?
                    } else {
                        comma = true
                    }
                }
                write!(f, ")")
            }
            Value::Lambda {
                rec: _,
                arg: _,
                body: _,
                captures: _,
            } => write!(f, "fn"),
        }
    }
}
