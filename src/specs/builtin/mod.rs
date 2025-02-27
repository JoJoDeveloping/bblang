use std::{collections::HashMap, rc::Rc};

use crate::{
    specs::exec::Value,
    utils::string_interner::{IStr, intern},
};

mod int;
mod ptr;
mod specs;

use super::{
    checked_ast::{
        expr::Expr,
        types::{Builtin, PolyType, Type},
    },
    exec::{
        ExecCtx,
        monad::{MonadArg, SpecMonad},
    },
    typecheck::w::GlobalCtx,
};

pub fn populate_tc_globals(g: &mut GlobalCtx) {
    let int = Rc::new(Type::Builtin(Builtin::Int));
    let intint = Rc::new(Type::Arrow(int.clone(), int.clone()));
    let intintint = Rc::new(Type::Arrow(int.clone(), intint.clone()));
    g.global_defs
        .insert(intern("neg"), PolyType::without_binders(intint.clone()));
    g.global_defs
        .insert(intern("add"), PolyType::without_binders(intintint.clone()));
    g.global_defs
        .insert(intern("mul"), PolyType::without_binders(intintint.clone()));
    g.global_defs.insert(
        intern("bitand"),
        PolyType::without_binders(intintint.clone()),
    );
    g.global_defs.insert(
        intern("le"),
        PolyType::without_binders(Rc::new(Type::Arrow(
            int.clone(),
            Rc::new(Type::Arrow(
                int.clone(),
                Rc::new(Type::Inductive(intern("Bool"), vec![])),
            )),
        ))),
    );

    let ptr = Rc::new(Type::Builtin(Builtin::Ptr));
    g.global_defs.insert(
        intern("Owned"),
        PolyType::without_binders(Rc::new(Type::Arrow(
            ptr.clone(),
            Rc::new(Type::Inductive(intern("Val"), vec![])),
        ))),
    );
    g.global_defs.insert(
        intern("Fail"),
        PolyType::without_binders(Rc::new(Type::Arrow(
            Rc::new(Type::Inductive(intern("Bool"), vec![])),
            Rc::new(Type::Inductive(intern("Empty"), vec![])),
        ))),
    );
    g.global_defs.insert(
        intern("ptradd"),
        PolyType::without_binders(Rc::new(Type::Arrow(
            ptr.clone(),
            Rc::new(Type::Arrow(int.clone(), ptr.clone())),
        ))),
    );
    g.global_defs.insert(
        intern("ptrdiff"),
        PolyType::without_binders(Rc::new(Type::Arrow(
            ptr.clone(),
            Rc::new(Type::Arrow(
                ptr,
                Rc::new(Type::Inductive(intern("Option"), vec![int.clone()])),
            )),
        ))),
    );
}

fn n_arg_builtin(n: usize, b: IStr) -> Rc<Value> {
    assert!(n > 0);
    let binders = (1..=n)
        .map(|x| Box::new(Expr::Var(intern(format!("x{x}")))))
        .collect();
    let mut base = Box::new(Expr::Builtin(b, binders));
    for x in (2..=n).rev() {
        base = Box::new(Expr::Lambda(None, intern(format!("x{x}")), base));
    }
    Rc::new(Value::Lambda {
        rec: None,
        arg: intern("x1"),
        body: base,
        captures: HashMap::new(),
    })
}

impl ExecCtx {
    fn insert_global(&mut self, name: IStr, global: Rc<Value>) {
        self.globals.insert(name, global);
        self.globals_order.push(name);
    }

    fn insert_n_arg_builtin(&mut self, b: IStr, n: usize) {
        self.insert_global(b, n_arg_builtin(n, b));
    }
}

pub fn populate_exec_builtins(ctx: &mut ExecCtx) {
    ctx.insert_n_arg_builtin(intern("add"), 2);
    ctx.insert_n_arg_builtin(intern("mul"), 2);
    ctx.insert_n_arg_builtin(intern("le"), 2);
    ctx.insert_n_arg_builtin(intern("neg"), 1);
    ctx.insert_n_arg_builtin(intern("bitand"), 2);
    ctx.insert_n_arg_builtin(intern("Owned"), 1);
    ctx.insert_n_arg_builtin(intern("Fail"), 1);
    ctx.insert_n_arg_builtin(intern("ptradd"), 2);
    ctx.insert_n_arg_builtin(intern("ptrdiff"), 2);
}

pub fn run_builtin<'b>(
    name: IStr,
    args: Vec<Rc<Value>>,
    monad: &mut MonadArg<'b>,
) -> SpecMonad<Rc<Value>> {
    Ok(match &args as &[_] {
        [a1, a2] if name == intern("add") => int::add(a1, a2),
        [a1, a2] if name == intern("mul") => int::mul(a1, a2),
        [a1, a2] if name == intern("le") => int::le(a1, a2),
        [a1, a2] if name == intern("bitand") => int::bitand(a1, a2),
        [a1] if name == intern("neg") => int::neg(a1),

        [a1] if name == intern("Fail") => return specs::fail(a1),
        [a1] if name == intern("Owned") => return specs::owned(a1, monad),

        [a1, a2] if name == intern("ptradd") => ptr::ptradd(a1, a2),
        [a1, a2] if name == intern("ptrdiff") => ptr::ptrdiff(a1, a2),
        _ => panic!(
            "Builtin {name} does not exist or can not be called with {} args!",
            args.len()
        ),
    })
}
