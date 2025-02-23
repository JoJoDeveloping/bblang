use crate::{
    specs::{
        checked_ast::types::Inductives,
        exec::{ExecCtx, Value},
        source_ast::*,
        typecheck::w::GlobalCtx,
    },
    utils::string_interner::intern,
};

use super::error::TypeError;

fn mknate(i: i32) -> Box<SourceExpr> {
    Box::new(match i {
        0 => SourceExpr::IndConst(intern("Nat"), intern("Zero"), None, vec![]),
        i => SourceExpr::IndConst(intern("Nat"), intern("Succ"), None, vec![mknate(i - 1)]),
    })
}

fn unmknat(v: &Value) -> i64 {
    match &*v {
        Value::InductiveVal { ty, constr, args }
            if *ty == intern("Nat") && *constr == intern("Zero") && args.len() == 0 =>
        {
            0
        }
        Value::InductiveVal { ty, constr, args }
            if *ty == intern("Nat") && *constr == intern("Succ") && args.len() == 1 =>
        {
            1 + unmknat(&args[0])
        }
        _ => unreachable!(),
    }
}

pub fn example() -> Result<(), TypeError> {
    let inductives = SourceInductives(vec![SourceInductive {
        generics: SourceGenerics { names: vec![] },
        name: intern("Nat"),
        constrs: [
            (intern("Zero"), SourceConstructor { args: vec![] }),
            (
                intern("Succ"),
                SourceConstructor {
                    args: vec![Box::new(SourceType::Inductive(intern("Nat"), vec![]))],
                },
            ),
        ]
        .into_iter()
        .collect(),
    }]);
    let nat_ty = Box::new(SourceType::Inductive(intern("Nat"), vec![]));
    let add = SourceExpr::Lambda(
        Some(intern("add")),
        intern("n"),
        None,
        Box::new(SourceExpr::Lambda(
            None,
            intern("m"),
            None,
            Box::new(SourceExpr::IndMatch(
                Box::new(SourceExpr::Var(intern("n"))),
                Some(intern("Nat")),
                [
                    (
                        intern("Zero"),
                        SourceMatchArm {
                            constr: intern("Zero"),
                            vars: vec![],
                            expr: Box::new(SourceExpr::Var(intern("m"))),
                        },
                    ),
                    (
                        intern("Succ"),
                        SourceMatchArm {
                            constr: intern("Succ"),
                            vars: vec![intern("n'")],
                            expr: Box::new(SourceExpr::IndConst(
                                intern("Nat"),
                                intern("Succ"),
                                None,
                                vec![Box::new(SourceExpr::App(
                                    Box::new(SourceExpr::App(
                                        Box::new(SourceExpr::Var(intern("add"))),
                                        Box::new(SourceExpr::Var(intern("n'"))),
                                    )),
                                    Box::new(SourceExpr::Var(intern("m"))),
                                ))],
                            )),
                        },
                    ),
                ]
                .into_iter()
                .collect(),
            )),
        )),
    );
    let add = SourceConstDef {
        name: intern("add"),
        ty: SourcePolyType {
            binders: SourceGenerics { names: vec![] },
            ty: Box::new(SourceType::Arrow(
                Box::clone(&nat_ty),
                Box::new(SourceType::Arrow(Box::clone(&nat_ty), Box::clone(&nat_ty))),
            )),
        },
        value: Box::new(add),
    };
    let mul = SourceExpr::Lambda(
        Some(intern("mul")),
        intern("n"),
        None,
        Box::new(SourceExpr::Lambda(
            None,
            intern("m"),
            None,
            Box::new(SourceExpr::IndMatch(
                Box::new(SourceExpr::Var(intern("n"))),
                Some(intern("Nat")),
                [
                    (
                        intern("Zero"),
                        SourceMatchArm {
                            constr: intern("Zero"),
                            vars: vec![],
                            expr: Box::new(SourceExpr::IndConst(
                                intern("Nat"),
                                intern("Zero"),
                                None,
                                vec![],
                            )),
                        },
                    ),
                    (
                        intern("Succ"),
                        SourceMatchArm {
                            constr: intern("Succ"),
                            vars: vec![intern("n'")],
                            expr: Box::new(SourceExpr::App(
                                Box::new(SourceExpr::App(
                                    Box::new(SourceExpr::Var(intern("add"))),
                                    Box::new(SourceExpr::Var(intern("m"))),
                                )),
                                Box::new(SourceExpr::App(
                                    Box::new(SourceExpr::App(
                                        Box::new(SourceExpr::Var(intern("mul"))),
                                        Box::new(SourceExpr::Var(intern("n'"))),
                                    )),
                                    Box::new(SourceExpr::Var(intern("m"))),
                                )),
                            )),
                        },
                    ),
                ]
                .into_iter()
                .collect(),
            )),
        )),
    );
    let mul = SourceConstDef {
        name: intern("mul"),
        ty: SourcePolyType {
            binders: SourceGenerics { names: vec![] },
            ty: Box::new(SourceType::Arrow(
                Box::clone(&nat_ty),
                Box::new(SourceType::Arrow(Box::clone(&nat_ty), Box::clone(&nat_ty))),
            )),
        },
        value: Box::new(mul),
    };
    let adduse = SourceConstDef {
        name: intern("mynum"),
        ty: SourcePolyType {
            binders: SourceGenerics { names: vec![] },
            ty: Box::clone(&nat_ty),
        },
        value: Box::new(SourceExpr::App(
            Box::new(SourceExpr::App(
                Box::new(SourceExpr::Var(intern("mul"))),
                mknate(6),
            )),
            mknate(7),
        )),
    };
    let prog = vec![
        SourceDef::Inductives(inductives),
        SourceDef::Const(add),
        SourceDef::Const(mul),
        SourceDef::Const(adduse),
    ];
    let mut globals = GlobalCtx::new();
    let res = globals.check_defs(prog)?;
    let tys = globals.type_defs;
    println!("{tys:?}");
    let mut exctx = ExecCtx::new(tys);
    exctx.consume_globals(res);
    println!("{exctx:?}");
    println!("{}", unmknat(exctx.globals.get(&intern("mynum")).unwrap()));
    Ok(())
}
