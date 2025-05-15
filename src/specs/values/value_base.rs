use std::{
    fmt::Display,
    hash::{Hash, Hasher},
};

use num_bigint::BigInt;

use crate::{
    specs::{
        InfPointer,
        checked_ast::expr::ExprBox,
        values::{get_interner, get_interner_borrowed},
    },
    utils::{interner::Interned, string_interner::IStr},
};

use super::traits::ValueLike;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum ValueBase {
    Lambda {
        rec: Option<IStr>,
        arg: IStr,
        body: ExprBox,
        captures: Vec<(IStr, Interned)>,
    },
    InductiveVal {
        ty: IStr,
        constr: IStr,
        args: Vec<Interned>,
    },
    NumValue(BigInt),
    PtrValue(InfPointer),
}

impl Hash for ValueBase {
    #[inline]
    fn hash<__H: Hasher>(&self, state: &mut __H) -> () {
        match self {
            ValueBase::Lambda {
                rec,
                arg,
                body,
                captures,
            } => {
                Hash::hash(&0, state);
                Hash::hash(rec, state);
                Hash::hash(arg, state);
                Hash::hash(body, state);
                Hash::hash(captures, state)
            }
            ValueBase::InductiveVal { ty, constr, args } => {
                Hash::hash(&1, state);
                Hash::hash(ty, state);
                Hash::hash(constr, state);
                Hash::hash(args, state)
            }
            ValueBase::NumValue(n) => {
                Hash::hash(&2, state);
                Hash::hash(n, state)
            }
            ValueBase::PtrValue(p) => {
                Hash::hash(&3, state);
                Hash::hash(p, state)
            }
        }
    }
}

impl ValueLike for ValueBase {
    fn is_fo(&self) -> bool {
        match self {
            ValueBase::Lambda { .. } => false,
            ValueBase::InductiveVal { args, .. } => args
                .iter()
                .all(|x| get_interner().read().unwrap().lookup(*x).0.is_fo()),
            ValueBase::NumValue(_) => true,
            ValueBase::PtrValue(_) => true,
        }
    }

    fn into_value(&self) -> ValueBase {
        self.clone()
    }

    fn compare_by_ref(&self, other: &ValueBase) -> bool {
        self == other
    }
}

impl Display for ValueBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueBase::InductiveVal { ty, constr, args } => {
                write!(f, "{ty}::{constr}(")?;
                let mut comma = false;
                for v in args {
                    if comma {
                        write!(f, ", ")?
                    } else {
                        comma = true
                    }
                    let a = get_interner_borrowed().read().unwrap();
                    let v = &a.lookup(*v).0;
                    v.fmt(f)?;
                }
                write!(f, ")")
            }
            ValueBase::Lambda { .. } => write!(f, "fn"),
            ValueBase::NumValue(x) => Display::fmt(x, f),
            ValueBase::PtrValue(x) => Display::fmt(x, f),
        }
    }
}
