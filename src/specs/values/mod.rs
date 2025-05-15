use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    rc::Rc,
    sync::{MappedRwLockReadGuard, RwLockReadGuard},
};

use interning::{
    ValueExtraData, ValueExtraDataBase, get_interner, get_interner_borrowed, intern_value,
};
use num_bigint::BigInt;
use traits::ValueLike;
use value_base::ValueBase;

use crate::{
    specs::{InfPointer, exec::pointer_origin::PointerOrigin},
    utils::{interner::Interned, string_interner::IStr},
};

use super::checked_ast::expr::ExprBox;

mod interning;
pub mod pred_arg;
mod traits;
mod value_base;

#[derive(Clone)]
pub struct Value(Interned, ValueExtraData);

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&get_interner().read().unwrap().lookup(self.0).0, f)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let i = get_interner_borrowed().read().unwrap();
        match &i.lookup(self.0).0 {
            ValueBase::Lambda {
                rec,
                arg,
                body,
                captures,
            } => f
                .debug_struct("Lambda")
                .field("rec", &rec)
                .field("arg", &arg)
                .field("body", body)
                .field(
                    "captures",
                    &match (&self.1).as_deref() {
                        Some(ValueExtraDataBase::Lambda { captures: c2 }) => captures
                            .iter()
                            .zip(c2.iter())
                            .map(|x| (x.0.0, Value(x.0.1, x.1.clone())))
                            .collect::<HashMap<_, _>>(),
                        None => captures
                            .iter()
                            .map(|x| (x.0, Value(x.1, None)))
                            .collect::<HashMap<_, _>>(),
                        _ => unreachable!(),
                    },
                )
                .finish(),
            ValueBase::InductiveVal { ty, constr, args } => f
                .debug_struct("InductiveVal")
                .field("ty", &ty)
                .field("constr", &constr)
                .field(
                    "args",
                    &match (&self.1).as_deref() {
                        Some(ValueExtraDataBase::InductiveVal { args: a2 }) => args
                            .iter()
                            .zip(a2.iter())
                            .map(|x| (Value(*x.0, x.1.clone())))
                            .collect::<Vec<_>>(),
                        None => args.iter().map(|x| (Value(*x, None))).collect::<Vec<_>>(),
                        _ => unreachable!(),
                    },
                )
                .finish(),
            ValueBase::NumValue(big_int) => f.debug_tuple("NumValue").field(big_int).finish(),
            ValueBase::PtrValue(inf_pointer) => match (&self.1).as_deref() {
                Some(ValueExtraDataBase::PtrValue(pointer_origin)) => f
                    .debug_tuple("PtrValue")
                    .field(inf_pointer)
                    .field(pointer_origin)
                    .finish(),
                _ => unreachable!(),
            },
        }
    }

    /*
    MatchableValue::Lambda {
        rec,
        arg,
        body,
        captures,
    } => f
        .debug_struct("Lambda")
        .field("rec", &rec)
        .field("arg", &arg)
        .field("body", body)
        .field("captures", &captures.collect::<HashMap<_, _>>())
        .finish(),
    MatchableValue::InductiveVal { ty, constr, args } => f
        .debug_struct("InductiveVal")
        .field("ty", &ty)
        .field("constr", &constr)
        .field("args", &args.collect::<Vec<_>>())
        .finish(),
    MatchableValue::NumValue(big_int) => f.debug_tuple("NumValue").field(big_int).finish(),
    MatchableValue::PtrValue(inf_pointer, pointer_origin) => f
        .debug_tuple("PtrValue")
        .field(inf_pointer)
        .field(pointer_origin)
        .finish(),
    _ => unreachable!(), */
}

pub enum MatchableValue<'a> {
    Lambda {
        rec: Option<IStr>,
        arg: IStr,
        body: &'a ExprBox,
        captures: Box<dyn Iterator<Item = (IStr, Value)> + 'a>,
    },
    InductiveVal {
        ty: IStr,
        constr: IStr,
        args: Box<dyn Iterator<Item = Value> + 'a>,
    },
    NumValue(&'a BigInt),
    PtrValue(&'a InfPointer, PointerOrigin),
}

impl Value {
    pub fn num_value(num: &BigInt) -> Self {
        struct BigIntNumInternable<'a>(&'a BigInt);

        impl<'a> Hash for BigIntNumInternable<'a> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                Hash::hash(&2, state);
                Hash::hash(self.0, state)
            }
        }

        impl<'a> ValueLike for BigIntNumInternable<'a> {
            fn is_fo(&self) -> bool {
                true
            }

            fn into_value(&self) -> ValueBase {
                ValueBase::NumValue(self.0.clone())
            }

            fn compare_by_ref(&self, other: &ValueBase) -> bool {
                match other {
                    ValueBase::NumValue(other) => self.0 == other,
                    _ => false,
                }
            }
        }

        let interned = intern_value(BigIntNumInternable(num));
        Value(interned, None)
    }

    pub fn ptr_value(ptr: &InfPointer, prov: PointerOrigin) -> Self {
        let extra = Rc::new(ValueExtraDataBase::PtrValue(prov));

        struct InfPtrPtrInternable<'a>(&'a InfPointer);

        impl<'a> Hash for InfPtrPtrInternable<'a> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                Hash::hash(&3, state);
                Hash::hash(self.0, state)
            }
        }

        impl<'a> ValueLike for InfPtrPtrInternable<'a> {
            fn is_fo(&self) -> bool {
                true
            }

            fn into_value(&self) -> ValueBase {
                ValueBase::PtrValue(self.0.clone())
            }

            fn compare_by_ref(&self, other: &ValueBase) -> bool {
                match other {
                    ValueBase::PtrValue(other) => self.0 == other,
                    _ => false,
                }
            }
        }

        let interned = intern_value(InfPtrPtrInternable(ptr));
        Value(interned, Some(extra))
    }

    pub fn closure(
        rec: Option<IStr>,
        arg: IStr,
        body: ExprBox,
        captures: HashMap<IStr, Value>,
    ) -> Self {
        let mut vec_interned = Vec::new();
        let mut vec_meta = Vec::new();
        let mut all_none = true;
        for (istr, Value(interned, extra)) in captures {
            all_none &= extra.is_none();
            vec_interned.push((istr, interned));
            vec_meta.push(extra);
        }
        let meta = if all_none {
            None
        } else {
            Some(Rc::new(ValueExtraDataBase::Lambda { captures: vec_meta }))
        };
        Value(
            intern_value(ValueBase::Lambda {
                rec,
                arg,
                body,
                captures: vec_interned,
            }),
            meta,
        )
    }

    pub fn inductive(ty: IStr, constr: IStr, args: Vec<Value>) -> Self {
        let mut vec_interned = Vec::new();
        let mut vec_meta = Vec::new();
        let mut all_none = true;
        for Value(interned, extra) in args {
            all_none &= extra.is_none();
            vec_interned.push(interned);
            vec_meta.push(extra);
        }
        let meta = if all_none {
            None
        } else {
            Some(Rc::new(ValueExtraDataBase::InductiveVal { args: vec_meta }))
        };
        Value(
            intern_value(ValueBase::InductiveVal {
                ty,
                constr,
                args: vec_interned,
            }),
            meta,
        )
    }

    pub fn is_fo(&self) -> bool {
        let b = get_interner_borrowed().read().unwrap();
        let xx = &b.lookup(self.0);
        xx.1.is_fo
    }

    pub fn as_closure<'a>(
        &'a self,
    ) -> Option<(
        Option<IStr>,
        IStr,
        MappedRwLockReadGuard<'a, ExprBox>,
        HashMap<IStr, Value>,
    )> {
        let bg = get_interner_borrowed().read().unwrap();
        let b = &bg.lookup(self.0).0;
        match b {
            ValueBase::Lambda {
                rec, arg, captures, ..
            } => {
                let x = match (&self.1).as_deref() {
                    Some(ValueExtraDataBase::Lambda { captures: c2 }) => captures
                        .iter()
                        .zip(c2.iter().cloned())
                        .map(|((i, x), y)| (*i, Value(*x, y)))
                        .collect(),
                    None => captures
                        .iter()
                        .map(|(i, x)| (*i, Value(*x, None)))
                        .collect(),
                    _ => unreachable!(),
                };
                let rec = *rec;
                let arg = *arg;
                let guard = RwLockReadGuard::map(bg, |x| match &x.lookup(self.0).0 {
                    ValueBase::Lambda { body, .. } => body,
                    _ => unreachable!(),
                });
                Some((rec, arg, guard, x))
            }
            _ => None,
        }
    }

    pub fn as_inductive(&self) -> Option<(IStr, IStr, Vec<Value>)> {
        let b = get_interner();
        let b = b.read().unwrap();
        let b = &b.lookup(self.0).0;
        match b {
            ValueBase::InductiveVal { ty, constr, args } => {
                let x = match (&self.1).as_deref() {
                    Some(ValueExtraDataBase::InductiveVal { args: a2 }) => args
                        .iter()
                        .zip(a2.iter().cloned())
                        .map(|(x, y)| Value(*x, y))
                        .collect(),
                    None => args.iter().map(|x| Value(*x, None)).collect(),
                    _ => unreachable!(),
                };
                Some((*ty, *constr, x))
            }
            _ => None,
        }
    }

    pub fn as_int<'a>(&'a self) -> Option<MappedRwLockReadGuard<'a, BigInt>> {
        let bg = get_interner_borrowed().read().unwrap();
        match RwLockReadGuard::try_map(bg, |x| match &x.lookup(self.0).0 {
            ValueBase::NumValue(x) => Some(x),
            _ => None,
        }) {
            Ok(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_pointer<'a>(
        &'a self,
    ) -> Option<(MappedRwLockReadGuard<'a, InfPointer>, &'a PointerOrigin)> {
        let bg = get_interner_borrowed().read().unwrap();
        match RwLockReadGuard::try_map(bg, |x| match &x.lookup(self.0).0 {
            ValueBase::PtrValue(x) => Some(x),
            _ => None,
        }) {
            Ok(x) => Some((
                x,
                match (&self.1).as_deref() {
                    Some(ValueExtraDataBase::PtrValue(x)) => x,
                    _ => unreachable!(),
                },
            )),
            _ => None,
        }
    }
}
