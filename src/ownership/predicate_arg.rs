use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    rc::Rc,
    sync::{Arc, OnceLock, RwLock},
};

use num_bigint::BigInt;

use crate::{
    specs::exec::{
        InfPointer, Value,
        pointer_origin::{PointerOrigin, PointerOrigins},
    },
    utils::{
        interner::{InternableBase, InternableFor, Interned, Interner},
        string_interner::IStr,
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PredArgBase {
    InductiveVal {
        ty: IStr,
        constr: IStr,
        args: Vec<PredArg>,
    },
    NumValue(BigInt),
    PtrValue(InfPointer),
}

impl Hash for PredArgBase {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) -> () {
        match self {
            PredArgBase::InductiveVal { ty, constr, args } => {
                Hash::hash(&0, state);
                Hash::hash(ty, state);
                Hash::hash(constr, state);
                Hash::hash(args, state)
            }
            PredArgBase::NumValue(b) => {
                Hash::hash(&1, state);
                Hash::hash(b, state)
            }
            PredArgBase::PtrValue(p) => {
                Hash::hash(&2, state);
                Hash::hash(p, state)
            }
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct PredArg(Interned);

impl InternableBase for PredArgBase {}

static INTERNER: OnceLock<Arc<RwLock<Interner<PredArgBase>>>> = OnceLock::new();

fn get_interner() -> Arc<RwLock<Interner<PredArgBase>>> {
    INTERNER
        .get_or_init(|| Arc::new(RwLock::new(Interner::new())))
        .clone()
}

impl Debug for PredArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PredArg")
            .field(&self.0)
            .field(&get_interner().read().unwrap().lookup(self.0))
            .finish()
    }
}

fn intern_pred_arg<S: InternableFor<PredArgBase>>(s: S) -> PredArg {
    PredArg(get_interner().write().unwrap().intern(s))
}

impl InternableFor<PredArgBase> for PredArgBase {
    fn into_base(&self) -> PredArgBase {
        self.clone()
    }

    fn compare_by_ref(&self, other: &PredArgBase) -> bool {
        self == other
    }
}

impl PredArg {
    pub fn from_value_inner(v: &Value, m: &mut PointerOrigins) -> Result<PredArg, ()> {
        Ok(match v {
            Value::InductiveVal { ty, constr, args } => {
                intern_pred_arg(PredArgBase::InductiveVal {
                    ty: *ty,
                    constr: *constr,
                    args: args
                        .iter()
                        .map(|x| PredArg::from_value_inner(&**x, m))
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            Value::Lambda { .. } => return Err(()), //panic!("Constructing higher-order predicate arg!"),
            Value::NumValue(big_int) => intern_pred_arg(PredArgByRef::NumValue(big_int)),
            Value::PtrValue(inf_pointer, origin) => {
                m.insert(origin.clone());
                intern_pred_arg(PredArgByRef::PtrValue(inf_pointer))
            }
        })
    }

    pub fn into_value_with_provenance(self, prov: impl Copy + Fn() -> PointerOrigin) -> Rc<Value> {
        let interner = get_interner();
        let read = interner.read().unwrap();
        Rc::new(match read.lookup(self.0) {
            PredArgBase::InductiveVal { ty, constr, args } => {
                let ty = *ty;
                let constr = *constr;
                let args = args.clone();
                drop(read);
                Value::InductiveVal {
                    ty,
                    constr,
                    args: args
                        .into_iter()
                        .map(|x| x.into_value_with_provenance(prov))
                        .collect(),
                }
            }
            PredArgBase::NumValue(big_int) => Value::NumValue(big_int.clone()),
            PredArgBase::PtrValue(inf_pointer) => Value::PtrValue(inf_pointer.clone(), prov()),
        })
    }
}

enum PredArgByRef<'a> {
    NumValue(&'a BigInt),
    PtrValue(&'a InfPointer),
}

impl<'a> Hash for PredArgByRef<'a> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) -> () {
        match self {
            PredArgByRef::NumValue(b) => {
                Hash::hash(&1, state);
                Hash::hash(*b, state)
            }
            PredArgByRef::PtrValue(p) => {
                Hash::hash(&2, state);
                Hash::hash(*p, state)
            }
        }
    }
}

impl<'a> InternableFor<PredArgBase> for PredArgByRef<'a> {
    fn into_base(&self) -> PredArgBase {
        match self {
            PredArgByRef::NumValue(big_int) => PredArgBase::NumValue((*big_int).clone()),
            PredArgByRef::PtrValue(inf_pointer) => PredArgBase::PtrValue((*inf_pointer).clone()),
        }
    }

    fn compare_by_ref(&self, other: &PredArgBase) -> bool {
        match (self, other) {
            (PredArgByRef::NumValue(x1), PredArgBase::NumValue(x2)) => *x1 == x2,
            (PredArgByRef::PtrValue(x1), PredArgBase::PtrValue(x2)) => *x1 == x2,
            _ => false,
        }
    }
}

impl Display for PredArgBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredArgBase::InductiveVal { ty, constr, args } => {
                write!(f, "{ty}::{constr}(")?;
                let mut comma = false;
                for v in args {
                    if comma {
                        write!(f, ", ")?
                    } else {
                        comma = true
                    }
                    Display::fmt(v, f)?;
                }
                write!(f, ")")
            }
            PredArgBase::NumValue(x) => Display::fmt(x, f),
            PredArgBase::PtrValue(x) => Display::fmt(x, f),
        }
    }
}

impl Display for PredArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(get_interner().read().unwrap().lookup(self.0), f)
    }
}
