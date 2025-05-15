use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{specs::exec::pointer_origin::PointerOrigins, utils::interner::Interned};

use super::{Value, ValueExtraDataBase, get_interner};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PredArg(Interned);

trait PointerOriginsContainer {
    fn collect_all(&self, m: &mut PointerOrigins);
}

impl<T: PointerOriginsContainer> PointerOriginsContainer for Option<T> {
    fn collect_all(&self, m: &mut PointerOrigins) {
        if let Some(x) = self {
            x.collect_all(m);
        }
    }
}

impl<T: PointerOriginsContainer> PointerOriginsContainer for Rc<T> {
    fn collect_all(&self, m: &mut PointerOrigins) {
        T::collect_all(self.as_ref(), m);
    }
}

impl PointerOriginsContainer for ValueExtraDataBase {
    fn collect_all(&self, m: &mut PointerOrigins) {
        match self {
            ValueExtraDataBase::Lambda { captures } => {
                captures.iter().for_each(|x| x.collect_all(m))
            }
            ValueExtraDataBase::InductiveVal { args } => args.iter().for_each(|x| x.collect_all(m)),
            ValueExtraDataBase::PtrValue(pointer_origin) => m.insert(pointer_origin.clone()),
        }
    }
}

impl PredArg {
    pub fn from_value_inner(v: Value, m: &mut PointerOrigins) -> Result<PredArg, ()> {
        if !v.is_fo() {
            return Err(());
        }

        v.1.collect_all(m);

        Ok(PredArg(v.0))
    }
}

impl Debug for PredArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PredArg")
            .field(&get_interner().read().unwrap().lookup(self.0).0)
            .field(&self.0)
            .finish()
    }
}

impl Display for PredArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&get_interner().read().unwrap().lookup(self.0).0, f)
    }
}
