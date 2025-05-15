use std::{
    hash::{Hash, Hasher},
    rc::Rc,
    sync::{Arc, OnceLock, RwLock},
};

use crate::{
    specs::exec::pointer_origin::PointerOrigin,
    utils::interner::{InternableBase, InternableFor, Interned, Interner},
};

use super::{traits::ValueLike, value_base::ValueBase};

#[derive(Clone, Copy, Debug)]
pub(super) struct ValueStoredMeta {
    pub(super) is_fo: bool,
}

impl Hash for ValueStoredMeta {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

impl PartialEq for ValueStoredMeta {
    // ignore the data in here
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for ValueStoredMeta {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct ValueStoredData<T = ValueBase>(pub(super) T, pub(super) ValueStoredMeta);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(super) enum ValueExtraDataBase {
    Lambda { captures: Vec<ValueExtraData> },
    InductiveVal { args: Vec<ValueExtraData> },
    PtrValue(PointerOrigin),
}

pub(super) type ValueExtraData = Option<Rc<ValueExtraDataBase>>;

impl InternableBase for ValueStoredData {}

static INTERNER: OnceLock<Arc<RwLock<Interner<ValueStoredData>>>> = OnceLock::new();

pub(super) fn get_interner_borrowed<'a>() -> &'a RwLock<Interner<ValueStoredData>> {
    INTERNER.get_or_init(|| Arc::new(RwLock::new(Interner::new())))
}

pub(super) fn get_interner() -> Arc<RwLock<Interner<ValueStoredData>>> {
    INTERNER
        .get_or_init(|| Arc::new(RwLock::new(Interner::new())))
        .clone()
}

pub(super) fn intern_value<S: ValueLike>(s: S) -> Interned
where
    ValueStoredData<S>: InternableFor<ValueStoredData>,
{
    let meta = ValueStoredMeta { is_fo: s.is_fo() };
    let data = ValueStoredData(s, meta);
    get_interner().write().unwrap().intern(data)
}

impl<T: ValueLike + Hash> InternableFor<ValueStoredData> for ValueStoredData<T> {
    fn into_base(&self) -> ValueStoredData {
        ValueStoredData(self.0.into_value(), self.1)
    }

    fn compare_by_ref(&self, other: &ValueStoredData) -> bool {
        self.0.compare_by_ref(&other.0)
    }
}
