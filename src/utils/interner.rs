use std::{
    collections::HashMap,
    fmt::Debug,
    hash::{BuildHasher, Hash, Hasher},
};

#[derive(Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Interned(usize);

pub struct Interner<T: InternableBase> {
    strings: Vec<T>,
    to_idx: HashMap<RawHashKeyValue, ()>,
}

#[derive(Clone, Copy)]
pub struct RawHashKeyValue(u64, pub usize);

impl RawHashKeyValue {
    pub fn new_in<S: BuildHasher, H: Hash>(bh: &S, x: &H, data: usize) -> Self {
        Self(bh.hash_one(x), data)
    }

    pub fn hash_in<S: BuildHasher>(self, bh: &S) -> u64 {
        bh.hash_one(self)
    }

    pub fn raw_hash(self) -> u64 {
        self.0
    }
}

impl Hash for RawHashKeyValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

pub trait InternableBase: Hash + Eq {}

pub trait InternableFor<Base>: Hash
where
    Base: InternableBase,
{
    fn into_base(&self) -> Base;
    fn compare_by_ref(&self, other: &Base) -> bool;
}

impl<T> Interner<T>
where
    T: InternableBase,
{
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            to_idx: HashMap::new(),
        }
    }

    pub fn intern<S: InternableFor<T>>(&mut self, s: S) -> Interned {
        let mut key = RawHashKeyValue::new_in(self.to_idx.hasher(), &s, 0);
        let keyh = key.hash_in(self.to_idx.hasher());
        let x = self
            .to_idx
            .raw_entry_mut()
            .from_hash(keyh, |kv| s.compare_by_ref(&self.strings[kv.1]));
        let idx = x
            .or_insert_with(|| {
                key.1 = self.strings.len();
                let sss = s.into_base();
                self.strings.push(sss);
                (key, ())
            })
            .0
            .1;
        assert_eq!(
            key.raw_hash(),
            self.to_idx.hasher().hash_one(&self.strings[idx])
        );
        Interned(idx)
    }

    pub fn lookup(&self, idx: Interned) -> &T {
        &self.strings[idx.0]
    }
}

impl Debug for Interned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}
