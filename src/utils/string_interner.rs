use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::{BuildHasher, Hash, Hasher},
    ops::Deref,
    sync::{Arc, OnceLock, RwLock},
};

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct IStr(usize);

struct Interner {
    strings: Vec<String>,
    to_idx: HashMap<RawHashKeyValue, ()>,
}

#[derive(Clone, Copy)]
pub struct RawHashKeyValue(u64, pub usize);

impl RawHashKeyValue {
    pub fn new_in<S: BuildHasher, H: Hash>(bh: &S, x: H, data: usize) -> Self {
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

impl Interner {
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            to_idx: HashMap::new(),
        }
    }

    pub fn intern<S: StrInternable>(&mut self, s: S) -> IStr {
        let mut key = RawHashKeyValue::new_in(self.to_idx.hasher(), &*s, 0);
        let keyh = key.hash_in(self.to_idx.hasher());
        let x = self
            .to_idx
            .raw_entry_mut()
            .from_hash(keyh, |kv| s == self.strings[kv.1].as_str());
        let idx = x
            .or_insert_with(|| {
                key.1 = self.strings.len();
                let sss = s.to_string();
                self.strings.push(sss);
                (key, ())
            })
            .0
            .1;
        assert_eq!(
            key.raw_hash(),
            self.to_idx.hasher().hash_one(&self.strings[idx])
        );
        IStr(idx)
    }

    pub fn lookup(&self, idx: IStr) -> &str {
        &self.strings[idx.0]
    }
}

static INTERNER: OnceLock<Arc<RwLock<Interner>>> = OnceLock::new();

fn get_interner() -> Arc<RwLock<Interner>> {
    INTERNER
        .get_or_init(|| Arc::new(RwLock::new(Interner::new())))
        .clone()
}

impl Debug for IStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("IStr")
            .field(&self.0)
            .field(&get_interner().read().unwrap().lookup(*self))
            .finish()
    }
}

impl Display for IStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(get_interner().read().unwrap().lookup(*self))
    }
}

pub fn intern<S: StrInternable>(s: S) -> IStr {
    get_interner().write().unwrap().intern(s)
}

/// This trait in particular says that all inhabitants behave correctly.
/// In particular, their trait implementations make sense and behave consistend with the thing representing a string.
pub trait StrInternable: Deref<Target = str> + for<'a> PartialEq<&'a str> + ToString {}

impl StrInternable for String {}
impl StrInternable for &str {}

#[cfg(test)]
mod test {
    use super::intern;

    #[test]
    fn test_basic() {
        let s1 = intern("foo");
        let s2 = intern("bar");
        let s3 = intern("baz");
        let s4 = intern("foo");
        assert_eq!(s1, s4);
        assert_ne!(s1, s2);
        assert_ne!(s1, s3);
        assert_ne!(s2, s3);
    }

    #[test]
    fn test_both_strings() {
        let s1 = intern("Foo");
        let s2 = intern("Foo".to_string());
        assert_eq!(s1, s2);
    }
}
