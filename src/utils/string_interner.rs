use std::{
    fmt::{Debug, Display},
    sync::{Arc, OnceLock, RwLock},
};

use super::interner::{InternableBase, InternableFor, Interned, Interner};

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct IStr(Interned);

impl InternableBase for String {}

static INTERNER: OnceLock<Arc<RwLock<Interner<String>>>> = OnceLock::new();

fn get_interner() -> Arc<RwLock<Interner<String>>> {
    INTERNER
        .get_or_init(|| Arc::new(RwLock::new(Interner::new())))
        .clone()
}

impl Debug for IStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("IStr")
            .field(&self.0)
            .field(&get_interner().read().unwrap().lookup(self.0))
            .finish()
    }
}

impl Display for IStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(get_interner().read().unwrap().lookup(self.0))
    }
}

pub fn intern<S: InternableFor<String>>(s: S) -> IStr {
    IStr(get_interner().write().unwrap().intern(s))
}

/// This trait in particular says that all inhabitants behave correctly.
/// In particular, their trait implementations make sense and behave consistend with the thing representing a string.

impl InternableFor<String> for String {
    fn into_base(&self) -> String {
        self.clone()
    }

    fn compare_by_ref(&self, other: &String) -> bool {
        self == other
    }
}
impl InternableFor<String> for &str {
    fn into_base(&self) -> String {
        self.to_string()
    }

    fn compare_by_ref(&self, other: &String) -> bool {
        *self == other
    }
}

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
