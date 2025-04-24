use std::{
    fmt::{Debug, Display},
    fs,
};

pub mod disjount_extend;
pub mod interner;
pub mod string_interner;

pub struct SwitchToDisplay<T>(pub T);
impl<T: Display> Debug for SwitchToDisplay<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

pub fn read_file(path: &str) -> String {
    fs::read_to_string(path).unwrap()
}

pub fn indent(n: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for _ in 0..n {
        write!(f, " ")?;
    }
    Ok(())
}
