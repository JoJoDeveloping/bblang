use std::{
    fmt::{Debug, Display},
    fs,
};

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
