use std::fmt::{Debug, Display};

pub mod string_interner;

pub struct SwitchToDisplay<T>(pub T);
impl<T: Display> Debug for SwitchToDisplay<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}
