use crate::{ownership::OwnershipPredicate, progs::memory::Memory};

#[derive(Clone, Debug)]
pub enum SpecMonadResidual {
    Fail { mode: bool },
    MissingOwnership(),
}

pub type SpecMonad<T> = Result<T, SpecMonadResidual>;

#[derive(Clone, Copy)]
pub enum MonadFailMode {
    Assume,
    Assert,
}

pub struct MonadOwnershipArg<'a> {
    pub extract_from: &'a mut OwnershipPredicate,
    pub extract_into: &'a mut OwnershipPredicate,
    pub mode: MonadFailMode,
    pub memory: &'a Memory,
}

pub type MonadArg<'a> = Option<MonadOwnershipArg<'a>>;

impl<'a> MonadOwnershipArg<'a> {
    pub fn with_new_into<'l, 'b>(
        self: &'l mut MonadOwnershipArg<'a>,
        new: &'b mut OwnershipPredicate,
    ) -> MonadOwnershipArg<'b>
    where
        'a: 'b,
        'l: 'b,
    {
        MonadOwnershipArg {
            extract_from: self.extract_from,
            extract_into: new,
            mode: self.mode,
            memory: self.memory,
        }
    }
}
