use crate::{ownership::OwnershipInfo, progs::memory::Memory};

#[derive(Clone, Debug)]
pub enum SpecMonadResidual {
    Fail { mode: bool },
    MissingOwnership(),
}

pub type SpecMonad<T> = Result<T, SpecMonadResidual>;

pub enum MonadFailMode {
    Assume,
    Assert,
}

pub struct MonadOwnershipArg<'a> {
    pub extract_from: &'a mut OwnershipInfo,
    pub extract_into: &'a mut OwnershipInfo,
    pub mode: MonadFailMode,
    pub memory: &'a Memory,
}

pub type MonadArg<'a> = Option<MonadOwnershipArg<'a>>;
