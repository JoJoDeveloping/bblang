use std::{collections::HashSet, fmt::Debug};

use super::InfPointer;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum PointerOrigin {
    Immediate,
    LoadedFrom(InfPointer),
}

impl PointerOrigin {}

#[derive(Clone)]
pub struct PointerOrigins {
    immediate: bool,
    loaded_from: HashSet<InfPointer>,
}

impl PointerOrigins {
    pub fn new() -> Self {
        Self {
            immediate: false,
            loaded_from: HashSet::new(),
        }
    }

    pub fn insert(&mut self, origin: PointerOrigin) {
        match origin {
            PointerOrigin::Immediate => {
                self.immediate = true;
            }
            PointerOrigin::LoadedFrom(inf_pointer) => {
                self.loaded_from.insert(inf_pointer);
            }
        };
    }

    pub fn insert_all(&mut self, origins: PointerOrigins) {
        self.immediate |= origins.immediate;
        self.loaded_from.extend(origins.loaded_from);
    }

    pub fn is_immediate(&self) -> bool {
        self.immediate
    }

    pub fn non_immediate_origins(&self) -> &HashSet<InfPointer> {
        &self.loaded_from
    }

    pub fn into_non_immediate_origins(self) -> HashSet<InfPointer> {
        self.loaded_from
    }
}

impl Debug for PointerOrigins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        if self.is_immediate() {
            write!(f, "Immediate, ")?;
        }
        for origin in self.non_immediate_origins() {
            write!(f, "{origin}, ")?;
        }
        write!(f, "}}")
    }
}
