use std::collections::HashSet;

use crate::specs::exec::InfPointer;

#[derive(Debug)]
pub struct OwnershipInfo {
    allowed_accessible: HashSet<InfPointer>,
}
impl OwnershipInfo {
    pub fn new() -> Self {
        Self {
            allowed_accessible: HashSet::new(),
        }
    }

    pub fn ensure(&self, p: &InfPointer) -> bool {
        self.allowed_accessible.contains(&p)
    }

    pub fn insert(&mut self, p: &InfPointer) {
        self.allowed_accessible.insert(p.clone());
    }

    pub fn remove(&mut self, p: &InfPointer) -> bool {
        self.allowed_accessible.remove(&p)
    }
}
