use std::fmt::Display;

#[derive(Clone, Copy, Debug)]
pub enum MachineErrorKind {
    MemoryUnsafety,
    MismatchedArgs,
    JumpOutOfBounds,
    UnboundObject,
    Overflow,
}

#[derive(Clone, Debug)]
pub struct MachineError {
    kind: MachineErrorKind,
    details: String,
    #[allow(unused)]
    origin: (),
}

impl MachineError {
    pub fn new(kind: MachineErrorKind, details: String) -> Self {
        Self {
            kind,
            details,
            origin: (),
        }
    }
}

impl Display for MachineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UB detected ({:?}): {}!", self.kind, self.details)
    }
}
