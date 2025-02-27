use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::utils::SwitchToDisplay;

use super::{
    ast::{Pointer, Value},
    errors::{MachineError, MachineErrorKind},
};

#[derive(Debug)]
pub struct Allocation {
    data: Vec<Value>,
}

#[derive(Debug)]
pub struct Memory {
    allocs: HashMap<u32, Allocation>,
    next_free: u32,
}

impl Allocation {
    pub fn new(len: u32) -> Self {
        Self {
            data: vec![Value::Int(0); len as usize],
        }
    }
}

impl Display for Allocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut comma = false;
        for x in &self.data {
            if comma {
                write!(f, ", ")?;
            }
            write!(f, "{x:?}")?;
            comma = true;
        }
        write!(f, "]")
    }
}

impl Display for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut keys: Vec<_> = self.allocs.keys().collect();
        keys.sort();
        let mut xx = &mut f.debug_struct("Memory");
        for k in keys {
            xx = xx.field(&format!("{k}"), &SwitchToDisplay(&self.allocs[k]));
        }
        xx.finish()
    }
}

impl Memory {
    pub fn new() -> Self {
        Self {
            allocs: HashMap::new(),
            next_free: 0,
        }
    }

    pub fn dealloc(&mut self, blk: u32) -> Result<(), MachineError> {
        match self.allocs.remove(&blk) {
            Some(_) => Ok(()),
            None => Err(MachineError::new(
                MachineErrorKind::MemoryUnsafety,
                format!("double free for ({blk}, 0)"),
            )),
        }
    }

    pub fn alloc(&mut self, len: u32) -> Option<u32> {
        assert_ne!(len, 0);
        let start = self.next_free;
        loop {
            if self.allocs.contains_key(&self.next_free) {
                self.next_free += 1;
                if self.next_free == start {
                    return None;
                } else {
                    continue;
                }
            } else {
                self.allocs.insert(self.next_free, Allocation::new(len));
                let res = self.next_free;
                self.next_free += 1;
                return Some(res);
            }
        }
    }

    pub fn load(&self, off: Pointer) -> Result<Value, MachineError> {
        let data = &self
            .allocs
            .get(&off.0)
            .ok_or_else(|| {
                MachineError::new(
                    MachineErrorKind::MemoryUnsafety,
                    format!("use-after-free for {:?}", off),
                )
            })?
            .data;
        let size = data.len();
        data.get(off.1 as usize).copied().ok_or_else(|| {
            MachineError::new(
                MachineErrorKind::MemoryUnsafety,
                format!("out-of-bounds for {:?} (allocation size is {size}", off),
            )
        })
    }

    pub fn store(&mut self, off: Pointer, new_data: Value) -> Result<(), MachineError> {
        let data = &mut self
            .allocs
            .get_mut(&off.0)
            .ok_or_else(|| {
                MachineError::new(
                    MachineErrorKind::MemoryUnsafety,
                    format!("use-after-free for {:?}", off),
                )
            })?
            .data;
        let size = data.len();
        if (off.1 as usize) < data.len() {
            data[off.1 as usize] = new_data;
            Ok(())
        } else {
            Err(MachineError::new(
                MachineErrorKind::MemoryUnsafety,
                format!("out-of-bounds for {:?} (allocation size is {size}", off),
            ))
        }
    }
}
