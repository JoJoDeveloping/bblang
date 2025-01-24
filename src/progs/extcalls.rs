use std::{collections::HashMap, fmt::Debug};

use crate::utils::string_interner::{IStr, intern};

use super::{ast::Value, memory::Memory};

pub struct ExtcallHandler {
    calls: HashMap<IStr, Box<dyn for<'a> FnMut(&mut Memory, Vec<Value>) -> Result<Value, ()>>>,
}

impl ExtcallHandler {
    pub fn new() -> Self {
        let mut calls: HashMap<
            IStr,
            Box<dyn for<'a> FnMut(&mut Memory, Vec<Value>) -> Result<Value, ()>>,
        > = HashMap::new();
        calls.insert(intern("putchar"), Box::new(lib::putchar));
        calls.insert(intern("debug"), Box::new(lib::debug));
        Self { calls }
    }

    pub fn handle(
        &mut self,
        memory: &mut Memory,
        fnname: IStr,
        args: Vec<Value>,
    ) -> Result<Value, ()> {
        self.calls.get_mut(&fnname).ok_or(())?(memory, args)
    }
}

impl Debug for ExtcallHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExtcallHandler").finish()
    }
}

mod lib {
    use std::io::Write;

    use crate::progs::{ast::Value, memory::Memory};

    pub fn putchar(_mem: &mut Memory, args: Vec<Value>) -> Result<Value, ()> {
        match &args[..] {
            [Value::Int(i)] => {
                if *i < 0 || *i > 255 {
                    return Err(());
                }
                std::io::stdout()
                    .lock()
                    .write_all(&[(*i).try_into().unwrap()])
                    .unwrap();
                Ok(Value::Int(0))
            }
            _ => Err(()),
        }
    }

    pub fn debug(mem: &mut Memory, args: Vec<Value>) -> Result<Value, ()> {
        println!("Debug called with: {args:?}");
        println!("Memory dump: {mem:?}");
        Ok(Value::Int(0))
    }
}
