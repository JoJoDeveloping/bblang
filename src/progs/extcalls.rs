use std::{collections::HashMap, fmt::Debug};

use crate::utils::string_interner::{IStr, intern};

use super::{
    ast::Value,
    errors::{MachineError, MachineErrorKind},
    memory::Memory,
};

pub struct ExtcallHandler {
    calls: HashMap<
        IStr,
        Box<dyn for<'a> FnMut(&mut Memory, Vec<Value>) -> Result<Value, MachineError>>,
    >,
}

impl ExtcallHandler {
    pub fn new() -> Self {
        let mut calls: HashMap<
            IStr,
            Box<dyn for<'a> FnMut(&mut Memory, Vec<Value>) -> Result<Value, MachineError>>,
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
    ) -> Result<Value, MachineError> {
        self.calls.get_mut(&fnname).ok_or_else(|| {
            MachineError::new(
                MachineErrorKind::UnboundObject,
                format!("function {fnname} does not exist"),
            )
        })?(memory, args)
    }
}

impl Debug for ExtcallHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExtcallHandler").finish()
    }
}

mod lib {
    use std::io::Write;

    use crate::progs::{
        ast::Value,
        errors::{MachineError, MachineErrorKind},
        memory::Memory,
    };

    pub fn putchar(_mem: &mut Memory, args: Vec<Value>) -> Result<Value, MachineError> {
        match &args[..] {
            [Value::Int(i)] => {
                if *i < 0 || *i > 255 {
                    return Err(MachineError::new(
                        MachineErrorKind::Overflow,
                        format!("putchar() argument {i} must be a byte"),
                    ));
                }
                std::io::stdout()
                    .lock()
                    .write_all(&[(*i).try_into().unwrap()])
                    .unwrap();
                Ok(Value::Int(0))
            }
            _ => Err(MachineError::new(
                MachineErrorKind::MismatchedArgs,
                "putchar requires exactly one argument".to_string(),
            )),
        }
    }

    pub fn debug(mem: &mut Memory, args: Vec<Value>) -> Result<Value, MachineError> {
        println!("Debug called with: {args:?}");
        println!("Memory dump: {mem:?}");
        Ok(Value::Int(0))
    }
}
