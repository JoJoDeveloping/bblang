use std::rc::Rc;

use crate::{
    progs::{ast::Pointer, machine::value_to_value},
    specs::exec::{
        Value,
        monad::{MonadArg, SpecMonad, SpecMonadResidual},
    },
    utils::string_interner::intern,
};

pub fn fail(v1: &Rc<Value>) -> SpecMonad<Rc<Value>> {
    let mode = match &**v1 {
        Value::InductiveVal { ty, constr, args }
            if *ty == intern("Bool") && *constr == intern("True") && args.len() == 0 =>
        {
            true
        }
        Value::InductiveVal { ty, constr, args }
            if *ty == intern("Bool") && *constr == intern("False") && args.len() == 0 =>
        {
            false
        }
        _ => panic!("Illegal call to Fail!"),
    };
    Err(SpecMonadResidual::Fail { mode })
}

pub fn owned<'b>(v1: &Rc<Value>, args: &mut MonadArg<'b>) -> SpecMonad<Rc<Value>> {
    let Value::PtrValue(ptr) = &**v1 else {
        panic!("Illegal call to Owned!")
    };
    let Some(aa) = args else {
        panic!("Call to Owned in wrong mode!");
    };
    if !aa.extract_from.remove(ptr) {
        eprintln!("Missing ownership {ptr:?}");
        return Err(SpecMonadResidual::MissingOwnership());
    }
    aa.extract_into.insert(ptr);
    let v = aa
        .memory
        .load(Pointer(ptr.0, (&ptr.1).try_into().unwrap()))
        .unwrap();
    Ok(value_to_value(v))
}
