use std::rc::Rc;

use num_bigint::{BigInt, Sign};

use crate::{
    progs::{ast::Pointer, machine::value_to_value},
    specs::exec::{
        Value,
        monad::{MonadArg, SpecMonad, SpecMonadResidual},
        pointer_origin::PointerOrigin,
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
    let Value::PtrValue(ptr, _) = &**v1 else {
        panic!("Illegal call to Owned!")
    };
    let Some(aa) = args else {
        panic!("Call to Owned in wrong mode!");
    };
    if !aa.extract_from.remove(ptr) {
        eprintln!("{:?} -> {:?}", aa.extract_from, aa.extract_into);
        eprintln!("Missing ownership {ptr:?}");
        return Err(SpecMonadResidual::MissingOwnership());
    }
    aa.extract_into.insert(ptr);
    let v = aa
        .memory
        .load(Pointer(ptr.0, (&ptr.1).try_into().unwrap()))
        .unwrap();
    Ok(value_to_value(v, || PointerOrigin::LoadedFrom(ptr.clone())))
}

pub fn block<'b>(
    ptr: &Rc<Value>,
    len: &Rc<Value>,
    args: &mut MonadArg<'b>,
) -> SpecMonad<Rc<Value>> {
    let (Value::PtrValue(ptr, _), Value::NumValue(num)) = (&**ptr, &**len) else {
        panic!("Illegal call to Owned!")
    };
    let Some(aa) = args else {
        panic!("Call to Owned in wrong mode!");
    };
    if ptr.1.sign() != Sign::NoSign {
        return Err(SpecMonadResidual::Fail { mode: true });
    }
    if BigInt::from(aa.memory.get_block(ptr.0).unwrap().length()) != *num {
        return Err(SpecMonadResidual::Fail { mode: true });
    }
    if !aa.extract_from.remove_block(ptr.0) {
        eprintln!("{:?} -> {:?}", aa.extract_from, aa.extract_into);
        eprintln!("Missing ownership {ptr:?}");
        return Err(SpecMonadResidual::MissingOwnership());
    }
    aa.extract_into.insert_block(ptr.0);
    Ok(Rc::new(Value::InductiveVal {
        ty: intern("Unit"),
        constr: intern("Unit"),
        args: vec![],
    }))
}
