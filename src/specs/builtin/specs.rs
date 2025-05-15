use num_bigint::{BigInt, Sign};

use crate::{
    progs::{ast::Pointer, machine::value_to_value},
    specs::{
        exec::{
            monad::{MonadArg, SpecMonad, SpecMonadResidual},
            pointer_origin::PointerOrigin,
        },
        values::Value,
    },
    utils::string_interner::intern,
};

pub fn fail(v1: &Value) -> SpecMonad<Value> {
    let mode = match v1.as_inductive() {
        Some((ty, constr, args))
            if ty == intern("Bool")
                && (constr == intern("True") || constr == intern("False"))
                && args.len() == 0 =>
        {
            constr == intern("True")
        }
        _ => panic!("Illegal call to Fail!"),
    };
    Err(SpecMonadResidual::Fail { mode })
}

pub fn owned<'b>(v1: &Value, args: &mut MonadArg<'b>) -> SpecMonad<Value> {
    let Some((ptr, _)) = v1.as_pointer() else {
        panic!("Illegal call to Owned!")
    };
    let Some(aa) = args else {
        panic!("Call to Owned in wrong mode!");
    };
    if !aa.extract_from.remove(&*ptr) {
        eprintln!("{:?} -> {:?}", aa.extract_from, aa.extract_into);
        eprintln!("Missing ownership {ptr:?}");
        return Err(SpecMonadResidual::MissingOwnership());
    }
    aa.extract_into.insert(&*ptr);
    let v = aa
        .memory
        .load(Pointer(ptr.0, (&ptr.1).try_into().unwrap()))
        .unwrap();
    let pptr = ptr.clone();
    drop(ptr);
    Ok(value_to_value(v, || {
        PointerOrigin::LoadedFrom(pptr.clone())
    }))
}

pub fn block<'b>(ptr: &Value, len: &Value, args: &mut MonadArg<'b>) -> SpecMonad<Value> {
    let (Some((ptr, _)), Some(num)) = (ptr.as_pointer(), len.as_int()) else {
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
    drop(ptr);
    drop(num);
    Ok(Value::inductive(intern("Unit"), intern("Unit"), vec![]))
}

pub fn eq(v1: &Value, v2: &Value) -> Value {
    assert!(v1.is_fo() && v2.is_fo());
    Value::inductive(
        intern("Bool"),
        if v1 == v2 {
            intern("True")
        } else {
            intern("False")
        },
        vec![],
    )
}
