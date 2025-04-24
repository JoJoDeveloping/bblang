use std::rc::Rc;

use crate::{
    specs::exec::{InfPointer, Value},
    utils::string_interner::intern,
};

pub fn ptradd(v1: &Rc<Value>, v2: &Rc<Value>) -> Rc<Value> {
    match (&**v1, &**v2) {
        (Value::PtrValue(n1, p), Value::NumValue(n2)) => Rc::new(Value::PtrValue(
            InfPointer(n1.0, n1.1.clone() + n2),
            p.clone(),
        )),
        _ => panic!("Illegal call to add!"),
    }
}

pub fn ptrdiff(v1: &Rc<Value>, v2: &Rc<Value>) -> Rc<Value> {
    match (&**v1, &**v2) {
        (Value::PtrValue(InfPointer(o1, n1), _), Value::PtrValue(InfPointer(o2, n2), _))
            if o1 == o2 =>
        {
            Rc::new(Value::InductiveVal {
                ty: intern("Option"),
                constr: intern("Some"),
                args: vec![Rc::new(Value::NumValue(n1.clone() - n2))],
            })
        }
        _ => panic!("Illegal call to add!"),
    }
}
