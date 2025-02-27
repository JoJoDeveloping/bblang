use std::rc::Rc;

use crate::{specs::exec::Value, utils::string_interner::intern};

pub fn add(v1: &Rc<Value>, v2: &Rc<Value>) -> Rc<Value> {
    match (&**v1, &**v2) {
        (Value::NumValue(n1), Value::NumValue(n2)) => Rc::new(Value::NumValue(n1.clone() + n2)),
        _ => panic!("Illegal call to add!"),
    }
}

pub fn mul(v1: &Rc<Value>, v2: &Rc<Value>) -> Rc<Value> {
    match (&**v1, &**v2) {
        (Value::NumValue(n1), Value::NumValue(n2)) => Rc::new(Value::NumValue(n1.clone() * n2)),
        _ => panic!("Illegal call to mul!"),
    }
}

pub fn le(v1: &Rc<Value>, v2: &Rc<Value>) -> Rc<Value> {
    match (&**v1, &**v2) {
        (Value::NumValue(n1), Value::NumValue(n2)) if n1 <= n2 => Rc::new(Value::InductiveVal {
            ty: intern("Bool"),
            constr: intern("True"),
            args: vec![],
        }),
        (Value::NumValue(_), Value::NumValue(_)) => Rc::new(Value::InductiveVal {
            ty: intern("Bool"),
            constr: intern("False"),
            args: vec![],
        }),
        _ => panic!("Illegal call to le!"),
    }
}

pub fn neg(v1: &Rc<Value>) -> Rc<Value> {
    match &**v1 {
        Value::NumValue(n1) => Rc::new(Value::NumValue(-n1)),
        _ => panic!("Illegal call to neg!"),
    }
}

pub fn bitand(v1: &Rc<Value>, v2: &Rc<Value>) -> Rc<Value> {
    match (&**v1, &**v2) {
        (Value::NumValue(n1), Value::NumValue(n2)) => Rc::new(Value::NumValue(n1.clone() & n2)),
        _ => panic!("Illegal call to bitand!"),
    }
}
