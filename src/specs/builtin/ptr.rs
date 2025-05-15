use crate::{
    specs::{InfPointer, values::Value},
    utils::string_interner::intern,
};

pub fn ptradd(v1: &Value, v2: &Value) -> Value {
    let (p1, p2) = match (v1.as_pointer(), v2.as_int()) {
        (Some((n1, p)), Some(n2)) => (&InfPointer(n1.0, n1.1.clone() + &*n2), p.clone()),
        _ => panic!("Illegal call to ptradd!"),
    };
    Value::ptr_value(p1, p2)
}

pub fn ptrdiff(v1: &Value, v2: &Value) -> Value {
    let res = match (v1.as_pointer(), v2.as_pointer()) {
        (Some((ip1, _)), Some((ip2, _))) if ip1.0 == ip2.0 => Some(ip1.1.clone() - &ip1.1),
        (Some(_), Some(_)) => None,
        _ => panic!("Illegal call to ptrdiff!"),
    };
    match res {
        Some(x) => Value::inductive(intern("Option"), intern("Some"), vec![Value::num_value(&x)]),
        None => Value::inductive(intern("Option"), intern("None"), vec![]),
    }
}
