use crate::{specs::values::Value, utils::string_interner::intern};

pub fn add(v1: &Value, v2: &Value) -> Value {
    Value::num_value(&match (v1.as_int(), v2.as_int()) {
        (Some(n1), Some(n2)) => n1.clone() + &*n2,
        _ => panic!("Illegal call to add!"),
    })
}

pub fn mul(v1: &Value, v2: &Value) -> Value {
    Value::num_value(&match (v1.as_int(), v2.as_int()) {
        (Some(n1), Some(n2)) => n1.clone() * &*n2,
        _ => panic!("Illegal call to mul!"),
    })
}

pub fn le(v1: &Value, v2: &Value) -> Value {
    Value::inductive(
        intern("Bool"),
        match (v1.as_int(), v2.as_int()) {
            (Some(n1), Some(n2)) => {
                if *n1 <= *n2 {
                    intern("True")
                } else {
                    intern("False")
                }
            }
            _ => panic!("Illegal call to le!"),
        },
        vec![],
    )
}

pub fn neg(v1: &Value) -> Value {
    Value::num_value(&match v1.as_int() {
        Some(n1) => -(n1.clone()),
        _ => panic!("Illegal call to neg!"),
    })
}

pub fn bitand(v1: &Value, v2: &Value) -> Value {
    Value::num_value(&match (v1.as_int(), v2.as_int()) {
        (Some(n1), Some(n2)) => n1.clone() & &*n2,
        _ => panic!("Illegal call to bitand!"),
    })
}
