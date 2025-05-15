use super::value_base::ValueBase;

pub(super) trait ValueLike {
    fn is_fo(&self) -> bool;
    fn into_value(&self) -> ValueBase;
    fn compare_by_ref(&self, other: &ValueBase) -> bool;
}
