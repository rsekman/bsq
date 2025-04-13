use crate::val::Val;

pub type Error = String;

pub(crate) fn type_error(expected: &str, actual: &Val) -> Error {
    format!("TypeError: expected {expected}, got {}", actual.get_type())
}
