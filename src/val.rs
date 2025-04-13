use std::rc::Rc;

use kuchiki::NodeRef;

use crate::error::Error;

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Nil(),
    Bool(bool),
    Float(f64),
    Int(isize),
    Str(String),
    Node(NodeRef),
    Vector(Vec<Val>),
}

impl Default for Val {
    fn default() -> Self {
        Val::Nil()
    }
}

pub(crate) type ValRef = Rc<Val>;

pub(crate) type ValResult = Result<ValRef, Error>;

impl Val {
    pub fn get_type(&self) -> &str {
        use Val::*;
        match self {
            Nil() => "None",
            Bool(_) => "Bool",
            Int(_) => "Int",
            Float(_) => "Float",
            Str(_) => "String",
            Node(_) => "Node",
            Vector(_) => "Vector",
        }
    }
}
