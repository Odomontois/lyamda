use std::{borrow::Cow, sync::Arc};

use super::Term;

pub type Str = Cow<'static, str>;

pub enum Value {
    Point,
    Prim(Primitive),
    Rec(Record),
    Extern(Calculate),
    Append { left: Arc<Value>, right: Arc<Value> },
    Box { context: Arc<Value>, proceed: Term },
}

pub struct Calculate;

pub enum Primitive {
    Long(u64),
    Text(String),
}

pub struct Record {
    pub values: Vec<Value>,
    pub names: Arc<Vec<Str>>,
}



pub enum Type {
    Universe,
    Bool,
    Long,
    Text,
    Function { dom: Arc<Type>, codom: Arc<Type> },
    And { left: Arc<Type>, right: Arc<Type> },
}
