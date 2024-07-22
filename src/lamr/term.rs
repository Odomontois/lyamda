use std::{future::Future, ptr::slice_from_raw_parts, sync::Arc};

use super::{Primitive, Str, Type};

pub enum Term {
    Type(Type),
    Append { left: Arc<Term>, right: Arc<Term> },
    Prim(Primitive),
    Get { name: Str, index: usize },
    Set { name: Str, value: Arc<Term> },
    Box(Arc<Term>),
    Lambda { dom: Arc<Term>, body: Arc<Term> },
    Apply { func: Arc<Term>, args: Arc<Term> },
}

trait Lol {
    fn lol(&self) -> Box<dyn Future<Output = i32> + Send + Sync>;
}

impl Lol for i32 {
    fn lol(&self) -> Box<dyn Future<Output = i32> + Send + Sync> {
        let x = *self;
        Box::new(async move { x })
    }
}
