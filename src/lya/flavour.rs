use std::{fmt::Display, hash::Hash, marker::PhantomData};

pub trait Flavour {
    type DeclareName;
    type UseName;
    type Type;
    type Term;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Untyped<K, X>(PhantomData<K>, PhantomData<X>);

impl<K, X> Flavour for Untyped<K, X> {
    type DeclareName = K;
    type UseName = K;
    type Term = X;
    type Type = ();
}



pub enum NoExt {}

impl Display for NoExt {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {}
    }
}
