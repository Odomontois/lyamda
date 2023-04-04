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

pub trait CloneFlavour: Flavour {
    type DeclareName: Clone;
    type UseName: Clone;
    type Type: Clone;
    type Term: Clone;
}

impl<F: Flavour> CloneFlavour for F
where
    F::DeclareName: Clone,
    F::UseName: Clone,
    F::Type: Clone,
    F::Term: Clone,
{
}

pub trait StaticFlavour: Flavour {
    type DeclareName: 'static;
    type UseName: 'static;
    type Type: 'static;
    type Term: 'static;
}

impl<F: Flavour> StaticFlavour for F
where
    F::DeclareName: 'static,
    F::UseName: 'static,
    F::Type: 'static,
    F::Term: 'static,
{
}

pub trait HashableNameFlavour: Flavour
where
    Self::UseName: Hash + Eq + Clone,
    Self::DeclareName: Into<Self::UseName>,
{
}

impl<F: Flavour> HashableNameFlavour for F
where
    F::UseName: Hash + Eq + Clone,
    F::DeclareName: Into<F::UseName>,
{
}

pub enum NoExt {}

impl Display for NoExt {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {}
    }
}
