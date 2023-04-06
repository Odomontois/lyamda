use std::marker::PhantomData;

use crate::fix::functors::{Layer, Functor, RefFunctor};

pub enum LamName<K, A> {
    Var(K),
    Abs(K, A),
    App(A, A),
}

pub struct Lam<K>(PhantomData<K>);

impl<K> Layer for Lam<K> {
    type Base<A> = LamName<K, A>;
}

impl<K> Functor for Lam<K> {
    fn map<A, B, F: FnMut(A) -> B>(base: Self::Base<A>, mut f: F) -> Self::Base<B> {
        match base {
            LamName::Var(k) => LamName::Var(k),
            LamName::Abs(k, a) => LamName::Abs(k, f(a)),
            LamName::App(a, b) => LamName::App(f(a), f(b)),
        }
    }
}

impl<K: Clone> RefFunctor for Lam<K> {
    fn map_ref<A, B, F: for<'t> FnMut(&'t A) -> B>(
        base: &Self::Base<A>,
        mut f: F,
    ) -> Self::Base<B> {
        match base {
            LamName::Var(k) => LamName::Var(k.clone()),
            LamName::Abs(k, a) => LamName::Abs(k.clone(), f(a)),
            LamName::App(a, b) => LamName::App(f(a), f(b)),
        }
    }
}
