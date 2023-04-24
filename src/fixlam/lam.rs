use std::marker::PhantomData;

use crate::fix::functors::{Functor, Layer, RefFunctor};

pub enum LamName<I, K, A> {
    Var(I, K),
    Abs(K, A),
    App(A, A),
}

pub struct Lam<I, K>(PhantomData<(I, K)>);

impl<I, K> Layer for Lam<I, K> {
    type Base<A> = LamName<I, K, A>;
}

impl<I, K> Functor for Lam<I, K> {
    fn map<A, B, F: FnMut(A) -> B>(base: Self::Base<A>, mut f: F) -> Self::Base<B> {
        match base {
            LamName::Var(i, k) => LamName::Var(i, k),
            LamName::Abs(k, a) => LamName::Abs(k, f(a)),
            LamName::App(a, b) => LamName::App(f(a), f(b)),
        }
    }
}

impl<I: Clone, K: Clone> RefFunctor for Lam<I, K> {
    fn map_ref<A, B, F: for<'t> FnMut(&'t A) -> B>(
        base: &Self::Base<A>,
        mut f: F,
    ) -> Self::Base<B> {
        match base {
            LamName::Var(i, k) => LamName::Var(i.clone(), k.clone()),
            LamName::Abs(k, a) => LamName::Abs(k.clone(), f(a)),
            LamName::App(a, b) => LamName::App(f(a), f(b)),
        }
    }
}
