use std::marker::PhantomData;

use super::functors::{Applicative, Layer, Pure};

struct Opt;

impl Layer for Opt {
    type Base<A> = Option<A>;
}

impl Pure for Opt {
    fn pure<A>(a: A) -> Self::Base<A> {
        Some(a)
    }
}

impl Applicative for Opt {
    fn zip_with<A, B, C, F: FnMut(A, B) -> C>(
        fa: Self::Base<A>,
        fb: Self::Base<B>,
        mut f: F,
    ) -> Self::Base<C> {
        fa.zip(fb).map(|(a, b)| f(a, b))
    }
}

struct Res<E>(PhantomData<E>);

impl<E> Layer for Res<E> {
    type Base<A> = Result<A, E>;
}

impl<E> Pure for Res<E> {
    fn pure<A>(a: A) -> Self::Base<A> {
        Ok(a)
    }
}

impl<E> Applicative for Res<E> {
    fn zip_with<A, B, C, F: FnMut(A, B) -> C>(
        fa: Self::Base<A>,
        fb: Self::Base<B>,
        mut f: F,
    ) -> Self::Base<C> {
        fa.and_then(|a| fb.map(|b| f(a, b)))
    }
}

struct Vect;

impl Layer for Vect {
    type Base<A> = Vec<A>;
}

impl Pure for Vect {
    fn pure<A>(a: A) -> Self::Base<A> {
        vec![a]
    }
}

impl Applicative for Vect {
    fn zip_with<A, B, C, F: FnMut(A, B) -> C>(
        fa: Self::Base<A>,
        fb: Self::Base<B>,
        mut f: F,
    ) -> Self::Base<C> {
        fa.into_iter().zip(fb).map(|(a, b)| f(a, b)).collect()
    }
}
