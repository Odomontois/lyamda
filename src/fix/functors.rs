use std::marker::PhantomData;

use frunk::{coproduct::CNil, Coproduct};

pub trait Layer {
    type Base<A>;
}

impl Layer for CNil {
    type Base<A> = CNil;
}

impl<H: Layer, T: Layer> Layer for Coproduct<H, T> {
    type Base<A> = Coproduct<H::Base<A>, T::Base<A>>;
}

pub trait CloneBase: Layer {
    fn clone<A: Clone>(base: &Self::Base<A>) -> Self::Base<A>;
}

impl CloneBase for CNil {
    fn clone<A: Clone>(base: &CNil) -> Self::Base<A> {
        match *base {}
    }
}

impl<H: CloneBase, T: CloneBase> CloneBase for Coproduct<H, T> {
    fn clone<A: Clone>(base: &Self::Base<A>) -> Self::Base<A> {
        match base {
            Coproduct::Inl(h) => Coproduct::Inl(H::clone(h)),
            Coproduct::Inr(t) => Coproduct::Inr(T::clone(t)),
        }
    }
}

pub trait Functor: Layer {
    fn map<A, B, F: FnMut(A) -> B>(base: Self::Base<A>, f: F) -> Self::Base<B>;
}

impl Functor for CNil {
    fn map<A, B, F: FnMut(A) -> B>(base: CNil, _f: F) -> Self::Base<B> {
        match base {}
    }
}

impl<H: Functor, T: Functor> Functor for Coproduct<H, T> {
    fn map<A, B, F: FnMut(A) -> B>(base: Self::Base<A>, mut f: F) -> Self::Base<B> {
        match base {
            Coproduct::Inl(h) => Coproduct::Inl(H::map(h, &mut f)),
            Coproduct::Inr(t) => Coproduct::Inr(T::map(t, &mut f)),
        }
    }
}

pub trait RefFunctor: Layer {
    fn map_ref<A, B, F: for<'t> FnMut(&'t A) -> B>(base: &Self::Base<A>, f: F) -> Self::Base<B>;
}

impl RefFunctor for CNil {
    fn map_ref<A, B, F: for<'t> FnMut(&'t A) -> B>(base: &CNil, _f: F) -> Self::Base<B> {
        match *base {}
    }
}

impl<H: RefFunctor, T: RefFunctor> RefFunctor for Coproduct<H, T> {
    fn map_ref<A, B, F: for<'t> FnMut(&'t A) -> B>(
        base: &Self::Base<A>,
        mut f: F,
    ) -> Self::Base<B> {
        match base {
            Coproduct::Inl(h) => Coproduct::Inl(H::map_ref(h, &mut f)),
            Coproduct::Inr(t) => Coproduct::Inr(T::map_ref(t, &mut f)),
        }
    }
}
pub trait Pure: Layer {
    fn pure<A>(a: A) -> Self::Base<A>;
}

pub trait Applicative: Functor + Pure {
    fn zip_with<A, B, C, F: FnMut(A, B) -> C>(
        fa: Self::Base<A>,
        fb: Self::Base<B>,
        f: F,
    ) -> Self::Base<C>;
}

impl<L: Applicative> Functor for L {
    fn map<A, B, F: FnMut(A) -> B>(base: Self::Base<A>, mut f: F) -> Self::Base<B> {
        Self::zip_with(base, Self::pure(()), |a, _| f(a))
    }
}

pub trait RefApplicative: RefFunctor + Pure {
    fn zip_with_ref<A, B, C, F: for<'t> FnMut(&'t A, &'t B) -> C>(
        fa: &Self::Base<A>,
        fb: &Self::Base<B>,
        f: F,
    ) -> Self::Base<C>;
}

impl<L: RefApplicative> RefFunctor for L {
    fn map_ref<A, B, F: for<'t> FnMut(&'t A) -> B>(
        base: &Self::Base<A>,
        mut f: F,
    ) -> Self::Base<B> {
        Self::zip_with_ref(base, &Self::pure(()), |a, _| f(a))
    }
}

pub trait Traverse: Functor {
    fn traverse<A, B, T: Applicative, F: FnMut(A) -> T::Base<B>>(
        base: Self::Base<A>,
        f: F,
    ) -> T::Base<Self::Base<B>>;

    fn traverse_hint<A, B, T: Applicative, F: FnMut(A) -> T::Base<B>>(
        base: Self::Base<A>,
        #[allow(unused_variables)] hint: T,
        f: F,
    ) -> T::Base<Self::Base<B>> {
        Self::traverse::<_, _, T, _>(base, f)
    }

    fn traverse_phantom<A, B, T: Applicative, F: FnMut(A) -> T::Base<B>>(
        base: Self::Base<A>,
        #[allow(unused_variables)] hint: PhantomData<T>,
        f: F,
    ) -> T::Base<Self::Base<B>> {
        Self::traverse::<_, _, T, _>(base, f)
    }
}

pub trait RefTraverse: RefFunctor {
    fn traverse_ref<A, B, T: RefApplicative, F: for<'t> FnMut(&'t A) -> T::Base<B>>(
        base: &Self::Base<A>,
        f: F,
    ) -> T::Base<Self::Base<B>>;

    fn traverse_ref_hint<A, B, T: RefApplicative, F: for<'t> FnMut(&'t A) -> T::Base<B>>(
        base: &Self::Base<A>,
        #[allow(unused_variables)] hint: T,
        f: F,
    ) -> T::Base<Self::Base<B>> {
        Self::traverse_ref::<_, _, T, _>(base, f)
    }

    fn traverse_ref_phantom<A, B, T: RefApplicative, F: for<'t> FnMut(&'t A) -> T::Base<B>>(
        base: &Self::Base<A>,
        #[allow(unused_variables)] hint: PhantomData<T>,
        f: F,
    ) -> T::Base<Self::Base<B>> {
        Self::traverse_ref::<_, _, T, _>(base, f)
    }
}
