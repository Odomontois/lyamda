use super::functors::{CloneBase, Functor, Layer, RefFunctor};

pub struct FixBox<L: Layer>(Box<L::Base<FixBox<L>>>);

impl<L: CloneBase> Clone for FixBox<L> {
    fn clone(&self) -> Self {
        FixBox(L::clone(&self.0).into())
    }
}

impl<L: Layer> FixBox<L> {
    pub fn new(a: L::Base<FixBox<L>>) -> FixBox<L> {
        FixBox(Box::new(a))
    }
}

impl<L: Functor> FixBox<L> {
    pub fn fold<R: 'static, F>(self, mut f: F) -> R
    where
        F: FnMut(L::Base<R>) -> R,
    {
        self.fold_impl(&mut f)
    }

    fn fold_impl<R: 'static, F>(self, f: &mut F) -> R
    where
        F: FnMut(L::Base<R>) -> R,
    {
        let br: L::Base<R> = L::map(*self.0, |x| x.fold_impl(f));
        f(br)
    }
}

impl<L: RefFunctor> FixBox<L> {
    pub fn fold_ref<'a, 'f, R: 'static, F>(&'a self, mut f: F) -> R
    where
        F: for<'t> FnMut(&'t L::Base<R>) -> R + 'a,
    {
        self.fold_ref_impl(&mut f)
    }

    fn fold_ref_impl<'a, 'f, R: 'static, F>(&'a self, f: &'f mut F) -> R
    where
        F: for<'t> FnMut(&'t L::Base<R>) -> R + 'f,
    {
        let FixBox(b) = self;
        let b = &**b;
        let br = L::map_ref(b, |x| x.fold_ref_impl(f));
        f(&br)
    }
}

#[cfg(test)]
mod test {
    use std::marker::PhantomData;

    use super::{CloneBase, FixBox, Functor, Layer, RefFunctor};

    type ListLayer<A, B> = Option<(A, B)>;
    struct List<A>(PhantomData<A>);

    impl<I> Layer for List<I> {
        type Base<A> = ListLayer<I, A>;
    }
    impl<I> Functor for List<I> {
        fn map<A, B, F: FnMut(A) -> B>(base: ListLayer<I, A>, mut f: F) -> ListLayer<I, B> {
            let (i, a) = base?;
            let b = f(a);
            Some((i, b))
        }
    }

    impl<I: Clone> RefFunctor for List<I> {
        fn map_ref<A, B, F: for<'t> FnMut(&'t A) -> B>(
            base: &Option<(I, A)>,
            mut f: F,
        ) -> Option<(I, B)> {
            let (i, a) = base.as_ref()?;
            let b = f(a);
            Some((i.clone(), b))
        }
    }

    impl<I: Clone> CloneBase for List<I> {
        fn clone<A: Clone>(base: &Option<(I, A)>) -> Option<(I, A)> {
            let (i, a) = base.as_ref()?;
            Some((i.clone(), a.clone()))
        }
    }

    fn range(start: i32, end: i32) -> FixBox<List<i32>> {
        let mut l = FixBox::new(None);

        for i in (start..end).rev() {
            l = FixBox::new(Some((i, l)));
        }

        l
    }

    #[test]
    fn consuming() {
        let s: i32 = range(1, 6).fold(|x| x.map_or(0, |(x, y)| x + y));
        assert_eq!(s, 15);
    }

    #[test]
    fn non_consuming() {
        let s: i32 = range(1, 6).fold_ref(|x| x.map_or(0, |(x, y)| x + y));
        assert_eq!(s, 15);
    }
}
