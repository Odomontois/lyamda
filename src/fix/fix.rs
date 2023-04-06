pub trait Layer {
    type Base<A>;
}

pub trait CloneBase: Layer {
    fn clone<A: Clone>(base: &Self::Base<A>) -> Self::Base<A>;
}

pub trait LayerFunctor: Layer {
    fn map<A, B, F: FnMut(A) -> B>(base: Self::Base<A>, f: F) -> Self::Base<B>;
}

pub trait LayerCloneFunctor: CloneBase {
    fn map_clone<A: Clone, B, F: FnMut(A) -> B>(base: &Self::Base<A>, f: F) -> Self::Base<B>;
}

pub trait LayerRefFunctor: Layer {
    fn map_ref<A, B, F: for<'t> FnMut(&'t A) -> B>(base: &Self::Base<A>, f: F) -> Self::Base<B>;
}

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

impl<L: LayerFunctor> FixBox<L> {
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

impl<L: LayerCloneFunctor> FixBox<L>
where
    FixBox<L>: Clone,
{
    pub fn fold_clone<R: 'static, F>(self, mut f: F) -> R
    where
        F: FnMut(L::Base<R>) -> R,
    {
        self.fold_clone_impl(&mut f)
    }

    fn fold_clone_impl<R: 'static, F>(self, f: &mut F) -> R
    where
        F: FnMut(L::Base<R>) -> R,
    {
        let br: L::Base<R> = L::map_clone(&*self.0, |x| x.fold_clone_impl(f));
        f(br)
    }
}

impl<L: LayerRefFunctor> FixBox<L> {
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

    use super::{CloneBase, FixBox, Layer, LayerCloneFunctor, LayerFunctor, LayerRefFunctor};

    type ListLayer<A, B> = Option<(A, B)>;
    struct List<A>(PhantomData<A>);

    impl<I> Layer for List<I> {
        type Base<A> = ListLayer<I, A>;
    }
    impl<I> LayerFunctor for List<I> {
        fn map<A, B, F: FnMut(A) -> B>(base: ListLayer<I, A>, mut f: F) -> ListLayer<I, B> {
            let (i, a) = base?;
            let b = f(a);
            Some((i, b))
        }
    }

    impl<I: Clone> LayerRefFunctor for List<I> {
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

    impl<I: Clone> LayerCloneFunctor for List<I> {
        fn map_clone<A: Clone, B, F: FnMut(A) -> B>(
            base: &Self::Base<A>,
            mut f: F,
        ) -> Self::Base<B> {
            let (i, a) = base.as_ref()?;
            let b = f(a.clone());
            Some((i.clone(), b))
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

    #[test]
    fn cloning() {
        let s: i32 = range(1, 6).fold_clone(|x| x.map_or(0, |(x, y)| x + y));
        assert_eq!(s, 15);
    }
}
