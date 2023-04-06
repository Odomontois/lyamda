pub trait Layer {
    type Base<A>;
}

pub trait LayerFunctor: Layer {
    fn map<A, B, F: FnMut(A) -> B>(base: Self::Base<A>, f: F) -> Self::Base<B>;
}

pub trait LayerRefFunctor: Layer {
    fn map_ref<'a, A: 'a, B: 'static, F: FnMut(&'a A) -> B + 'a>(
        base: &'a Self::Base<A>,
        f: F,
    ) -> Self::Base<B>;
}

pub struct FixBox<L: Layer>(Box<L::Base<FixBox<L>>>);

impl<L: Layer> FixBox<L> {
    pub fn new(a: L::Base<FixBox<L>>) -> FixBox<L> {
        FixBox(Box::new(a))
    }
}

// impl <L: Layer> From<L::Base<FixBox<L>>> for FixBox<L> {
//     fn from(a: L::Base<FixBox<L>>) -> FixBox<L> {
//         FixBox::new(a)
//     }
// }

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

impl<L: LayerRefFunctor> FixBox<L> {
    pub fn fold_ref<'a, R: 'static, F>(&'a self, f: &mut F) -> R
    where
        F: FnMut(&'a L::Base<R>) -> R + 'a,
    {
        let FixBox(b) = self;
        let b = &**b;
        // let br = L::map_ref(b, |x| x.fold_ref(f));
        todo!()
    }
}

#[cfg(test)]
mod test {
    use std::marker::PhantomData;

    use super::{FixBox, Layer, LayerFunctor};

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

    #[test]
    fn test() {
        let l: FixBox<List<i32>> =
            FixBox::new(Some((1i32, FixBox::new(Some((2, FixBox::new(None)))))));

        let s: i32 = l.fold(|x| x.map_or(0, |(x, y)| x + y));
        assert_eq!(s, 3);
    }
}
