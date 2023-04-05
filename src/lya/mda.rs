use std::{
    fmt::{Debug, Display},
    hash::Hash,
    rc::Rc,
};

use super::{
    debrujin::{DeBrujin, DeBrujinCtx, NotFoundError},
    evaluate::{EvalError, EvalValue, Evaluate},
    flavour::{Flavour, NoExt, Untyped},
};
use derive_more::From;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct Star();

impl Display for Star {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "*")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lam<F: Flavour> {
    Var(F::UseName),
    Abs(F::DeclareName, F::Type, Rc<Lam<F>>),
    App(Rc<Lam<F>>, Rc<Lam<F>>),
    Ext(F::Term),
}

impl<F: Flavour> Display for Lam<F>
where
    F::DeclareName: Display,
    F::UseName: Display,
    F::Type: Display,
    F::Term: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lam::Var(v) => write!(f, "{v}"),
            Lam::Abs(v, ty, body) => write!(f, "(λ{v}:{ty}. {body})"),
            Lam::App(fun, arg) => write!(f, "({fun} {arg})"),
            Lam::Ext(e) => write!(f, "{e}"),
        }
    }
}

impl<F: Flavour> Lam<F> {
    pub fn lam(v: F::DeclareName, ty: F::Type, body: Lam<F>) -> Self {
        Lam::Abs(v, ty, body.into())
    }

    pub fn dlam(v: F::DeclareName, body: Lam<F>) -> Lam<F>
    where
        F::Type: Default,
    {
        Self::lam(v, F::Type::default(), body)
    }

    pub fn app(self, arg: Lam<F>) -> Self {
        Lam::App(self.into(), arg.into())
    }

    pub fn appc(&self, arg: &Lam<F>) -> Self
    where
        Self: Clone,
    {
        self.clone().app(arg.clone())
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum DebEvalError<K, E> {
    NotFound(#[from] NotFoundError<K>),
    Eval(#[from] EvalError<E>),
}

pub type FlavourTerm<F> = <F as Flavour>::Term;
pub type FlavourName<F> = <F as Flavour>::UseName;
pub type FlavourValue<F> = <FlavourTerm<F> as Evaluate>::Value;
pub type FlavourError<F> = <FlavourTerm<F> as Evaluate>::Error;

pub type DebEvalRes<A, F> = Result<A, DebEvalError<FlavourName<F>, FlavourError<F>>>;

pub type DebEval<F> = DebEvalRes<EvalValue<FlavourValue<F>, FlavourError<F>>, F>;

pub type DebEvalValue<F> = DebEvalRes<FlavourValue<F>, F>;

impl<F: Flavour<UseName = K, DeclareName = K>, K: Hash + Eq + Clone> Lam<F>
where
    F::Term: Clone,
{
    pub fn to_debrujin(&self) -> Result<Lam<DeBrujin<F::Term>>, NotFoundError<K>> {
        let mut ctx = DeBrujinCtx::default();
        ctx.to_debrujin(self).ok_or_else(|| ctx.error())
    }
}

impl<F, K, T> Lam<F>
where
    F: Flavour<UseName = K, DeclareName = K, Term = T>,
    K: Hash + Eq + Clone,
    T: Evaluate + Clone + 'static,
{
    pub fn eval_deb(&self) -> DebEval<F> {
        let d = self.to_debrujin()?;
        let d = d.evaluate()?;
        Ok(d)
    }

    pub fn evaluate_to_value(&self) -> DebEvalValue<F> {
        Ok(self.eval_deb()?.as_value()?)
    }
}

pub type UntypedLam<K> = Lam<Untyped<K, NoExt>>;
pub type UntypedLamWith<K, Ext> = Lam<Untyped<K, Ext>>;
