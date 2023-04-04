use std::{
    fmt::{Debug, Display},
    hash::Hash,
    rc::Rc,
};

use super::{
    debrujin::{DeBrujin, DeBrujinCtx, NotFoundError},
    evaluate::{EvalError, EvalValue, Evaluate},
    flavour::{CloneFlavour, Flavour, HashableNameFlavour, NoExt, Untyped},
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

impl<F: Flavour> Lam<F> {}

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
pub enum EvalRunError<K, E> {
    NotFound(#[from] NotFoundError<K>),
    Eval(#[from] EvalError<E>),
}

pub type LamRes<A, F> = Result<A, EvalRunError<<F as Flavour>::UseName, <F as Evaluate>::Error>>;

pub type LamResEval<F> = LamRes<EvalValue<<F as Evaluate>::Value, <F as Evaluate>::Error>, F>;

pub type LamResValue<F> = LamRes<<F as Evaluate>::Value, F>;

impl<F: Flavour> Lam<F> where 
    F::UseName: Hash + Eq + Clone, {
    pub fn to_debrujin(&self) -> Result<Lam<F>, NotFoundError<F::UseName>> {
        let mut ctx = DeBrujinCtx::default();
        ctx.to_debrujin(self).ok_or_else(|| ctx.error())
    }
}

impl<F: Flavour + Evaluate> Lam<F>
where
    F::UseName: Hash + Eq + Clone,
    F::DeclareName: Into<F::UseName>,
{
    pub fn evaluate(&self) -> LamResEval<F> {
        let d = self.to_debrujin()?;
        let d = d.evaluate()?;
        Ok(d)
    }

    pub fn evaluate_to_value(&self) -> LamResValue<F> {
        Ok(self.evaluate()?.as_value()?)
    }
}

pub type UntypedLam<K> = Lam<Untyped<K, NoExt>>;
pub type UntypedLamWith<K, Ext> = Lam<Untyped<K, Ext>>;
