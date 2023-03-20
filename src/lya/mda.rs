use std::{
    fmt::{Debug, Display},
    hash::Hash,
    marker::PhantomData,
    rc::Rc,
};

use super::{
    debrujin::{DeBrujin, DeBrujinCtx, NotFoundError},
    evaluate::{EvalError, EvalValue, Evaluate},
};
use derive_more::From;
use thiserror::Error;
pub trait VarName {
    type Decl;
    type Use;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Named<K>(PhantomData<K>);

impl<S> VarName for Named<S> {
    type Decl = S;
    type Use = S;
}

pub enum NoExt {}

impl Display for NoExt {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct Star();

impl Display for Star {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "*")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, From)]
pub enum Lam<Name: VarName, Ty, Ext> {
    Var(Name::Use),
    Abs(Name::Decl, Ty, Rc<Lam<Name, Ty, Ext>>),
    App(Rc<Lam<Name, Ty, Ext>>, Rc<Lam<Name, Ty, Ext>>),
    #[from]
    Ext(Ext),
}

impl<Name: VarName, Ty: Display, Ext: Display> Display for Lam<Name, Ty, Ext>
where
    Name::Decl: Display,
    Name::Use: Display,
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

impl<Name: VarName, Ext> Lam<Name, (), Ext> {}

impl<Name: VarName, Ty, Ext> Lam<Name, Ty, Ext> {
    pub fn lam(v: Name::Decl, ty: Ty, body: Lam<Name, Ty, Ext>) -> Self {
        Lam::Abs(v, ty, body.into())
    }

    pub fn dlam(v: Name::Decl, body: Lam<Name, Ty, Ext>) -> Lam<Name, Ty, Ext>
    where
        Ty: Default,
    {
        Self::lam(v, Ty::default(), body)
    }

    pub fn app(self, arg: Lam<Name, Ty, Ext>) -> Self {
        Lam::App(self.into(), arg.into())
    }
}

#[derive(Error, Debug)]
pub enum EvalRunError<K, E> {
    NotFound(#[from] NotFoundError<K>),
    Eval(#[from] EvalError<E>),
}

pub type LamRes<A, K, Ext> = Result<A, EvalRunError<K, <Ext as Evaluate>::Error>>;

pub type LamResEval<K, Ext> =
    LamRes<EvalValue<<Ext as Evaluate>::Value, <Ext as Evaluate>::Error>, K, Ext>;

pub type LamResValue<K, Ext> = LamRes<<Ext as Evaluate>::Value, K, Ext>;

impl<Ty: Clone + 'static, Ext: Clone + 'static, K: Hash + Eq + Clone> Lam<Named<K>, Ty, Ext> {
    pub fn to_debrujin(&self) -> Result<Lam<DeBrujin, Ty, Ext>, NotFoundError<K>> {
        let mut ctx = DeBrujinCtx::default();
        ctx.to_debrujin(self).ok_or_else(|| ctx.error())
    }
}

impl<
        Ty: Clone + Debug + 'static,
        Ext: Debug + Clone + Evaluate + 'static,
        K: Hash + Eq + Clone,
    > Lam<Named<K>, Ty, Ext>
where
    Ext::Value: Debug,
    Ext::Error: Debug,
{
    pub fn evaluate(&self) -> LamResEval<K, Ext> {
        let d = self.to_debrujin()?;
        let d = d.evaluate()?;
        Ok(d)
    }

    pub fn evaluate_to_value(&self) -> LamResValue<K, Ext> {
        Ok(self.evaluate()?.as_value()?)
    }
}

pub type UntypedLam<K> = Lam<Named<K>, Star, NoExt>;
pub type UntypedLamWith<K, Ext> = Lam<Named<K>, Star, Ext>;
