use std::{collections::HashSet, hash::Hash, marker::PhantomData, rc::Rc};

use super::{
    debrujin::{DeBrujin, DeBrujinCtx},
    evaluate::{EvalError, EvalValue, Evaluate},
};
use derive_more::From;
use thiserror::Error;
pub trait VarName {
    type Decl;
    type Use;
}

pub struct Named<K>(PhantomData<K>);

impl<S> VarName for Named<S> {
    type Decl = S;
    type Use = S;
}

pub enum NoExt {}

#[derive(Debug, Clone, PartialEq, Eq, Hash, From)]
pub enum Lam<Name: VarName, Ty, Ext> {
    Var(Name::Use),
    Abs(Name::Decl, Ty, Rc<Lam<Name, Ty, Ext>>),
    App(Rc<Lam<Name, Ty, Ext>>, Rc<Lam<Name, Ty, Ext>>),
    #[from]
    Ext(Ext),
}

impl<Name: VarName, Ext> Lam<Name, (), Ext> {}

impl<Name: VarName, Ty, Ext> Lam<Name, Ty, Ext> {
    pub fn lam(v: Name::Decl, ty: Ty, body: impl Into<Rc<Lam<Name, Ty, Ext>>>) -> Self {
        Lam::Abs(v, ty, body.into())
    }

    pub fn dlam(v: Name::Decl, body: impl Into<Rc<Lam<Name, Ty, Ext>>>) -> Lam<Name, Ty, Ext>
    where
        Ty: Default,
    {
        Self::lam(v, Ty::default(), body)
    }

    pub fn app(
        f: Lam<Name, Ty, Ext>,
        arg: Lam<Name, Ty, Ext>,
    ) -> Self {
        Lam::App(f.into(), arg.into())
    }
}

#[derive(Error, Debug)]
pub enum EvalRunError<K, E> {
    UnknownVar(#[from] HashSet<K>),
    Eval(#[from] EvalError<E>),
}

pub type LamEvalRes<K, Ext> = Result<
    EvalValue<<Ext as Evaluate>::Value, <Ext as Evaluate>::Error>,
    EvalRunError<K, <Ext as Evaluate>::Error>,
>;

impl<Ty: Clone + 'static, Ext: Clone + 'static, K: Hash + Eq + Clone> Lam<Named<K>, Ty, Ext> {
    pub fn to_debrujin(&self) -> Result<Lam<DeBrujin, Ty, Ext>, HashSet<K>> {
        let mut ctx = DeBrujinCtx::default();
        ctx.to_debrujin(self).ok_or_else(|| ctx.not_found)
    }

    pub fn evaluate(&self) -> LamEvalRes<K, Ext>
    where
        Ext: Evaluate,
    {
        let d = self.to_debrujin()?;
        let d = d.evaluate()?;
        Ok(d)
    }
}

pub type UntypedLam<K> = Lam<Named<K>, (), NoExt>;
