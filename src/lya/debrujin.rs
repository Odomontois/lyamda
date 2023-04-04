use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    marker::PhantomData,
};

use derivative::Derivative;
use thiserror::Error;

use super::{
    flavour::Flavour,
    mda::{Lam, Star},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeBrujin<Term>(PhantomData<Term>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Use(pub usize);

impl Display for Use {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let i = self.0;
        write!(f, "[{i}]")
    }
}

impl<Ext> Flavour for DeBrujin<Ext> {
    type DeclareName = Star;
    type UseName = Use;
    type Term = Ext;
    type Type = ();
}

#[derive(Derivative)]
#[derivative(Default(bound = ""))]

pub struct DeBrujinCtx<S> {
    vars: Vec<S>,
    var_depth: HashMap<S, Vec<usize>>,
    not_found: HashSet<S>,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub struct NotFoundError<S>(Vec<S>);

impl<K: Hash + Eq + Clone> DeBrujinCtx<K> {
    fn push(&mut self, k: K) {
        self.var_depth
            .entry(k.clone())
            .or_insert(vec![])
            .push(self.vars.len());
        self.vars.push(k);
    }

    fn get(&self, k: &K) -> Option<usize> {
        let v = self.var_depth.get(k)?;
        v.last().copied()
    }

    fn pop(&mut self) -> Option<usize> {
        let k = self.vars.pop()?;
        let v = self.var_depth.get_mut(&k)?;
        v.pop()
    }

    pub fn error(self) -> NotFoundError<K> {
        NotFoundError(self.not_found.into_iter().collect())
    }

    pub fn to_debrujin<F: Flavour<UseName = K, DeclareName = K>>(
        &mut self,
        lam: &Lam<F>,
    ) -> Option<Lam<DeBrujin<F::Term>>>
    where
        F::Term: Clone,
    {
        match lam {
            Lam::Var(k) => self
                .get(&k)
                .map(|d| Lam::Var(Use(self.vars.len() - d - 1)))
                .or_else(|| {
                    self.not_found.insert(k.clone());
                    None
                }),
            Lam::Abs(k, ty, body) => {
                self.push(k.clone());
                let body = self.to_debrujin(&*body)?;
                self.pop();
                Some(Lam::Abs(Star(), (), body.into()))
            }
            Lam::App(f, arg) => {
                let f = self.to_debrujin(&*f);
                let arg = self.to_debrujin(&*arg);
                Some(Lam::App(f?.into(), arg?.into()))
            }
            Lam::Ext(ext) => Some(Lam::Ext(ext.clone())),
        }
    }
}
