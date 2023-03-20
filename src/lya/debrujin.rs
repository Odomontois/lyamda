use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
};

use derivative::Derivative;
use thiserror::Error;

use super::mda::{Lam, Named, Star, VarName};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeBrujin {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Use(pub usize);

impl Display for Use {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let i = self.0;
        write!(f, "[{i}]")
    }
}

impl VarName for DeBrujin {
    type Decl = Star;
    type Use = Use;
}

#[derive(Derivative)]
#[derivative(Default(bound = ""))]

pub struct DeBrujinCtx<S> {
    vars: Vec<S>,
    var_depth: HashMap<S, usize>,
    not_found: HashSet<S>,
}

#[derive(Debug, Error)]
pub struct NotFoundError<S>(Vec<S>);

impl<K: Hash + Eq + Clone> DeBrujinCtx<K> {
    fn push(&mut self, k: K) {
        self.var_depth.insert(k.clone(), self.vars.len());
        self.vars.push(k);
    }

    fn pop(&mut self) {
        if let Some(k) = self.vars.pop() {
            self.var_depth.remove(&k);
        }
    }

    pub fn error(self) -> NotFoundError<K> {
        NotFoundError(self.not_found.into_iter().collect())
    }

    pub fn to_debrujin<Ty: Clone, Ext: Clone>(
        &mut self,
        lam: &Lam<Named<K>, Ty, Ext>,
    ) -> Option<Lam<DeBrujin, Ty, Ext>> {
        match lam {
            Lam::Var(k) => self
                .var_depth
                .get(&k)
                .map(|&d| Lam::Var(Use(self.vars.len() - d - 1)))
                .or_else(|| {
                    self.not_found.insert(k.clone());
                    None
                }),
            Lam::Abs(k, ty, body) => {
                self.push(k.clone());
                let body = self.to_debrujin(&*body)?;
                self.pop();
                Some(Lam::Abs(Star(), ty.clone(), body.into()))
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
