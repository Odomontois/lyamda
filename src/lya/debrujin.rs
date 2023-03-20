use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use derivative::Derivative;

use super::mda::{Lam, Named, VarName};

pub enum DeBrujin {}

impl VarName for DeBrujin {
    type Decl = ();
    type Use = usize;
}

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
pub struct DeBrujinCtx<S> {
    vars: Vec<S>,
    var_depth: HashMap<S, usize>,
    pub not_found: HashSet<S>,
}

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

    pub fn to_debrujin<Ty: Clone, Ext: Clone>(
        &mut self,
        lam: &Lam<Named<K>, Ty, Ext>,
    ) -> Option<Lam<DeBrujin, Ty, Ext>> {
        match lam {
            Lam::Var(k) => self
                .var_depth
                .get(&k)
                .map(|&d| Lam::Var(self.vars.len() - d))
                .or_else(|| {
                    self.not_found.insert(k.clone());
                    None
                }),
            Lam::Abs(k, ty, body) => {
                self.push(k.clone());
                let body = self.to_debrujin(&*body)?;
                self.pop();
                Some(Lam::Abs((), ty.clone(), body.into()))
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
