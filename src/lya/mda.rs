use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    marker::PhantomData,
};

use derivative::Derivative;

pub trait VarName {
    type Decl;
    type Use;
}

pub struct DeBrujin;

impl VarName for DeBrujin {
    type Decl = ();
    type Use = usize;
}

pub struct Named<K>(PhantomData<K>);

impl<S> VarName for Named<S> {
    type Decl = S;
    type Use = S;
}

pub enum NoExt {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lam<Name: VarName, Ty, Ext> {
    Var(Name::Use),
    Abs(Name::Decl, Ty, Box<Lam<Name, Ty, Ext>>),
    App(Box<Lam<Name, Ty, Ext>>, Box<Lam<Name, Ty, Ext>>),
    Ext(Ext),
}

impl<Name: VarName, Ext> Lam<Name, (), Ext> {}

impl<Name: VarName, Ty, Ext> Lam<Name, Ty, Ext> {
    pub fn lam(v: Name::Decl, ty: Ty, body: impl Into<Box<Lam<Name, Ty, Ext>>>) -> Self {
        Lam::Abs(v, ty, body.into())
    }

    pub fn dlam(v: Name::Decl, body: impl Into<Box<Lam<Name, Ty, Ext>>>) -> Lam<Name, Ty, Ext>
    where
        Ty: Default,
    {
        Self::lam(v, Ty::default(), body)
    }

    pub fn app(
        f: impl Into<Box<Lam<Name, Ty, Ext>>>,
        arg: impl Into<Box<Lam<Name, Ty, Ext>>>,
    ) -> Self {
        Lam::App(f.into(), arg.into())
    }
}

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
struct DeBrujinCtx<S> {
    vars: Vec<S>,
    var_depth: HashMap<S, usize>,
    not_found: HashSet<S>,
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

    fn to_debrujin<Ty, Ext>(
        &mut self,
        lam: Lam<Named<K>, Ty, Ext>,
    ) -> Option<Lam<DeBrujin, Ty, Ext>> {
        match lam {
            Lam::Var(k) => self
                .var_depth
                .get(&k)
                .map(|&d| Lam::Var(self.vars.len() - d))
                .or_else(|| {
                    self.not_found.insert(k);
                    None
                }),
            Lam::Abs(k, ty, body) => {
                self.push(k.clone());
                let body = self.to_debrujin(*body)?;
                self.pop();
                Some(Lam::Abs((), ty, body.into()))
            }
            Lam::App(f, arg) => {
                let f = self.to_debrujin(*f);
                let arg = self.to_debrujin(*arg);
                Some(Lam::App(f?.into(), arg?.into()))
            }
            Lam::Ext(ext) => Some(Lam::Ext(ext)),
        }
    }
}

impl<Ty, Ext, K: Hash + Eq + Clone> Lam<Named<K>, Ty, Ext> {
    pub fn to_debrujin(self) -> Result<Lam<DeBrujin, Ty, Ext>, HashSet<K>> {
        let mut ctx = DeBrujinCtx::default();
        ctx.to_debrujin(self).ok_or_else(|| ctx.not_found)
    }
}



pub type UntypedLam<K> = Lam<Named<K>, (), NoExt>;

#[test]
fn lol() {
    let l: UntypedLam<&str> = Lam::dlam("x", Lam::Var("x"));
}
