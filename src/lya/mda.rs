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
    type Use = u32;
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
    Lam(Name::Decl, Ty, Box<Lam<Name, Ty, Ext>>),
    App(Box<Lam<Name, Ty, Ext>>, Box<Lam<Name, Ty, Ext>>),
    Ext(Ext),
}

impl<Name: VarName, Ext> Lam<Name, (), Ext> {}

impl<Name: VarName, Ty, Ext> Lam<Name, Ty, Ext> {
    pub fn lam(v: Name::Decl, ty: Ty, body: impl Into<Box<Lam<Name, Ty, Ext>>>) -> Self {
        Lam::Lam(v, ty, body.into())
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
    ctx: Vec<S>,
    vs: HashMap<S, u32>,
    depth: u32,
    not_found: HashSet<S>,
}

impl<K: Hash + Eq + Clone> DeBrujinCtx<K> {
    fn push(&mut self, k: K) {
        self.ctx.push(k.clone());
        self.vs.insert(k, self.depth);
        self.depth += 1;
    }

    fn pop(&mut self) {
        self.ctx.pop();
        self.depth -= 1;
    }

    fn to_debrujin<Ty, Ext>(
        &mut self,
        lam: Lam<Named<K>, Ty, Ext>,
    ) -> Option<Lam<DeBrujin, Ty, Ext>> {
        match lam {
            Lam::Var(k) => {
                if let Some(&d) = self.vs.get(&k) {
                    Some(Lam::Var(d))
                } else {
                    self.not_found.insert(k);
                    None
                }
            }
            Lam::Lam(k, ty, body) => {
                self.push(k.clone());
                let body = self.to_debrujin(*body)?;
                self.pop();
                Some(Lam::Lam((), ty, body.into()))
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
