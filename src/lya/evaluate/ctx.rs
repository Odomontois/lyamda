use std::{cell::RefCell, rc::Rc};

use derivative::Derivative;
use thiserror::Error;

use super::EvalValue;

#[derive(Derivative, Debug)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct Context<V> {
    vars: Rc<RefCell<Vec<V>>>,
    arity: usize,
}

impl<V: Clone> Context<V> {
    pub fn pushed(&self, v: V) -> Self {
        let r = self.vars.borrow();
        let vars = if self.arity == r.len() {
            drop(r);
            let mut r = self.vars.borrow_mut();
            r.push(v);
            self.vars.clone()
        } else {
            let mut vars: Vec<_> = r.iter().take(self.arity).cloned().collect();
            vars.push(v);
            Rc::new(RefCell::new(vars))
        };
        let arity = self.arity + 1;
        Self { vars, arity }
    }

    pub fn get(&self, i: usize) -> Result<V, ContextError> {
        let r = self.vars.borrow();
        r.get(self.arity - i - 1)
            .map(|x| x.clone())
            .ok_or_else(|| ContextError::NotFound)
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ContextError {
    #[error("variable not found")]
    NotFound,
}

pub type EvalCtx<R, E> = Context<EvalValue<R, E>>;

