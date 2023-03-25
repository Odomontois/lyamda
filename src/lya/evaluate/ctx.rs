use std::{cell::RefCell, rc::Rc};

use derivative::Derivative;
use thiserror::Error;

use super::EvalValue;

#[derive(Derivative, Debug)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct Context<V> {
    vars: Rc<RefCell<Vec<Option<V>>>>,
    arity: Rc<usize>,
}

impl<'a, V: Clone> Context<V> {
    pub fn pushed<'b>(&'b self, v: V) -> Self {
        let r = self.vars.borrow();
        let mut vars = r.clone();
        vars.push(Some(v));
        let vars = Rc::new(RefCell::new(vars));
        let ix = Rc::new(&*self.arity + 1);
        Self { vars, arity: ix }
    }

    pub fn get(&self, i: usize) -> Result<V, ContextError> {
        let r = self.vars.borrow();
        r.get(r.len() - i - 1)
            .cloned()
            .flatten()
            .ok_or_else(|| ContextError::NotFound)
    }
}

impl<V> Drop for Context<V> {
    fn drop(&mut self) {
        if let Some(&mut arity) = Rc::get_mut(&mut self.arity) {
            if arity > 0 {
                let mut r = self.vars.borrow_mut();
                r[arity - 1] = None;
            }
        }
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ContextError {
    #[error("variable not found")]
    NotFound,
}

pub type EvalCtx<R, E> = Context<EvalValue<R, E>>;

#[test]
fn lol() {
    use crate::lya::{evaluate::EvalError, uints::ArithmeticError};
    use std::mem::size_of;

    println!(
        "{}",
        size_of::<EvalValue<u64, EvalError<ArithmeticError>>>()
    );
}
