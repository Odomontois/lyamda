use std::{cell::RefCell, rc::Rc};

use derivative::Derivative;
use thiserror::Error;

use super::EvalValue;

#[derive(Derivative, Debug)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct Context<V> {
    vars: Rc<RefCell<Vec<(V, bool)>>>,
    arity: Rc<usize>,
}

impl<V: Clone> Context<V> {
    pub fn pushed(&self, v: V) -> Self {
        let r = self.vars.borrow();
        let vars = if *self.arity == r.len() {
            drop(r);
            let mut r = self.vars.borrow_mut();
            r.push((v, true));
            self.vars.clone()
        } else {
            let mut vars: Vec<_> = r.iter().take(*self.arity).cloned().collect();
            vars.push((v, true));
            Rc::new(RefCell::new(vars))
        };
        let arity = (*self.arity + 1).into();
        Self { vars, arity }
    }

    pub fn get(&self, i: usize) -> Result<V, ContextError> {
        let r = self.vars.borrow();
        r.get(*self.arity - i - 1)
            .map(|x| x.0.clone())
            .ok_or_else(|| ContextError::NotFound)
    }
}

impl<V> Drop for Context<V> {
    fn drop(&mut self) {
        if let Some(&mut arity) = Rc::get_mut(&mut self.arity) {
            if arity > 0 {
                if let Ok(mut r) = self.vars.try_borrow_mut() {
                    r[arity - 1].1 = false;
                    if arity == r.len() {
                        while r.last().map_or(false, |x| !x.1) {
                            r.pop();
                        }
                    }
                }
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

    println!("{}", size_of::<ContextError>());
}
