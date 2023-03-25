use std::{borrow::Borrow, rc::Rc};

use derivative::Derivative;
use thiserror::Error;

use super::EvalValue;

#[derive(Derivative, Debug)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct Context<V> {
    vars: Rc<Vec<V>>,
}

impl<'a, V: Clone> Context<V> {
    pub fn pushed<'b>(&'b self, v: V) -> Self {
        let vars: &Vec<_> = self.vars.borrow();
        let mut vars = vars.clone();
        vars.push(v);
        let vars = Rc::new(vars);
        Self { vars }
    }

    pub fn get(&self, i: usize) -> Result<V, ContextError> {
        let vars: &Vec<_> = self.vars.borrow();
        vars.get(self.vars.len() - i - 1)
            .cloned()
            .ok_or_else(|| ContextError::NotFound)
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

    println!("{}", size_of::<EvalValue<u64, EvalError<ArithmeticError>>>());
}
