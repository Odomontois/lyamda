use std::rc::Rc;

use derivative::Derivative;

use super::{EvalStepResult, EvalError, EvalResult};
use derive_more::From;


#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct EvalFunc<R, E>(Rc<dyn Fn(EvalValue<R, E>) -> EvalStepResult<R, E>>);

impl<R, E> EvalFunc<R, E> {
    pub fn apply(&self, x: EvalValue<R, E>) -> EvalStepResult<R, E> {
        (self.0)(x)
    }
}

#[derive(Derivative, From)]
#[derivative(Clone(bound = "R: Clone"), Debug)]
pub enum EvalValue<R, E> {
    #[from]
    Value(R),
    Func(#[derivative(Debug = "ignore")] EvalFunc<R, E>),
}

impl<R, E> Default for EvalValue<R, E> {
    fn default() -> Self {
        EvalValue::Func(EvalFunc(Rc::new(|_| Err(EvalError::NotAFunction))))
    }
}

impl<R, E> EvalValue<R, E> {
    pub fn func(f: impl Fn(EvalValue<R, E>) -> EvalStepResult<R, E> + 'static) -> EvalValue<R, E> {
        Self::Func(EvalFunc(Rc::new(f)))
    }

    pub fn val_func(f: impl Fn(EvalValue<R, E>) -> EvalResult<R, E> + 'static) -> EvalValue<R, E> {
        Self::func(move |v| Ok(f(v)?.into()))
    }

    pub fn as_value(self) -> Result<R, EvalError<E>> {
        match self {
            EvalValue::Value(x) => Ok(x),
            EvalValue::Func(_) => Err(EvalError::NotAValue),
        }
    }

    pub fn is_func(&self) -> bool {
        match self {
            EvalValue::Value(_) => false,
            EvalValue::Func(_) => true,
        }
    }
}
