mod ctx;
mod step;
mod value;

use std::{fmt::Debug, rc::Rc};

use thiserror::Error;

pub use self::ctx::{ContextError, EvalCtx};
use self::step::EvalStep;
pub use self::step::EvalStepResult;
pub(crate) use self::value::EvalValue;

use super::{
    debrujin::{DeBrujin, Use},
    flavour::Flavour,
    mda::Lam,
};

#[derive(Error, Debug, PartialEq, Eq)]
#[error(transparent)]
pub struct ExtError<E>(#[from] E);

#[derive(Error, Debug, PartialEq, Eq)]
pub enum EvalError<E> {
    ExtError(#[from] ExtError<E>),
    NotAFunction,
    NotAValue,
    Context(#[from] ContextError),
}

pub trait Evaluate {
    type Value: Clone + 'static;
    type Error: 'static;

    fn eval_step<'a>(
        &'a self,
        ctx: EvalCtx<Self::Value, Self::Error>,
    ) -> EvalStepResult<Self::Value, Self::Error>;

    fn eval<'a>(
        &'a self,
        ctx: EvalCtx<Self::Value, Self::Error>,
    ) -> EvalResult<Self::Value, Self::Error> {
        let step = self.eval_step(ctx)?;
        step.run()
    }

    fn evaluate(&self) -> EvalResult<Self::Value, Self::Error> {
        self.eval(EvalCtx::default())
    }
}

pub type EvalResult<R, E> = Result<EvalValue<R, E>, EvalError<E>>;

impl<F: Evaluate + Flavour> Evaluate for Lam<F> {
    type Value = F::Value;
    type Error = F::Error;

    fn eval_step<'a>(
        &'a self,
        ctx: EvalCtx<Self::Value, Self::Error>,
    ) -> EvalStepResult<Self::Value, Self::Error> {
        match self {
            Lam::Var(Use(i)) => Ok(ctx.get(*i)?.into()),

            Lam::Abs(_, _, exp) => {
                let exp: Rc<Lam<F>> = exp.clone();
                let ctx = ctx.clone();
                Ok(EvalValue::func(move |v| exp.eval_step(ctx.pushed(v))).into())
            }

            Lam::App(f, a) => {
                let f = f.clone();
                let a = a.clone();
                Ok(EvalStep::defer(move || {
                    Ok(f.eval_step(ctx.clone())?.continue_with(move |f| match f {
                        EvalValue::Value(_) => Err(EvalError::NotAFunction).into(),
                        EvalValue::Func(f) => {
                            Ok((*a).eval_step(ctx)?.continue_with(move |a| f.apply(a)))
                        }
                    }))
                }))
            }
            Lam::Ext(e) => Ok(e.eval_step(ctx)?.into()),
        }
    }
}

#[cfg(test)]
mod test;
