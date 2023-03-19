use std::{borrow::Borrow, rc::Rc};

use derivative::Derivative;

use crate::lya::mda::{DeBrujin, Lam};
#[derive(Derivative)]
#[derivative(Clone(bound = "R: Clone"))]
pub enum EvalValue<'a, R, E> {
    Value(R),
    Func(Rc<dyn Fn(EvalValue<'a, R, E>) -> EvalResult<R, E> + 'a>),
}

impl <A, E> From<A> for EvalValue<'_, A, E> {
    fn from(x: A) -> Self {
        EvalValue::Value(x)
    }
}

impl<'a, R, E> EvalValue<'a, R, E> {
    pub fn func(f: impl Fn(EvalValue<'a, R, E>) -> EvalResult<R, E> + 'a) -> EvalValue<'a, R, E> {
        Self::Func(Rc::new(f))
    }

    pub fn as_value(self) -> Result<R, EvalError<E>> {
        match self {
            EvalValue::Value(x) => Ok(x),
            EvalValue::Func(_) => Err(EvalError::NotAValue),
        }
    }

    pub fn as_func<'b: 'a>(
        &'b self,
    ) -> Result<&'b dyn Fn(EvalValue<'a, R, E>) -> EvalResult<R, E>, EvalError<E>> {
        match self {
            EvalValue::Value(_) => Err(EvalError::NotAFunction),
            EvalValue::Func(f) => Ok(f.as_ref()),
        }
    }
}

pub enum EvalError<E> {
    Error(E),
    NotAFunction,
    NotAValue,
    NotFound(usize),
}

impl<E> From<E> for EvalError<E> {
    fn from(e: E) -> Self {
        EvalError::Error(e)
    }
}

pub trait Evaluate {
    type Value: Clone;
    type Error;
    fn eval<'a>(
        &'a self,
        ctx: EvalCtx<'a, Self::Value, Self::Error>,
    ) -> EvalResult<'a, Self::Value, Self::Error>;
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct EvalCtx<'a, R, E> {
    vars: Rc<Vec<EvalValue<'a, R, E>>>,
}

impl<'a, R: Clone, E> EvalCtx<'a, R, E> {
    fn pushed<'b>(&'b self, v: EvalValue<'a, R, E>) -> EvalCtx<'a, R, E> {
        let vars: &Vec<_> = self.vars.borrow();
        let mut vars = vars.clone();
        vars.push(v);
        let vars = Rc::new(vars);
        EvalCtx { vars }
    }
}

pub type EvalResult<'a, R, E> = Result<EvalValue<'a, R, E>, EvalError<E>>;

impl<Ext: Evaluate, Ty> Evaluate for Lam<DeBrujin, Ty, Ext> {
    type Value = Ext::Value;
    type Error = Ext::Error;
    fn eval<'a>(
        &'a self,
        ctx: EvalCtx<'a, Self::Value, Self::Error>,
    ) -> EvalResult<'a, Self::Value, Self::Error> {
        match self {
            Lam::Var(i) => ctx
                .vars
                .get(*i)
                .cloned()
                .ok_or_else(|| EvalError::NotFound(*i)),
            Lam::Abs((), _, exp) => {
                let exp: &'a Box<Lam<DeBrujin, Ty, Ext>> = exp;
                let ctx = ctx.clone();
                Ok(EvalValue::Func(Rc::new(move |v| exp.eval(ctx.pushed(v)))))
            }

            Lam::App(f, a) => {
                let v = (*a).eval(ctx.clone())?;
                match (*f).eval(ctx)? {
                    EvalValue::Value(_) => Err(EvalError::NotAFunction),
                    EvalValue::Func(f) => (*f)(v),
                }
            }
            Lam::Ext(e) => e.eval(ctx),
        }
    }
}
