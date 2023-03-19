use std::{borrow::Borrow, rc::Rc};

use derivative::Derivative;

use crate::lya::mda::{DeBrujin, Lam};
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub enum EvalValue<'a, R, E> {
    Value(Rc<R>),
    Func(Rc<dyn Fn(EvalValue<'a, R, E>) -> EvalResult<R, E> + 'a>),
}

pub enum EvalError<E> {
    Error(E),
    NotAFunction,
    NotAValue,
    NotFound(usize),
}
trait Evaluate {
    type Value;
    type Error;
    fn eval<'a>(
        &'a self,
        ctx: EvalCtx<'a, Self::Value, Self::Error>,
    ) -> EvalResult<'a, Self::Value, Self::Error>;
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
struct EvalCtx<'a, R, E> {
    vars: Rc<Vec<EvalValue<'a, R, E>>>,
}

impl<'a, R, E> EvalCtx<'a, R, E> {
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
