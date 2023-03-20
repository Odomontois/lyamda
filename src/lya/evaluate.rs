use std::{borrow::Borrow, rc::Rc};

use derivative::Derivative;
use derive_more::From;
use thiserror::Error;

use super::{debrujin::DeBrujin, mda::Lam};
#[derive(Derivative, From)]
#[derivative(Clone(bound = "R: Clone"), Debug)]
pub enum EvalValue<R, E> {
    #[from]
    Value(R),
    Func(#[derivative(Debug = "ignore")] Rc<dyn Fn(EvalValue<R, E>) -> EvalResult<R, E>>),
}

impl<R, E> EvalValue<R, E> {
    pub fn func(f: impl Fn(EvalValue<R, E>) -> EvalResult<R, E> + 'static) -> EvalValue<R, E> {
        Self::Func(Rc::new(f))
    }

    pub fn as_value(self) -> Result<R, EvalError<E>> {
        match self {
            EvalValue::Value(x) => Ok(x),
            EvalValue::Func(_) => Err(EvalError::NotAValue),
        }
    }

    pub fn as_func<'b>(
        &'b self,
    ) -> Result<&'b dyn Fn(EvalValue<R, E>) -> EvalResult<R, E>, EvalError<E>> {
        match self {
            EvalValue::Value(_) => Err(EvalError::NotAFunction),
            EvalValue::Func(f) => Ok(f.as_ref()),
        }
    }
}

#[derive(Error, Debug)]
pub enum EvalError<E> {
    Error(#[from] E),
    NotAFunction,
    NotAValue,
    NotFound(usize),
}

pub trait Evaluate {
    type Value: Clone + 'static;
    type Error: 'static;

    fn eval<'a>(
        &'a self,
        ctx: EvalCtx<Self::Value, Self::Error>,
    ) -> EvalResult<Self::Value, Self::Error>;

    fn evaluate(&self) -> EvalResult<Self::Value, Self::Error> {
        self.eval(EvalCtx::default())
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct EvalCtx<R, E> {
    vars: Rc<Vec<EvalValue<R, E>>>,
}

impl<'a, R: Clone, E> EvalCtx<R, E> {
    fn pushed<'b>(&'b self, v: EvalValue<R, E>) -> EvalCtx<R, E> {
        let vars: &Vec<_> = self.vars.borrow();
        let mut vars = vars.clone();
        vars.push(v);
        let vars = Rc::new(vars);
        EvalCtx { vars }
    }
}

pub type EvalResult<R, E> = Result<EvalValue<R, E>, EvalError<E>>;

impl<Ext: Evaluate + 'static, Ty: 'static> Evaluate for Lam<DeBrujin, Ty, Ext> {
    type Value = Ext::Value;
    type Error = Ext::Error;
    fn eval<'a>(
        &'a self,
        ctx: EvalCtx<Self::Value, Self::Error>,
    ) -> EvalResult<Self::Value, Self::Error> {
        match self {
            Lam::Var(i) => ctx
                .vars
                .get(*i)
                .cloned()
                .ok_or_else(|| EvalError::NotFound(*i)),
            Lam::Abs(_, _, exp) => {
                let exp: Rc<Lam<DeBrujin, Ty, Ext>> = exp.clone();
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

#[cfg(test)]
mod test {
    use crate::lya::{
        mda::{Lam, Named},
        uints::{Op, UIntExt},
    };
    use Op::*;
    use UIntExt::*;

    type IntLam = Lam<Named<&'static str>, (), UIntExt>;

    #[test]
    fn num1() {
        let l: IntLam = UIntExt::Num(1).into();
        let res = l.evaluate();
        println!("{res:?}");
    }

    #[test]
    fn num1_half() {
        let l: IntLam = Lam::app(Op(Add).into(), Num(1).into()).into();
        let res = l.evaluate();
        println!("{res:?}");
    }

    #[test]
    fn num2() {
        let l: IntLam = Lam::app(Lam::app(Op(Add).into(), Num(1).into()), Num(2).into()).into();
        let res = l.evaluate();
        println!("{res:?}");
    }
}
