use std::fmt::Display;

use super::{
    evaluate::{EvalCtx, EvalStepResult, EvalValue, Evaluate, ExtError},
    flavour::Untyped,
    mda::Lam,
};
use derive_more::From;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Op {
    Add,
    Mul,
    Sub,
    Div,
}

use ArithmeticError::*;
impl Op {
    fn apply(&self, x: u64, y: u64) -> Result<u64, ExtError<ArithmeticError>> {
        Ok(match self {
            Op::Add => x.checked_add(y).ok_or(Overflow),
            Op::Mul => x.checked_mul(y).ok_or(Overflow),
            Op::Sub => x.checked_sub(y).ok_or(Overflow),
            Op::Div => {
                if y == 0 {
                    Err(DivideByZero)
                } else {
                    x.checked_div(y).ok_or(Overflow)
                }
            }
        }?)
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Mul => write!(f, "*"),
            Op::Sub => write!(f, "-"),
            Op::Div => write!(f, "/"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, From)]
pub enum UIntExt {
    Num(u64),
    Op(Op),
}

impl<K> Into<Lam<Untyped<K, UIntExt>>> for UIntExt {
    fn into(self) -> Lam<Untyped<K, UIntExt>> {
        Lam::Ext(self)
    }
}

impl Display for UIntExt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UIntExt::Num(x) => write!(f, "{x}"),
            UIntExt::Op(op) => write!(f, "{op}"),
        }
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ArithmeticError {
    #[error("division by zero")]
    DivideByZero,
    #[error("operation overflow")]
    Overflow,
    #[error("argument was not a number")]
    NotANumber,
}

impl Evaluate for UIntExt {
    type Error = ArithmeticError;
    type Value = u64;

    fn eval_step<'a>(
        &'a self,
        _ctx: EvalCtx<Self::Value, Self::Error>,
    ) -> EvalStepResult<Self::Value, Self::Error> {
        Ok(match *self {
            UIntExt::Num(x) => EvalValue::Value(x).into(),
            UIntExt::Op(op) => EvalValue::val_func(move |x: EvalValue<Self::Value, _>| {
                let x = x.as_value()?;

                Ok(EvalValue::val_func(move |y: EvalValue<Self::Value, _>| {
                    let y = y.as_value()?;
                    let z = op.apply(x, y)?;
                    Ok(EvalValue::Value(z))
                }))
            })
            .into(),
        })
    }
}
