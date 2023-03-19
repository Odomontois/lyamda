use super::evaluate::{EvalCtx, EvalResult, EvalValue, Evaluate};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Op {
    Add,
    Mul,
    Sub,
    Div,
}

impl Op {
    fn apply(&self, x: u64, y: u64) -> Result<u64, ArithmeticError> {
        match self {
            Op::Add => x.checked_add(y).ok_or(ArithmeticError::Overflow),
            Op::Mul => x.checked_mul(y).ok_or(ArithmeticError::Overflow),
            Op::Sub => x.checked_sub(y).ok_or(ArithmeticError::Overflow),
            Op::Div => {
                if y == 0 {
                    Err(ArithmeticError::DivideByZero)
                } else {
                    x.checked_div(y).ok_or(ArithmeticError::Overflow)
                }
            }
        }
    }
}

pub enum UIntExt {
    Num(u64),
    Op(Op),
    Curried(Op, u64),
}

#[derive(Clone, Copy)]
pub enum UIntRes {
    Num(u64),
    Curried(Op, u64),
}

impl UIntRes {
    pub fn as_num(self) -> Result<u64, ArithmeticError> {
        match self {
            UIntRes::Num(x) => Ok(x),
            UIntRes::Curried(_, _) => Err(ArithmeticError::NotANumber),
        }
    }
}

pub enum ArithmeticError {
    DivideByZero,
    Overflow,
    NotANumber,
}

impl Evaluate for UIntExt {
    type Error = ArithmeticError;
    type Value = UIntRes;

    fn eval<'a>(
        &'a self,
        _ctx: EvalCtx<'a, Self::Value, Self::Error>,
    ) -> EvalResult<'a, Self::Value, Self::Error> {
        Ok(match self {
            UIntExt::Num(x) => EvalValue::Value(UIntRes::Num(*x)),
            UIntExt::Op(op) => EvalValue::func(|x: EvalValue<Self::Value, _>| {
                let x = x.as_value()?.as_num()?;
                Ok(UIntRes::Curried(*op, x).into())
            }),
            UIntExt::Curried(op, x) => EvalValue::func(|y: EvalValue<UIntRes, _>| {
                let y = y.as_value()?.as_num()?;
                let z = op.apply(*x, y)?;
                Ok(EvalValue::Value(UIntRes::Num(z)))
            }),
        })
    }
}
