#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Op {
    Add,
    Mul,
    Sub,
    Div,
}

use derive_more::From;
use thiserror::Error;
use ArithmeticError::*;
impl Op {
    fn apply(&self, x: i64, y: i64) -> Result<i64, ArithmeticError> {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, From)]
pub enum I64 {
    Num(i64),
    Op(Op),
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
