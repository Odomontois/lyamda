use std::rc::Rc;

use derivative::Derivative;

use crate::lya::mda::{DeBrujin, Lam};
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
enum EvalValue<R, E> {
    Value(Rc<R>),
    Func(Rc<dyn Fn(EvalValue<R, E>) -> EvalResult<R, E>>),
}

enum EvalError<E> {
    Error(E),
    NotAFunction,
    NotAValue,
    NotFound(usize),
}
trait Evaluate {
    type Value;
    type Error;
    fn eval(
        &self,
        ctx: &mut Vec<EvalValue<Self::Value, Self::Error>>,
    ) -> Result<EvalValue<Self::Value, Self::Error>, EvalError<Self::Error>>;
}

pub type EvalResult<R, E> = Result<EvalValue<R, E>, EvalError<E>>;

impl<Ext: Evaluate, Ty> Evaluate for Lam<DeBrujin, Ty, Ext> {
    type Value = Ext::Value;
    type Error = Ext::Error;
    fn eval(
        &self,
        ctx: &mut Vec<EvalValue<Self::Value, Self::Error>>,
    ) -> EvalResult<Self::Value, Self::Error> {
        match self {
            Lam::Var(i) => ctx.get(*i).cloned().ok_or_else(|| EvalError::NotFound(*i)),
            Lam::Abs(_, _, _) => todo!(),
            Lam::App(f, a) => {
                let v = (*a).eval(ctx)?;
                match (*f).eval(ctx)? {
                    EvalValue::Value(_) => Err(EvalError::NotAFunction),
                    EvalValue::Func(f) => (*f)(v),
                }
            }
            Lam::Ext(e) => e.eval(ctx),
        }
    }
}
