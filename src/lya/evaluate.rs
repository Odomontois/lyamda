use std::{borrow::Borrow, fmt::Debug, rc::Rc};

use derivative::Derivative;
use derive_more::From;
use thiserror::Error;

use super::{
    debrujin::{DeBrujin, Use},
    mda::Lam,
};

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub enum EvalFunc<R, E> {
    Immut(Rc<dyn Fn(EvalValue<R, E>) -> EvalStepResult<R, E>>),
}

impl<R, E> EvalFunc<R, E> {
    fn apply(&self, x: EvalValue<R, E>) -> EvalStepResult<R, E> {
        match self {
            EvalFunc::Immut(f) => f(x),
        }
    }
}

#[derive(Derivative, From)]
#[derivative(Clone(bound = "R: Clone"), Debug)]
pub enum EvalValue<R, E> {
    #[from]
    Value(R),
    Func(#[derivative(Debug = "ignore")] EvalFunc<R, E>),
}

impl<R, E> EvalValue<R, E> {
    pub fn func(f: impl Fn(EvalValue<R, E>) -> EvalStepResult<R, E> + 'static) -> EvalValue<R, E> {
        Self::Func(EvalFunc::Immut(Rc::new(f)))
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

#[derive(Error, Debug, PartialEq, Eq)]
pub enum EvalError<E> {
    Error(#[from] E),
    NotAFunction,
    NotAValue,
    NotFound(usize),
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
        let mut step = self.eval_step(ctx)?;
        loop {
            match step {
                EvalStep::Return(x) => return Ok(x),
                EvalStep::Later(f) => step = f()?,
            }
        }
    }

    fn evaluate(&self) -> EvalResult<Self::Value, Self::Error> {
        self.eval(EvalCtx::default())
    }
}

#[derive(Derivative, Debug)]
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

#[derive(From)]
pub enum EvalStep<R, E> {
    Later(Box<dyn FnOnce() -> EvalStepResult<R, E>>),
    #[from]
    Return(EvalValue<R, E>),
}

impl<R: 'static, E: 'static> EvalStep<R, E> {
    pub fn continue_with(
        self,
        f: impl FnOnce(EvalValue<R, E>) -> EvalStepResult<R, E> + 'static,
    ) -> EvalStep<R, E> {
        match self {
            EvalStep::Later(g) => EvalStep::Later(Box::new(move || Ok(g()?.continue_with(f)))),
            EvalStep::Return(v) => EvalStep::Later(Box::new(move || f(v))),
        }
    }
}

pub type EvalStepResult<R, E> = Result<EvalStep<R, E>, EvalError<E>>;
pub type EvalResult<R, E> = Result<EvalValue<R, E>, EvalError<E>>;

impl<Ext: Evaluate + 'static, Ty: 'static> Evaluate for Lam<DeBrujin, Ty, Ext> {
    type Value = Ext::Value;
    type Error = Ext::Error;

    fn eval_step<'a>(
        &'a self,
        ctx: EvalCtx<Self::Value, Self::Error>,
    ) -> EvalStepResult<Self::Value, Self::Error> {
        match self {
            Lam::Var(Use(i)) => Ok(ctx
                .vars
                .get(ctx.vars.len() - *i - 1)
                .cloned()
                .ok_or_else(|| EvalError::NotFound(*i))?
                .into()),

            Lam::Abs(_, _, exp) => {
                let exp: Rc<Lam<DeBrujin, Ty, Ext>> = exp.clone();
                let ctx = ctx.clone();
                Ok(EvalValue::func(move |v| exp.eval_step(ctx.pushed(v))).into())
            }

            Lam::App(f, a) => {
                let a = a.clone();
                Ok((*f)
                    .eval_step(ctx.clone())?
                    .continue_with(move |f| match f {
                        EvalValue::Value(_) => Err(EvalError::NotAFunction).into(),
                        EvalValue::Func(f) => {
                            Ok((*a).eval_step(ctx)?.continue_with(move |a| f.apply(a)))
                        }
                    }))
            }
            Lam::Ext(e) => Ok(e.eval(ctx)?.into()),
        }
    }
}

#[cfg(test)]
mod test {

    use crate::lya::{
        mda::{Lam, UntypedLamWith},
        uints::{Op, UIntExt},
    };

    use Lam::Var;
    use Op::*;
    use UIntExt::*;

    type IntLam = UntypedLamWith<&'static str, UIntExt>;

    #[test]
    fn num1() {
        let l: IntLam = Num(1).into();
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 1);
    }

    #[test]
    fn num1_half() {
        let l: IntLam = Lam::app(Op(Add).into(), Num(1).into()).into();
        assert!(l.evaluate().unwrap().is_func());
    }

    fn dlam(x: &'static str, body: IntLam) -> IntLam {
        Lam::dlam(x, body)
    }

    fn num(n: u64) -> IntLam {
        Num(n).into()
    }

    #[test]
    fn num2() {
        let l: IntLam = Lam::app(Lam::app(Op(Add).into(), Num(1).into()), Num(2).into()).into();
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 3);
    }

    fn adder(n: u64) -> IntLam {
        Lam::app(Lam::Ext(Op(Add)), Lam::Ext(Num(n)))
    }

    #[allow(unused)]
    struct Church<A> {
        zero: A,
        succ: A,
        add: A,
        alt_add: A,
        mul: A,
        alt_mul: A,
        pow: A,
        from_u64: Box<dyn Fn(u64) -> A>,
        to_uint: Box<dyn Fn(A) -> A>,
    }

    fn church() -> Church<IntLam> {
        let succ = dlam(
            "x",
            dlam(
                "s",
                dlam("z", Var("s").app(Var("x").app(Var("s")).app(Var("z")))),
            ),
        );
        let add = dlam(
            "x",
            dlam(
                "y",
                dlam(
                    "s",
                    dlam(
                        "z",
                        Var("x")
                            .app(Var("s"))
                            .app(Var("y").app(Var("s")).app(Var("z"))),
                    ),
                ),
            ),
        );
        let alt_add = dlam("x", dlam("y", Var("x").app(succ.clone()).app(Var("y"))));

        let from_u64 = Box::new(|n| {
            let mut l = Var("z");
            for _ in 0..n {
                l = Var("s").app(l);
            }
            dlam("s", dlam("z", l))
        });
        let zero = dlam("s", dlam("z", Var("z")));
        let pow = dlam("x", dlam("y", Var("y").app(Var("x"))));
        let mul = dlam(
            "x",
            dlam("y", dlam("s", Var("x").app(Var("y").app(Var("s"))))),
        );

        let alt_mul = dlam(
            "x",
            dlam(
                "y",
                Var("x").app(add.clone().app(Var("y"))).app(zero.clone()),
            ),
        );
        let to_uint = Box::new(|l: IntLam| l.app(adder(1)).app(Num(0).into()));
        Church {
            succ,
            add,
            alt_add,
            zero,
            mul,
            alt_mul,
            pow,
            from_u64,
            to_uint,
        }
    }

    #[test]
    fn increment_3_times() {
        let mut l: IntLam = Num(69).into();
        for _ in 0..3 {
            l = adder(1).app(l);
        }
        let l = l.evaluate_to_value().unwrap();
        assert_eq!(l, 72);
    }

    #[test]
    fn simple() {
        let f = dlam("x", dlam("y", Var("x"))).app(num(1)).app(num(2));
        let res = f.evaluate_to_value().unwrap();
        assert_eq!(res, 1);

        let f = dlam("x", dlam("y", Var("y"))).app(num(1)).app(num(2));
        let res = f.evaluate_to_value().unwrap();
        assert_eq!(res, 2);

        let id = dlam("x", Var("x"));
        let uu = dlam("x", id.clone().app(Var("x"))).app(id).app(num(42));
        let res = uu.evaluate_to_value().unwrap();
        assert_eq!(res, 42);
    }

    #[test]
    fn church_zero() {
        let ch = church();
        let res = ch
            .zero
            .app(adder(1))
            .app(num(0))
            .evaluate_to_value()
            .unwrap();
        assert_eq!(res, 0);
    }

    #[test]
    fn church_three() {
        let ch = church();
        let res = (ch.from_u64)(3)
            .app(adder(1))
            .app(num(0))
            .evaluate_to_value()
            .unwrap();
        assert_eq!(res, 3);
    }

    #[test]
    fn church_four() {
        let ch = church();
        let res = ch
            .succ
            .app((ch.from_u64)(3))
            .app(adder(1))
            .app(num(0))
            .evaluate_to_value()
            .unwrap();
        assert_eq!(res, 4)
    }

    #[test]
    fn church_add() {
        let ch = church();
        let l = (ch.to_uint)(ch.add.clone().app((ch.from_u64)(3)).app((ch.from_u64)(4)));
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 7);
        let l = (ch.to_uint)(ch.add.app((ch.from_u64)(333)).app((ch.from_u64)(444)));
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 777);
    }

    #[test]
    fn church_alt_add() {
        let ch = church();
        let l = (ch.to_uint)(
            ch.alt_add
                .clone()
                .app((ch.from_u64)(3))
                .app((ch.from_u64)(4)),
        );
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 7);

        let l = (ch.to_uint)(ch.alt_add.app((ch.from_u64)(333)).app((ch.from_u64)(444)));
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 777);
    }

    #[test]
    fn church_mul() {
        let ch = church();
        let l = (ch.to_uint)(ch.mul.clone().app((ch.from_u64)(3)).app((ch.from_u64)(4)));
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 12);

        let l = (ch.to_uint)(ch.mul.clone().app((ch.from_u64)(33)).app((ch.from_u64)(44)));
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 1452);
    }

    #[test]
    fn church_alt_mul() {
        let ch = church();
        let alt_mul = dlam(
            "x",
            dlam("y", Var("x").app(ch.add.app(Var("y"))).app(ch.zero)),
        );
        let l = (ch.to_uint)(alt_mul.clone().app((ch.from_u64)(3)).app((ch.from_u64)(4)));
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 12);

        let l = (ch.to_uint)(alt_mul.app((ch.from_u64)(33)).app((ch.from_u64)(44)));
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 1452);
    }

    #[test]
    fn church_big_mul() {
        let ch = church();

        let l = (ch.to_uint)(
            ch.alt_mul
                .clone()
                .app((ch.from_u64)(3))
                .app((ch.from_u64)(4)),
        );
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 12);
    }

    #[ignore]
    #[test]
    fn big_mul() {
        let ch = church();

        let l = (ch.to_uint)(
            ch.mul
                .clone()
                .app((ch.from_u64)(10000))
                .app((ch.from_u64)(1000)),
        );
        let res = l.evaluate_to_value().unwrap();
        assert_eq!(res, 10000000);
    }
}
