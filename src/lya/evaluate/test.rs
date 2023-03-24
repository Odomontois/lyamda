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

fn run_big_mul(m: impl Fn(&Church<IntLam>) -> &IntLam) {
    let ch = church();
    let ten = (ch.from_u64)(10);
    let hundred = m(&ch).appc(&ten).appc(&ten);
    let thousand = m(&ch).appc(&hundred).appc(&ten);
    let ten_thousands = m(&ch).appc(&thousand).appc(&ten);

    let l = (ch.to_uint)(m(&ch).appc(&ten_thousands).appc(&thousand));
    let res = l.evaluate_to_value().unwrap();
    assert_eq!(res, 10_000_000);
}

#[ignore]
#[test]
fn big_mul() {
    run_big_mul(|c| &c.mul)
}

#[ignore]
#[test]
fn big_alt_mul() {
    run_big_mul(|c| &c.alt_mul)
}
