use crate::lya::{
    mda::{Lam, UntypedLamWith},
    uints::Op,
    uints::UIntExt,
};

type IntLam = UntypedLamWith<&'static str, UIntExt>;

use Op::Add;

use Lam::Var;
use UIntExt::*;


fn dlam(x: &'static str, body: IntLam) -> IntLam {
    Lam::dlam(x, body)
}

#[test]
fn kek() {
    let s = dlam(
        "f",
        dlam(
            "g",
            dlam("x", Var("f").app(Var("x")).app(Var("g").app(Var("x")))),
        ),
    );

    let k = dlam("x", dlam("y", Var("x")));
    println!("{:?}", s.app(k))
}