use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lyamda::lya::{
    mda::{Lam, UntypedLamWith},
    uints::Op,
    uints::UIntExt,
};

fn eval_bench(c: &mut Criterion) {
    run_big_mul(|ch| &ch.mul, "big multiplicaion", c);
    run_big_mul(|ch| &ch.alt_mul, "big alt multiplicaion", c);
    run_big_mul(|ch| &ch.alt2_mul, "big alt 2 multiplicaion", c);
}

criterion_group!(benches, eval_bench);
criterion_main!(benches);

fn run_big_mul(m: impl Fn(&Church<IntLam>) -> &IntLam, name: &str, c: &mut Criterion) {
    let ch = church();
    let ten = (ch.from_u64)(10);
    let hundred = m(&ch).appc(&ten).appc(&ten);
    let thousand = m(&ch).appc(&hundred).appc(&ten);
    let _ten_thousands = m(&ch).appc(&thousand).appc(&ten);
    let hundred_thousands = (ch.to_uint)(m(&ch).appc(&thousand).appc(&hundred));

    c.bench_function(name, |b| {
        b.iter(|| {
            black_box(hundred_thousands.evaluate_to_value().unwrap());
        });
    });
}
use Op::Add;

use Lam::Var;
use UIntExt::*;

#[allow(unused)]
struct Church<A> {
    zero: A,
    succ: A,
    add: A,
    alt_add: A,
    mul: A,
    alt_mul: A,
    alt2_mul: A,
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

    let alt2_mul = dlam(
        "x",
        dlam(
            "y",
            Var("x").app(alt_add.clone().app(Var("y"))).app(zero.clone()),
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
        alt2_mul,
        pow,
        from_u64,
        to_uint,
    }
}
fn dlam(x: &'static str, body: IntLam) -> IntLam {
    Lam::dlam(x, body)
}

#[allow(unused)]
fn num(n: u64) -> IntLam {
    Num(n).into()
}
type IntLam = UntypedLamWith<&'static str, UIntExt>;

fn adder(n: u64) -> IntLam {
    Lam::app(Lam::Ext(Op(Add)), Lam::Ext(Num(n)))
}
