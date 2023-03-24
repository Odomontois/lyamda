use std::collections::VecDeque;

use derivative::Derivative;

pub use super::ctx::{ContextError, EvalCtx};

use super::{EvalError, EvalValue};

type Cont<V, E> = dyn FnOnce(V) -> Result<Step<V, E>, E>;

#[derive(Derivative)]
#[derivative(Default(bound = "V: Default"))]
pub struct Step<V, E> {
    focus: V,
    stack: Vec<Box<Cont<V, E>>>,
}

impl<V, E> From<V> for Step<V, E> {
    fn from(v: V) -> Self {
        Self {
            focus: v,
            stack: Vec::new(),
        }
    }
}

impl<V: 'static, E: 'static> Step<V, E> {
    pub fn continue_with(
        mut self,
        f: impl FnOnce(V) -> Result<Step<V, E>, E> + 'static,
    ) -> Step<V, E> {
        self.stack.push(Box::new(f));
        self
    }

    pub fn defer(f: impl FnOnce() -> Result<Step<V, E>, E> + 'static) -> Step<V, E>
    where
        V: Default,
    {
        Self {
            focus: V::default(),
            stack: vec![Box::new(|_| f())],
        }
    }

    pub fn run(self) -> Result<V, E> {
        let Self { mut focus, stack } = self;
        let mut actions: VecDeque<Box<Cont<V, E>>> = stack.into();
        loop {
            match actions.pop_front() {
                Some(action) => {
                    let step = action(focus)?;
                    focus = step.focus;
                    for action in step.stack.into_iter().rev() {
                        actions.push_front(action);
                    }
                }
                None => return Ok(focus),
            }
        }
    }
}

pub type EvalStep<R, E> = Step<EvalValue<R, E>, EvalError<E>>;
pub type EvalStepResult<R, E> = Result<EvalStep<R, E>, EvalError<E>>;
