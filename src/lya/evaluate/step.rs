use std::collections::VecDeque;

use derivative::Derivative;

pub use super::ctx::{ContextError, EvalCtx};

use super::{EvalError, EvalValue};

type Cont<'a, V, E> = Box<dyn FnOnce(V) -> Result<Step<'a, V, E>, E> + 'a>;

#[derive(Derivative)]
#[derivative(Default(bound = "V: Default"))]
pub struct Step<'a, V, E> {
    focus: V,
    stack: Vec<Cont<'a, V, E>>,
}

impl<'a, V, E> From<V> for Step<'a, V, E> {
    fn from(v: V) -> Self {
        Self {
            focus: v,
            stack: Vec::new(),
        }
    }
}

impl<'a, V, E> Step<'a, V, E> {
    pub fn continue_with(
        mut self,
        f: impl FnOnce(V) -> Result<Step<'a, V, E>, E> + 'a,
    ) -> Step<'a, V, E> {
        self.stack.push(Box::new(f));
        self
    }

    pub fn defer(f: impl FnOnce() -> Result<Step<'a, V, E>, E> + 'a) -> Step<'a, V, E>
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
        let mut actions: VecDeque<Cont<V, E>> = stack.into();
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

pub type EvalStep<R, E> = Step<'static, EvalValue<R, E>, EvalError<E>>;
pub type EvalStepResult<R, E> = Result<EvalStep<R, E>, EvalError<E>>;
