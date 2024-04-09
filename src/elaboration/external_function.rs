use std::{fmt::Debug, sync::atomic::{AtomicU32, Ordering}};

use dyn_clone::DynClone;

use super::{ElaborationError, Pattern, Term, Value};

static NEW_EXTERNAL_FUNCTION_ID: AtomicU32 = AtomicU32::new(0);

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct ExternalFunctionId(u32);

impl ExternalFunctionId {
    pub fn new() -> ExternalFunctionId {
        let id = NEW_EXTERNAL_FUNCTION_ID.fetch_add(1, Ordering::SeqCst);
        ExternalFunctionId(id)
    }
}

impl Debug for ExternalFunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn{}", self.0)
    }
}

// TODO: remove pub crate
#[derive(Clone)]
pub struct ExternalFunction {
    pub(crate) id: ExternalFunctionId,
    pub(crate) pattern: Pattern,
    pub(crate) arg_type: Box<Value>,
    pub(crate) body_type: Box<Term>,
    pub(crate) func: Box<dyn ScytheFunction>
}

impl PartialEq for ExternalFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Debug for ExternalFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f   
            .debug_tuple("ExternalFunction")
            .field(&self.id)
            .finish()
    }
}

pub trait ScytheFunction: DynClone + FnMut(Value) -> Result<Value, ElaborationError> {}

impl<F> ScytheFunction for F where F: Clone + FnMut(Value) -> Result<Value, ElaborationError> {}

dyn_clone::clone_trait_object!(ScytheFunction);