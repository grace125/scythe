use super::*;
pub use holes::*;
use thiserror::Error;

mod holes {
    use std::{fmt::Debug, sync::atomic::{AtomicU32, Ordering}};

    static NEW_HOLE_ID: AtomicU32 = AtomicU32::new(0);

    #[derive(Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
    pub struct Hole(u32);

    impl Hole {
        pub fn new() -> Hole {
            let id = NEW_HOLE_ID.fetch_add(1, Ordering::SeqCst);
            Hole(id)
        }
    }

    impl Debug for Hole {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "h{}", self.0)
        }
    }
}

#[derive(Clone, Error, Debug)]
pub enum UnificationError {
    
    #[error("Expected {0:?}, got {1:?}")]
    ExpectedThisGotThat(Value, Value),

    #[error("{0}")]
    ElaborationError(Box<ElaborationError>),
}

impl From<ElaborationError> for UnificationError {
    fn from(value: ElaborationError) -> Self {
        match value {
            ElaborationError::UnificationError(u) => u,
            e => UnificationError::ElaborationError(e.into())
        }
    }
}

pub fn equal(ctx: &mut Context, env: &mut Environment, l: &mut Value, r: &mut Value) -> bool {
    unify(ctx, env, l, r).is_ok()
}

// Returns true if the output has holes
pub fn unify(ctx: &mut Context, env: &mut Environment, l: &mut Value, r: &mut Value) -> Result<bool, UnificationError> {
    match (l, r) {
        (Value::Type, Value::Type) |
        (Value::Unit, Value::Unit) |
        (Value::Nat, Value::Nat) |
        (Value::Str, Value::Str) |
        (Value::EmptyTuple, Value::EmptyTuple) => Ok(false),

        (Value::ExternalFunc(f1), Value::ExternalFunc(f2))  if f1 == f2 => Ok(false),
        (Value::NatNum(n1), Value::NatNum(n2))              if n1 == n2 => Ok(false),
        (Value::StrLiteral(s1), Value::StrLiteral(s2))      if s1 == s2 => Ok(false),

        (Value::Func(e1, p1, b1), Value::Func(e2, p2, b2)) => {
            unify_func(ctx, env, e1, p1, b1, e2, p2, b2)
        },
        (Value::Tuple(l1, r1), Value::Tuple(l2, r2)) => {
            unify_tuple(ctx, env, l1, r1, l2, r2)
        },
        (Value::FuncType(env_l, patt_l, arg_l_type, body_l_type), Value::FuncType(env_r, patt_r, arg_r_type, body_r_type)) => {
            unify_func_type(ctx, env, env_l, patt_l, arg_l_type, body_l_type, env_r, patt_r, arg_r_type, body_r_type)
        },
        (Value::TupleType(patt1, l1_type, r1_type), Value::TupleType(patt2, l2_type, r2_type)) => {
            unify_tuple_type(ctx, env, patt1, l1_type, r1_type, patt2, l2_type, r2_type)
        },
        (Value::Neutral(n1), Value::Neutral(n2)) => {
            unify_neutral(ctx, env, n1, n2)
        },
        
        (l @ Value::Hole(_), r @ Value::Hole(_)) => {
            unify_hole_hole(ctx, env, l, r)
        },
        (l @ Value::Hole(_), other) => {
            dbg!("c");
            unify_hole_other(ctx, env, l, other)
        },
        (other, r @ Value::Hole(_)) => {
            dbg!("d");
            unify_hole_other(ctx, env, r, other)
        },

        (Value::Binder(..) | Value::BlankBinder | Value::Annotation(..), Value::Binder(..) | Value::BlankBinder | Value::Annotation(..)) => unimplemented!(),
        (Value::BlankBinder, _) | (_, Value::BlankBinder) => Ok(false),
        
        (Value::Binder(x), v) | (v, Value::Binder(x)) => {
            env.insert(*x, v.clone());
            Ok(false)
        },

        (Value::Annotation(patt, _), other) => unify(ctx, env, &mut *patt.0, other),
        (other, Value::Annotation(patt, _)) => unify(ctx, env, other, &mut *patt.0),

        (l, r @ (
            Value::Neutral(_) |
            
            Value::NatNum(_) |
            Value::StrLiteral(_) |
            Value::EmptyTuple |
            Value::Tuple(..) |
            Value::ExternalFunc(..) |
            Value::Func(..) |

            Value::Nat |
            Value::Unit |
            Value::Str |
            Value::TupleType(..) |
            Value::FuncType(..) |

            Value::Type 
        )) => Err(UnificationError::ExpectedThisGotThat(l.clone(), r.clone())),
    }
}

fn unify_func(
    ctx: &mut Context, 
    env: &mut Environment, 
    env_l: &mut Environment, patt_l: &mut Pattern, term_l: &mut Term, 
    env_r: &mut Environment, patt_r: &mut Pattern, term_r: &mut Term
) -> Result<bool, UnificationError> {
    let g = GenericValue::new();

    env_l.start_scope();
    bind(ctx, env_l, patt_l, g.into())?;
    let mut val_l = evaluate_recoverable(ctx, env_l, (*term_l).clone())?;
    env_l.end_scope();

    env_r.start_scope();
    bind(ctx, env_r, patt_r, g.into())?;
    let mut val_r = evaluate_recoverable(ctx, env_r, (*term_r).clone())?;
    env_r.end_scope();
    
    env.start_scope();
    env.insert_binding(GenericTerm::new(), g); // FIXME: maybe unnecessary binding. will equality ever quote values?
    let res = unify(ctx, env, &mut val_l, &mut val_r)?;
    env.end_scope();

    Ok(res)
}

fn unify_func_type(
    ctx: &mut Context, 
    env: &mut Environment, 
    env_l: &mut Environment, patt_l: &mut Pattern, arg_l_type: &mut Value, body_l_type: &mut Term,
    env_r: &mut Environment, patt_r: &mut Pattern, arg_r_type: &mut Value, body_r_type: &mut Term,
) -> Result<bool, UnificationError> {
    let has_holes = unify(ctx, env, &mut *arg_l_type, &mut *arg_r_type)? || unify_func(ctx, env, env_l, patt_l, body_l_type, env_r, patt_r, body_r_type)?;
    Ok(has_holes)
}

fn unify_tuple(ctx: &mut Context, env: &mut Environment, l1: &mut Value, r1: &mut Value, l2: &mut Value, r2: &mut Value) -> Result<bool, UnificationError> {
    let has_holes = unify(ctx, env, l1, l2)? || unify(ctx, env, r1, r2)?;
    Ok(has_holes)
}

fn unify_tuple_type(
    ctx: &mut Context, 
    env: &mut Environment, 
    patt1: &mut Pattern, l1_type: &mut Box<Value>, r1_type: &mut Box<Term>, 
    patt2: &mut Pattern, l2_type: &mut Box<Value>, r2_type: &mut Box<Term>, 
) -> Result<bool, UnificationError> {
    let has_holes = unify(ctx, env, l1_type, l2_type)?;

    let g = GenericValue::new();

    env.start_scope();
    bind(ctx, env, &patt1, g.into())?;
    let mut r1_type = evaluate(ctx, env, *r1_type.clone())?;
    env.end_scope();

    env.start_scope();
    bind(ctx, env, &patt2, g.into())?;
    let mut r2_type = evaluate(ctx, env, *r2_type.clone())?;
    env.end_scope();

    env.start_scope();
    env.insert_binding(GenericTerm::new(), g); // FIXME: maybe unnecessary binding. will equality ever quote values?
    let has_holes = unify(ctx, env, &mut r1_type, &mut r2_type)? || has_holes;
    env.end_scope();

    Ok(has_holes)
}

fn unify_neutral(ctx: &mut Context, env: &mut Environment, n1: &mut NeutralValue, n2: &mut NeutralValue) -> Result<bool, UnificationError> {
    match (n1, n2) {
        (NeutralValue::Generic(g1), NeutralValue::Generic(g2)) if g1 == g2 => Ok(true),
        (NeutralValue::Call(n1, v1), NeutralValue::Call(n2, v2)) => {
            Ok(unify_neutral(ctx, env, n1, n2)? || unify(ctx, env, v1, v2)?)
        },
        (NeutralValue::ExternalCall(ext_fn1, n1), NeutralValue::ExternalCall(ext_fn2, n2)) => {
            Ok(ext_fn1 == ext_fn2 || unify_neutral(ctx, env, n1, n2)?)
        },
        (NeutralValue::TupleLeft(l1, r1), NeutralValue::TupleLeft(l2, r2)) => {
            Ok(unify_neutral(ctx, env, l1, l2)? || unify(ctx, env, r1, r2)?)
        },
        (NeutralValue::TupleRight(l1, r1), NeutralValue::TupleRight(l2, r2)) => {
            Ok(unify(ctx, env, l1, l2)? || unify_neutral(ctx, env, r1, r2)?)
        },

        (NeutralValue::Let(patt, _, rest), other) => { 
            env.start_scope();
            bind(ctx, env, &patt, GenericValue::new().into())?;
            let mut rest = evaluate(ctx, env, *rest.clone())?;
            let unity = unify(ctx, env, &mut rest, &mut other.clone().into());
            env.end_scope();
            unity
        }
        (other, NeutralValue::Let(patt, _, rest)) => {
            env.start_scope();
            bind(ctx, env, &patt, GenericValue::new().into())?;
            let mut rest = evaluate(ctx, env, *rest.clone())?;
            let unity = unify(ctx, env, &mut other.clone().into(), &mut rest);
            env.end_scope();
            unity
        },
        (a, b @ (
            NeutralValue::TupleLeft(..) |
            NeutralValue::TupleRight(..) |
            NeutralValue::ExternalCall(..) |
            NeutralValue::Generic(_) |
            NeutralValue::Call(..)
        )) => Err(UnificationError::ExpectedThisGotThat(a.clone().into(), b.clone().into())),
    }
}

fn unify_hole_hole(ctx: &mut Context, env: &mut Environment, l: &mut Value, r: &mut Value) -> Result<bool, UnificationError> {
    env.update_hole(l)?;
    env.update_hole(r)?;

    match (l, r) {
        (Value::Hole(h1), Value::Hole(h2)) => {
            env.unify_empty_holes(*h1, *h2)?;
            Ok(true)
        },
        (l, r) => unify(ctx, env, l, r)
    }
}

fn unify_hole_other(ctx: &mut Context, env: &mut Environment, l: &mut Value, other: &mut Value) -> Result<bool, UnificationError> {
    env.update_hole(l)?;
    if let Value::Hole(hole) = l {
        env.assign_to_empty_hole(*hole, other.clone())?;
        *l = other.clone();
        Ok(false)
    }
    else {
        unify(ctx, env, l, other)
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    use super::{super::tests::*, super::evaluation::tests::*};

    #[test]
    fn nat_unify() {
        assert_evaluates_to_eq(
            nat(3u32), 
            nat(3u32), 
        )
    }

    #[test]
    #[should_panic]
    fn nat_unify_err() {
        assert_evaluates_to_eq(
            nat(0u32), 
            nat(1u32), 
        )
    }
}