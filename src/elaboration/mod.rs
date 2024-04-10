mod generic;
mod surface;
mod environment;
mod evaluation;
mod type_checking;
mod pattern_matching;
mod equality;
mod external_function;

pub use generic::*;
use num_bigint::BigUint;
pub use surface::*;
pub use evaluation::*;
pub use environment::*;
use thiserror::Error;
pub use type_checking::*;
pub(crate) use pattern_matching::*;
pub use equality::*;

use self::external_function::ExternalFunction;


// XXX: type-check before evaluation
// XXX: guarantee scopes are sanitary
// XXX: re-evaluate values properly 


#[derive(Clone, Debug)]
pub enum Pattern {
    Ignore,
    EmptyTuple,
    Generic(GenericTerm),
    Annotation(Box<Pattern>, Box<Term>)
}

// TODO: add annotations to AST
#[derive(Clone, Debug)]
pub enum Term {
    Generic(GenericTerm),

    Call(Box<Term>, Box<Term>),
    // First(Box<Term>),
    // Second(Box<Term>),

    Func(Pattern, Box<Term>),
    ExternalFunc(ExternalFunction),
    BinaryTuple(Box<Term>, Box<Term>),
    EmptyTuple,
    NatNum(BigUint),
    StrLiteral(String),

    FuncType(Pattern, Box<Term>, Box<Term>),
    BinaryTupleType(Pattern, Box<Term>, Box<Term>),
    Unit,
    Nat,
    Str,
    
    Type,
}

impl From<GenericTerm> for Term {
    fn from(term: GenericTerm) -> Self {
        Term::Generic(term)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Neutral(NeutralValue),

    Func(Environment, Pattern, Box<Term>), 
    ExternalFunc(ExternalFunction),
    BinaryTuple(Box<Value>, Box<Value>),
    EmptyTuple, 
    NatNum(BigUint),
    StrLiteral(String),
    
    FuncType(Environment, Pattern, Box<Value>, Box<Term>),
    BinaryTupleType(Pattern, Box<Value>, Box<Term>),
    Unit,
    Nat,
    Str,
    
    Type
}

impl Value {
    pub fn is_neutral(&self) -> bool {
        match self {
            Value::Neutral(_) => true,
            _ => false
        }
    }
}

impl From<GenericValue> for Value {
    fn from(g: GenericValue) -> Self {
        Value::Neutral(NeutralValue::Generic(g))
    }
}

impl From<NeutralValue> for Value {
    fn from(n: NeutralValue) -> Self {
        Value::Neutral(n)
    }
}

#[derive(Clone, Debug)]
pub enum NeutralValue {
    Generic(GenericValue),
    Call(Box<NeutralValue>, Box<Value>),
    ExternalCall(Box<ExternalFunction>, Box<NeutralValue>)
}

impl From<GenericValue> for NeutralValue {
    fn from(g: GenericValue) -> Self {
        NeutralValue::Generic(g)
    }
}

#[derive(Clone, Debug, Error)]
pub enum ElaborationError {
    #[error("Identifier {0} not found")]
    IdentifierNotFound(String),
    #[error("Binding {0:?} not found")]
    BindingNotFound(GenericTerm),
    #[error("Unbinding {0:?} not found")]
    UnbindingNotFound(GenericValue),
    #[error("Type {0:?} not found")]
    TypeNotFound(GenericTerm),
    #[error("Invalid call")]
    InvalidCall(Value),
    #[error("Can't find type of {0:?}")]
    CannotFindTypeOf(GenericTerm),
    #[error("Error binding to {0:?}: expected type {1:?}, found {2:?}")]
    ExpectedTypeFoundType(Pattern, Value, Value),
}

#[cfg(test)]
pub mod tests {
    use super::*;

    use crate::elaboration::{Term, Context};

    use super::{to_core, IdentifierError, SurfacePattern, SurfaceTerm};

    pub const UNIT: SurfaceTerm = SurfaceTerm::Unit;
    pub const TYPE: SurfaceTerm = SurfaceTerm::Type;
    pub const EMPTY_TUPLE: SurfaceTerm = SurfaceTerm::EmptyTuple;
    pub const NAT: SurfaceTerm = SurfaceTerm::Nat;

    pub mod pattern {
        use super::*;

        pub const IGNORE: SurfacePattern = SurfacePattern::Ignore;

        pub fn var(id: impl Into<String>) -> SurfacePattern {
            SurfacePattern::Generic(id.into())
        }

        pub fn annot(p: SurfacePattern, ty: SurfaceTerm) -> SurfacePattern {
            SurfacePattern::Annotation(Box::new(p), Box::new(ty))
        }
    }

    pub fn var(id: impl Into<String>) -> SurfaceTerm {
        SurfaceTerm::Generic(id.into())
    }

    pub fn nat(n: impl Into<BigUint>) -> SurfaceTerm {
        SurfaceTerm::NatNum(n.into())
    }

    pub fn func(arg: SurfacePattern, body: SurfaceTerm) -> SurfaceTerm {
        SurfaceTerm::Func(arg, Box::new(body))
    }

    pub fn func_type(p: SurfacePattern, arg_type: SurfaceTerm, arg_body: SurfaceTerm) -> SurfaceTerm {
        SurfaceTerm::FuncType(p, Box::new(arg_type), Box::new(arg_body))
    }

    pub fn tuple(l: SurfaceTerm, r: SurfaceTerm) -> SurfaceTerm {
        SurfaceTerm::BinaryTuple(Box::new(l), Box::new(r))
    }

    pub fn tuple_type(patt: SurfacePattern, l_type: SurfaceTerm, r_type: SurfaceTerm) -> SurfaceTerm {
        SurfaceTerm::BinaryTupleType(patt, Box::new(l_type), Box::new(r_type))
    }

    pub fn call(f: SurfaceTerm, x: SurfaceTerm) -> SurfaceTerm {
        SurfaceTerm::Call(Box::new(f), Box::new(x))
    }

    pub fn core(t: SurfaceTerm) -> Result<Term, IdentifierError> {
        let mut ctx = Context::empty();
        to_core(&mut ctx, t)
    }

    // TODO: determine if this is even correct
    pub fn term_eq(ctx: &mut Context, l: Term, r: Term) -> Result<bool, ElaborationError> {
        let mut env = &mut ctx.global_environment();
        let l = evaluate(ctx, &mut env, l);
        println!("{:#?}\n", &ctx);
        println!("{:#?}\n", &env);
        let r = evaluate(ctx, &mut env, r);
        return def_equal(ctx, &mut env, &mut l?, &mut r?)
    }

    pub fn surface_evaluate(s: SurfaceTerm) -> Result<Value, ElaborationError> {
        let mut ctx = Context::empty();
        surface_evaluate_with_context(&mut ctx, s)
    }

    pub fn surface_evaluate_with_context(ctx: &mut Context, s: SurfaceTerm) -> Result<Value, ElaborationError> {
        let mut env = &mut ctx.global_environment();
        let term = to_core(ctx, s).unwrap();
        evaluate(ctx, &mut env, term)
    }
}

// TODO: uncomment/fix test cases
// #[cfg(test)]
// pub mod tests {
//     use super::*;

//     fn eval(t: Term) -> Result<Value, ElaborationError> {
//         let mut ctx = Context::default();
//         let mut env = Environment::default();
//         ctx.evaluate(&mut env, t)
//     }

//     fn func(arg: impl Into<String>, body: Term) -> Term {
//         Term::Func(Pattern::Id(arg.into()), Box::new(body))
//     }

//     fn call(f: Term, x: Term) -> Term {
//         Term::Call(Box::new(f), Box::new(x))
//     }

//     fn var(id: impl Into<String>) -> Term {
//         Term::Id(id.into())
//     }

//     #[test]
//     fn basic() {
//         println!("{:?}", eval(call(func("x", Term::EmptyTuple), Term::EmptyTuple)));
//     }

//     #[test]
//     fn basic_2() {
//         println!("{:?}", eval(call(func("x", var("x")), Term::EmptyTuple)));
//     }

//     #[test]
//     fn basic_3() {
//         println!("{:?}", eval(call(func("x", func("y", var("x"))), Term::EmptyTuple)));
//     }
// }