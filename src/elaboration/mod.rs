mod generic;
mod surface;
mod environment;
mod evaluation;
mod type_checking;
mod pattern_matching;
mod equality;

pub use generic::*;
pub use surface::*;
pub use evaluation::*;
pub use environment::*;
pub use type_checking::*;
pub(crate) use pattern_matching::*;
pub use equality::*;


// XXX: type-check before evaluation
// XXX: guarantee scopes are sanitary
// XXX: re-evaluate values properly 


#[derive(Clone, Debug)]
pub enum Pattern {
    Generic(GenericTerm),
    Annotation(Box<Pattern>, Box<Term>)
}

// TODO: add annotations to AST
#[derive(Clone, Debug)]
pub enum Term {
    Generic(GenericTerm),
    Func(Pattern, Box<Term>),
    FuncType(Pattern, Box<Term>, Box<Term>),
    Call(Box<Term>, Box<Term>),
    EmptyTuple,
    Unit,
    // BinaryTuple(Box<Term>, Box<Term>),
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
    FuncType(Environment, Pattern, Box<Value>, Box<Term>),
    EmptyTuple,
    // BinaryTuple(Box<Value>, Box<Value>),
    Unit,
    Type
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
    Call(Box<NeutralValue>, Box<Value>)
}

impl From<GenericValue> for NeutralValue {
    fn from(g: GenericValue) -> Self {
        NeutralValue::Generic(g)
    }
}

#[derive(Clone, Debug)]
pub enum ElaborationError {
    IdentifierNotFound(String),
    BindingNotFound(GenericTerm),
    UnbindingNotFound(GenericValue),
    TypeNotFound(GenericTerm),
    InvalidCall(Value),
    CannotFindTypeOf(GenericTerm),
    ExpectedTypeFoundType(Pattern, Value, Value)
}

#[cfg(test)]
pub mod tests {
    use super::*;

    use crate::elaboration::{Term, Context};

    use super::{to_core, IdentifierError, SurfacePattern, SurfaceTerm};

    pub const UNIT: SurfaceTerm = SurfaceTerm::Unit;
    pub const TYPE: SurfaceTerm = SurfaceTerm::Type;
    pub const EMPTY_TUPLE: SurfaceTerm = SurfaceTerm::EmptyTuple;

    pub mod pattern {
        use super::*;

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

    pub fn func(arg: SurfacePattern, body: SurfaceTerm) -> SurfaceTerm {
        SurfaceTerm::Func(arg, Box::new(body))
    }

    pub fn func_type(p: SurfacePattern, arg_type: SurfaceTerm, arg_body: SurfaceTerm) -> SurfaceTerm {
        SurfaceTerm::FuncType(p, Box::new(arg_type), Box::new(arg_body))
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