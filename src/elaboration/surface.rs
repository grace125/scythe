use std::collections::HashMap;
use num_bigint::BigUint;
use thiserror::Error;

use crate::elaboration::{Pattern, Context, Term, generic::*};

pub enum SurfacePattern {
    Ignore,
    EmptyTuple,
    Generic(String),
    Annotation(Box<SurfacePattern>, Box<SurfaceTerm>)
}

pub enum SurfaceTerm {
    Generic(String), // TODO: switch to Cow<str>?
    Func(SurfacePattern, Box<SurfaceTerm>),
    Call(Box<SurfaceTerm>, Box<SurfaceTerm>),
    FuncType(SurfacePattern, Box<SurfaceTerm>, Box<SurfaceTerm>),
    BinaryTuple(Box<SurfaceTerm>, Box<SurfaceTerm>),
    BinaryTupleType(SurfacePattern, Box<SurfaceTerm>, Box<SurfaceTerm>),
    EmptyTuple,
    NatNum(BigUint),
    StrLiteral(String),
    Unit,
    Nat,
    Str,
    Type,
}

#[derive(Default, Debug)]
struct SurfaceEnvironment {
    variables: HashMap<String, GenericTerm>,
    history: Vec<HistoryItem>,
}

impl SurfaceEnvironment {
    pub fn from_ctx(ctx: &Context) -> Self {
        Self {
            variables: ctx.surface_variables.clone(),
            history: Vec::new()
        }
    }
}

impl SurfaceEnvironment {
    fn start_scope(&mut self) {
        self.history.push(HistoryItem::Scope);
    }

    fn end_scope(&mut self) {
        while let Some(h) = self.history.pop() {
            match h {
                HistoryItem::Scope => break,
                HistoryItem::Variable(id, Some(x))  => { self.variables.insert(id, x); },
                HistoryItem::Variable(id, None)     => { self.variables.remove(&id); }
            }
        }
    }

    fn new_variable(&mut self, ctx: &mut Context, id: String) -> GenericTerm {
        let x = GenericTerm::new();
        let old_var = self.variables.insert(id.clone(), x);
        ctx.ids.insert(x, id.clone());
        self.history.push(HistoryItem::Variable(id, old_var));
        x
    }

    fn get_variable(&mut self, id: String) -> Result<GenericTerm, IdentifierError> {
        self.variables.get(&id).ok_or(IdentifierError::IdentifierNotFound(id)).cloned()
    }
}

#[derive(Clone, Debug)]
enum HistoryItem {
    Scope,
    Variable(String, Option<GenericTerm>)
}

pub fn to_core(ctx: &mut Context, surface: SurfaceTerm) -> Result<Term, IdentifierError> {
    let senv = &mut SurfaceEnvironment::from_ctx(ctx);
    to_core_inner(ctx, senv, surface)
}

fn to_core_inner(ctx: &mut Context, senv: &mut SurfaceEnvironment, surface: SurfaceTerm) -> Result<Term, IdentifierError> {
    match surface {
        SurfaceTerm::Generic(id) => {
            let generic_term = senv.get_variable(id)?;
            Ok(generic_term.into())
        }
        SurfaceTerm::Func(arg, body) => {
            senv.start_scope();
            let patt = surface_bind_pattern(ctx, senv, arg)?;
            let body = to_core_inner(ctx, senv, *body)?;
            senv.end_scope();
            Ok(Term::Func(patt, Box::new(body)))
        },
        SurfaceTerm::Call(f, x) => {
            let f = to_core_inner(ctx, senv, *f)?;
            let x = to_core_inner(ctx, senv, *x)?;
            Ok(Term::Call(Box::new(f), Box::new(x)))
        },
        
        SurfaceTerm::BinaryTuple(l, r) => Ok(Term::BinaryTuple(
            Box::new(to_core_inner(ctx, senv, *l)?), 
            Box::new(to_core_inner(ctx, senv, *r)?)
        )),
        SurfaceTerm::EmptyTuple => Ok(Term::EmptyTuple),
        SurfaceTerm::NatNum(n) => Ok(Term::NatNum(n)),
        SurfaceTerm::StrLiteral(s) => Ok(Term::StrLiteral(s)),
        
        SurfaceTerm::Nat => Ok(Term::Nat),
        SurfaceTerm::Unit => Ok(Term::Unit),
        SurfaceTerm::Str => Ok(Term::Str),
        SurfaceTerm::FuncType(patt, arg_type, body_type) => {
            let arg_type = Box::new(to_core_inner(ctx, senv, *arg_type)?);
            senv.start_scope();
            let patt = surface_bind_pattern(ctx, senv, patt)?;
            let body_type = Box::new(to_core_inner(ctx, senv, *body_type)?);
            senv.end_scope();
            Ok(Term::FuncType(patt, arg_type, body_type))
        },
        SurfaceTerm::BinaryTupleType(patt, l_type, r_type) => {
            let l_type = Box::new(to_core_inner(ctx, senv, *l_type)?);
            senv.start_scope();
            let patt = surface_bind_pattern(ctx, senv, patt)?;
            let r_type = Box::new(to_core_inner(ctx, senv, *r_type)?);
            senv.end_scope();
            Ok(Term::BinaryTupleType(patt, l_type, r_type))
        }
        SurfaceTerm::Type => Ok(Term::Type),
    }
}

fn surface_bind_pattern(ctx: &mut Context, senv: &mut SurfaceEnvironment, patt: SurfacePattern) -> Result<Pattern, IdentifierError> {
    match patt {
        SurfacePattern::Ignore => Ok(Pattern::Ignore),
        SurfacePattern::EmptyTuple => Ok(Pattern::EmptyTuple),
        SurfacePattern::Generic(id) => {
            if ctx.keywords.contains(&id) {
                Err(IdentifierError::KeywordInPattern(id))
            }
            else {
                Ok(Pattern::Generic(senv.new_variable(ctx, id)))
            }
            
        },
        SurfacePattern::Annotation(patt_inner, type_id) => {
            let type_term = to_core_inner(ctx, senv, *type_id)?;
            Ok(Pattern::Annotation(Box::new(surface_bind_pattern(ctx, senv, *patt_inner)?), Box::new(type_term.into())))
        },
    }
}

#[derive(Clone, Debug, Error)]
pub enum IdentifierError {
    #[error("Identifier {0} not found")]
    IdentifierNotFound(String),
    #[error("Keyword {0} used in pattern")]
    KeywordInPattern(String)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use super::super::{*, tests::*};

    #[test]
    fn correct_var_1() {
        let ctx = &mut Context::empty();

        let term_1 = to_core_inner(ctx, &mut SurfaceEnvironment::default(), func(pattern::var("x"), func(pattern::var("x"), var("x")))).unwrap();
        let term_2 = to_core_inner(ctx, &mut SurfaceEnvironment::default(), func(pattern::var("x"), func(pattern::var("y"), var("y")))).unwrap();

        assert!(term_eq(ctx, term_1, term_2).unwrap());
    }

    #[test]
    pub fn unit_by_generic() {
        let ctx = &mut Context::empty();
        let env = &mut ctx.global_environment();
        let l = &mut surface_evaluate_with_context(ctx, UNIT).unwrap();
        let r = &mut surface_evaluate_with_context(ctx, SurfaceTerm::Generic("Unit".into())).unwrap();
        assert!(def_equal(ctx, env, l, r).unwrap())
    }

    #[test]
    pub fn type_by_generic() {
        let ctx = &mut Context::empty();
        let env = &mut ctx.global_environment();
        let l = &mut surface_evaluate_with_context(ctx, TYPE).unwrap();
        let r = &mut surface_evaluate_with_context(ctx, SurfaceTerm::Generic("Type".into())).unwrap();
        assert!(def_equal(ctx, env, l, r).unwrap())
    }
}