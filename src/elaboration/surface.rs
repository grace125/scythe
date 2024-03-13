use std::collections::HashMap;
use crate::elaboration::{Pattern, Context, Term, generic::*};

pub enum SurfacePattern {
    Generic(String),
    Annotation(Box<SurfacePattern>, Box<SurfaceTerm>)
}

pub enum SurfaceTerm {
    Generic(String), // TODO: switch to Cow<str>?
    Func(SurfacePattern, Box<SurfaceTerm>),
    Call(Box<SurfaceTerm>, Box<SurfaceTerm>),
    EmptyTuple,
    FuncType(SurfacePattern, Box<SurfaceTerm>, Box<SurfaceTerm>),
    Unit,
    Type,
}

#[derive(Default, Debug)]
pub struct SurfaceEnvironment {
    variables: HashMap<String, GenericTerm>,
    history: Vec<HistoryItem>,
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

pub fn to_core(ctx: &mut Context, sctx: &mut SurfaceEnvironment, surface: SurfaceTerm) -> Result<Term, IdentifierError> {
    match surface {
        SurfaceTerm::Generic(id) => {
            let generic_term = sctx.get_variable(id)?;
            Ok(generic_term.into())
        }
        SurfaceTerm::Func(arg, body) => {
            sctx.start_scope();
            let patt = surface_bind_pattern(ctx, sctx, arg)?;
            let body = to_core(ctx, sctx, *body)?;
            sctx.end_scope();
            Ok(Term::Func(patt, Box::new(body)))
        },
        SurfaceTerm::Call(f, x) => {
            let f = to_core(ctx, sctx, *f)?;
            let x = to_core(ctx, sctx, *x)?;
            Ok(Term::Call(Box::new(f), Box::new(x)))
        },
        SurfaceTerm::EmptyTuple => Ok(Term::EmptyTuple),
        SurfaceTerm::FuncType(patt, arg_type, body_type) => {
            let arg_type = to_core(ctx, sctx, *arg_type)?;
            sctx.start_scope();
            let patt = surface_bind_pattern(ctx, sctx, patt)?;
            let body_type = to_core(ctx, sctx, *body_type)?;
            sctx.end_scope();
            Ok(Term::FuncType(patt, Box::new(arg_type), Box::new(body_type)))
        },
        SurfaceTerm::Unit => Ok(Term::Unit),
        SurfaceTerm::Type => Ok(Term::Type),
    }
}

fn surface_bind_pattern(ctx: &mut Context, sctx: &mut SurfaceEnvironment, patt: SurfacePattern) -> Result<Pattern, IdentifierError> {
    match patt {
        SurfacePattern::Generic(id) => Ok(Pattern::Generic(sctx.new_variable(ctx, id))),
        SurfacePattern::Annotation(patt_inner, type_id) => {
            let type_term = to_core(ctx, sctx, *type_id)?;
            Ok(Pattern::Annotation(Box::new(surface_bind_pattern(ctx, sctx, *patt_inner)?), Box::new(type_term.into())))
        },
    }
}

#[derive(Clone, Debug)]
pub enum IdentifierError {
    IdentifierNotFound(String)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use super::super::{*, tests::*};

    #[test]
    fn correct_var_1() {
        let mut ctx = Context::default();

        let term_1 = to_core(&mut ctx, &mut SurfaceEnvironment::default(), func(pattern::var("x"), func(pattern::var("x"), var("x")))).unwrap();
        let term_2 = to_core(&mut ctx, &mut SurfaceEnvironment::default(), func(pattern::var("x"), func(pattern::var("y"), var("y")))).unwrap();

        assert!(term_eq(&mut ctx, term_1, term_2).unwrap());
    }
}