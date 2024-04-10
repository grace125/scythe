use concrete_syntax::{ParseError, parse};
use elaboration::{ElaborationError, IdentifierError, evaluate, infer, quote, to_core, Context, Term};
use thiserror::Error;
use abstract_syntax::abstract_parse;
use lexing::lex;

pub mod lexing;
pub mod concrete_syntax;
pub mod abstract_syntax;
pub mod elaboration;


#[derive(Error, Debug)]
pub enum LanguageError {
    // Lexing(),
    #[error("failure parsing: {0}")]
    Parsing(#[from] ParseError),

    #[error("failure elaborating: {0}")]
    Elaboration(#[from] ElaborationError),

    #[error("failure converting to core: {0}")]
    Core(#[from] IdentifierError)
}

pub fn compile_default(s: &str) -> Result<Term, LanguageError> {
    let ctx = &mut Context::empty();
    let env = &mut ctx.global_environment();
    let s = to_core(ctx, abstract_parse(parse(&mut lex(s))?))?;
    let (s, _) = infer(ctx, env, s)?;
    let s = evaluate(ctx, env, s)?;
    Ok(quote(ctx, env, s)?)
}