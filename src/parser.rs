use logos::{Logos, Lexer};
use std::num::{ParseIntError, ParseFloatError};


#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex(r"[a-zA-Z_][0-9a-zA-Z_]*", |lex| lex.slice().to_string(), priority = 2)]
    Id(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse(), priority = 1)] 
    Nat(u64),

    #[regex(r"[0-9]+\.[0-9]*", |lex| lex.slice().parse(), priority = 1)]
    Float(f64),

    // #[regex("\"\"")]
    // String,

    #[regex(r"([]{}();:,+*-/%]|=>|if|then|else)", |lex| op_kind_from_str(lex.slice()), priority = 1)]
    Op(OpKind),
}

fn op_kind_from_str(s: &str) -> OpKind {
    use OpKind::*;

    match s {
        "+" => Add,
        "*" => Mul,
        "-" => Sub,
        "/" => Div,
        "%" => Mod,
        _ => unreachable!()
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingError {
    InvalidNatLiteral,
    InvalidFloatLiteral,

    #[default]
    NonAsciiCharacter,
}

impl From<ParseIntError> for LexingError {
    fn from(_value: ParseIntError) -> Self {
        LexingError::InvalidNatLiteral
    }
}

impl From<ParseFloatError> for LexingError {
    fn from(_value: ParseFloatError) -> Self {
        LexingError::InvalidFloatLiteral
    }
}

const ROOT_GROUP: u32   = 0b__0;
const SUM_GROUP: u32    = 0b__1;
const PROD_GROUP: u32   = 0b_10;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Root,
}

impl OpKind {
    pub fn arity(&self) -> Option<usize> {  
        match self {
            OpKind::Root => Some(0),
            _ => Some(2)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Operator {
    kind: OpKind,
    children: Vec<AST>
}

impl Operator {
    pub fn new(kind: OpKind) -> Operator {
        Operator {
            children: if let Some(a) = kind.arity() { Vec::with_capacity(a) } else { Vec::new() }, 
            kind, 
        }
    }
}

impl Operator {
    fn left_precedence(&self) -> Option<u32> {
        match self.kind {
            OpKind::Add | OpKind::Sub               => Some(SUM_GROUP),
            OpKind::Mul | OpKind::Div | OpKind::Mod => Some(PROD_GROUP),
            OpKind::Root => Some(ROOT_GROUP),
        }
    }

    fn right_precedence(&self) -> Option<u32> {
        match self.kind {
            OpKind::Add | OpKind::Sub               => {
                if let Some(AST::None) = self.children.last() {
                    Some(PROD_GROUP)
                }
                else {
                    Some(SUM_GROUP)
                }
            },
            OpKind::Mul | OpKind::Div | OpKind::Mod => Some(PROD_GROUP),
            OpKind::Root => Some(ROOT_GROUP),
        }
    }

    fn push(&mut self, item: AST, precedence: Option<u32>) -> Result<(), ParseError> {
        if precedence.is_some() {
            self.children.push(item);
            Ok(())
        }
        else if item != AST::None {
            Err(ParseError::ItemExpected)
        }
        else {
            Ok(())
        }
    }

    fn arity_check(&self) -> Result<(), ParseError> {
        let arity = self.kind.arity();
        if arity.is_some() && Some(self.children.len()) != arity {
            Err(ParseError::InvalidArity)
        }
        else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Op(Operator),
    Nat(u64),
    Float(f64),
    Id(String),
    None,
}

pub fn parse(lex: &mut Lexer<Token>) -> Result<AST, ParseError> {
    parse_inner(lex, Operator::new(OpKind::Root), AST::None)
}

fn parse_inner(lex: &mut Lexer<Token>, mut last_op: Operator, item: AST) -> Result<AST, ParseError> {
    let next_token = match lex.next() {
        Some(Ok(t)) => t,
        Some(Err(e)) =>  return Err(ParseError::from(e)),
        None => {
            last_op.push(item, last_op.right_precedence())?;

            last_op.arity_check()?;

            return Ok(AST::Op(last_op));
        }, 
    };

    match next_token {
        Token::Nat(n)   if item == AST::None => parse_inner(lex, last_op, AST::Nat(n)),
        Token::Float(f) if item == AST::None => parse_inner(lex, last_op, AST::Float(f)),
        Token::Id(id)   if item == AST::None => parse_inner(lex, last_op, AST::Id(id)),

        Token::Nat(n)   => parse_apposition(lex, last_op, item, AST::Nat(n)),
        Token::Float(f) => parse_apposition(lex, last_op, item, AST::Float(f)),
        Token::Id(id)   => parse_apposition(lex, last_op, item, AST::Id(id)),

        Token::Op(kind) => {
            let mut this_op = Operator::new(kind);
            let this_op_precedence = this_op.left_precedence();
            let last_op_precedence = last_op.right_precedence();
            if last_op_precedence >= this_op_precedence {
                last_op.push(item, last_op_precedence)?;

                last_op.arity_check()?;

                this_op.push(AST::Op(last_op), this_op_precedence)?;
                parse_inner(lex, this_op, AST::None)
            }
            else {
                this_op.push(item, this_op_precedence)?;
                
                last_op.push(parse_inner(lex, this_op, AST::None)?, last_op_precedence)?;

                Ok(AST::Op(last_op))
            }
        }
    }
}

#[allow(unused_mut, unused_variables)]
fn parse_apposition(lex: &mut Lexer<Token>, mut last_op: Operator, item_1: AST, item_2: AST) -> Result<AST, ParseError> {
    todo!()
}

#[derive(Debug)]
pub enum ParseError {
    LexingError(LexingError),
    EmptyFile,
    ItemExpected,
    InvalidArity,
    InvalidEmptySlot(String),
}

impl From<LexingError> for ParseError {
    fn from(value: LexingError) -> Self {
        ParseError::LexingError(value)
    }
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use super::*;

    #[test]
    fn arithmetic() {
        println!("{:?}", parse(&mut Token::lexer("x + y * z")));
    }

    #[test]
    fn arithmetic_2() {
        for token in Token::lexer("x + y * 3 / 4.0 - -2.0 % 5") {
            println!("{:?}", token);
        }
    }
}
