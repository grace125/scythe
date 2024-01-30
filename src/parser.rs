use logos::{Logos, Lexer};
use std::num::{ParseIntError, ParseFloatError};

const APPOSITION_LEFT_GROUP: u32    = 0b__10;
const APPOSITION_RIGHT_GROUP: u32   = 0b___1;
const SUM_GROUP: u32                = 0b_100;
const PROD_GROUP: u32               = 0b1000;

const ROOT_SLOT: OperatorSlot               = OperatorSlot::new(0, 0);
const APPOSITION_LEFT_SLOT: OperatorSlot    = OperatorSlot::new(APPOSITION_LEFT_GROUP, 0);
const APPOSITION_RIGHT_SLOT: OperatorSlot   = OperatorSlot::new(APPOSITION_RIGHT_GROUP, 0);
const SUM_SLOT: OperatorSlot                = OperatorSlot::new(SUM_GROUP, 0);
const PROD_SLOT: OperatorSlot               = OperatorSlot::new(PROD_GROUP, 0);

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
    match s {
        "+" => OpKind::Add,
        "*" => OpKind::Mul,
        "-" => OpKind::Sub,
        "/" => OpKind::Div,
        "%" => OpKind::Mod,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Apposition,
    Root,
}

#[derive(Clone, PartialEq)]
pub struct Operator {
    kind: OpKind,
    left: Option<Box<CST>>,
    right: Option<Box<CST>>
}

impl std::fmt::Debug for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Operator { kind, left, right } = self;
        
        f.write_fmt(format_args!("{:?}(", kind))?;
        if let Some(left) = left {
            (**left).fmt(f)?;
        }
        if let Some(right) = right {
            f.write_str(", ")?;
            (**right).fmt(f)?;
            f.write_str(")")
        }
        else {
            f.write_str(",)")
        }
    }
}

impl Into<CST> for Operator {
    fn into(self) -> CST {
        CST::Op(self)
    }
}

impl Into<Box<CST>> for Operator {
    fn into(self) -> Box<CST> {
        Box::new(CST::Op(self))
    }
}

impl Into<Option<Box<CST>>> for Operator {
    fn into(self) -> Option<Box<CST>> {
        Some(Box::new(CST::Op(self)))
    }
}

impl Operator {
    pub fn new(kind: OpKind) -> Operator {
        Operator { 
            kind, 
            left: None,
            right: None 
        }
    }

    fn left_slot(&self) -> Option<OperatorSlot> {
        match self.kind {
            OpKind::Add | OpKind::Sub               => Some(SUM_SLOT),
            OpKind::Mul | OpKind::Div | OpKind::Mod => Some(PROD_SLOT),
            OpKind::Apposition => Some(APPOSITION_LEFT_SLOT),
            OpKind::Root => Some(ROOT_SLOT),
        }
    }

    fn right_slot(&self) -> Option<OperatorSlot> {
        match self.kind {
            OpKind::Add | OpKind::Sub => Some(
                if self.left.is_none() { PROD_SLOT } else { SUM_SLOT }
            ),
            OpKind::Mul | OpKind::Div | OpKind::Mod => Some(PROD_SLOT),
            OpKind::Apposition => Some(APPOSITION_RIGHT_SLOT),
            OpKind::Root => Some(ROOT_SLOT),
        }
    }
}

pub struct OperatorSlot {
    precedence: u32,
    conflicts: u32,
}

impl OperatorSlot {
    const fn new(precedence: u32, conflicts: u32) -> Self {
        OperatorSlot { precedence, conflicts }
    }

    fn takes_precedence_over(&self, other: &OperatorSlot) -> Result<bool, ParseError> {
        if (self.precedence & other.conflicts) | (other.precedence & self.conflicts) != 0 {
            Err(ParseError::OperatorConflict)
        }
        else {
            Ok(self.precedence >= other.precedence)
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum CST {
    Op(Operator),
    Nat(u64),
    Float(f64),
    Id(String),
}

impl std::fmt::Debug for CST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Op(operator) => operator.fmt(f),
            Self::Nat(n) => f.write_fmt(format_args!("Nat({})", n)),
            Self::Float(fl) => f.write_fmt(format_args!("Float({})", fl)),
            Self::Id(id) => f.write_fmt(format_args!("`{}`", id)),
        }
    }
}

impl Into<Option<Box<CST>>> for CST {
    fn into(self) -> Option<Box<CST>> {
        Some(Box::new(self))
    }
}

pub fn parse(lex: &mut Lexer<Token>) -> Result<CST, ParseError> {
    _parse(lex, Operator::new(OpKind::Root), None).map(|op| CST::Op(op))
}

fn _parse(lex: &mut Lexer<Token>, mut last_op: Operator, item: Option<Box<CST>>) -> Result<Operator, ParseError> {
    let next_token = match lex.next() {
        Some(Ok(t)) => t,
        Some(Err(e)) => return Err(ParseError::from(e)),
        None => { 
            last_op.right = item;
            return Ok(last_op);
        }, 
    };

    match next_token {
        Token::Nat(n)   => parse_literal(lex, last_op, item, CST::Nat(n)),
        Token::Float(f) => parse_literal(lex, last_op, item, CST::Float(f)),
        Token::Id(id)   => parse_literal(lex, last_op, item, CST::Id(id)),
        Token::Op(op_kind) => {
            let mut this_op = Operator::new(op_kind);
            match (last_op.right_slot(), item, this_op.left_slot()) {
                (Some(last_slot), item, Some(this_slot)) => parse_comparison(lex, last_op, last_slot, this_op, this_slot, item, None),
                (None, None, Some(_)) => {
                    this_op.left = Some(Box::new(CST::Op(last_op)));
                    _parse(lex, this_op, None)
                },
                (None, Some(item), Some(_)) => {
                    this_op.left = Some(item);
                    Ok(Operator {
                        kind: OpKind::Apposition, 
                        left: last_op.into(), 
                        right: _parse(lex, this_op, None)?.into()
                    })
                },
                (Some(_), None, None) => {
                    last_op.right = _parse(lex, this_op, None)?.into();
                    Ok(last_op)
                },
                (Some(_), Some(item), None) => Ok(Operator {
                    kind: OpKind::Apposition, 
                    left: Some(item), 
                    right: _parse(lex, this_op, None)?.into()
                }),
                (None, None, None) => Ok(Operator { 
                    kind: OpKind::Apposition, 
                    left: last_op.into(), 
                    right: _parse(lex, this_op, None)?.into()
                }),
                (None, Some(item), None) => Ok(Operator {
                    kind: OpKind::Apposition,
                    left: last_op.into(),
                    right: Operator {
                        kind: OpKind::Apposition,
                        left: item.into(),
                        right: _parse(lex, this_op, None)?.into()
                    }.into()
                })
            }
        }
    }
}

#[inline]
fn parse_literal(lex: &mut Lexer<Token>, last_op: Operator, item: Option<Box<CST>>, literal: CST) -> Result<Operator, ParseError> {
    if item.is_some() {
        match last_op.right_slot() {
            Some(last_slot) => parse_comparison(
                lex, 
                last_op, last_slot, 
                Operator::new(OpKind::Apposition), APPOSITION_LEFT_SLOT, 
                item, Some(Box::new(literal))
            ),
            None => {
                let this_op = Operator {
                    kind: OpKind::Apposition,
                    left: Some(Box::new(CST::Op(last_op))),
                    right: None
                };
                _parse(lex, this_op, Some(Box::new(literal)))
            }
        }
    }
    else {
        _parse(lex, last_op, Some(Box::new(literal)))
    }
}

#[inline]
fn parse_comparison(
    lex: &mut Lexer<Token>, 
    mut last_op: Operator,  last_slot: OperatorSlot, 
    mut this_op: Operator,  this_slot: OperatorSlot, 
    item: Option<Box<CST>>, next_item: Option<Box<CST>>
) -> Result<Operator, ParseError> {
    if last_slot.takes_precedence_over(&this_slot)? {
        last_op.right = item;
        this_op.left = Some(Box::new(CST::Op(last_op)));
        _parse(lex, this_op, next_item)
    }
    else {
        this_op.left = item;
        last_op.right = Some(Box::new(_parse(lex, this_op, next_item)?.into()));
        Ok(last_op)
    }
}


#[derive(Debug)]
pub enum ParseError {
    OperatorConflict,
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

    fn rooted(op: Operator) -> CST {
        CST::Op(Operator {
            kind: OpKind::Root,
            left: None,
            right: op.into()
        })
    }

    #[test]
    fn arithmetic() -> Result<(), ParseError> {
        assert_eq!(parse(&mut Token::lexer("x * y + z"))?, rooted(Operator {
            kind: OpKind::Add,
            left: Operator {
                kind: OpKind::Mul,
                left: CST::Id("x".to_string()).into(),
                right: CST::Id("y".to_string()).into()
            }.into(),
            right: CST::Id("z".to_string()).into()
        }));
        Ok(())
    }

    #[test]
    fn arithmetic_2() -> Result<(), ParseError> {
        assert_eq!(parse(&mut Token::lexer("2 % x - yy / 44"))?, rooted(Operator {
            kind: OpKind::Sub,
            left: Operator {
                kind: OpKind::Mod,
                left: CST::Nat(2).into(),
                right: CST::Id("x".to_string()).into()
            }.into(),
            right: Operator {
                kind: OpKind::Div,
                left: CST::Id("yy".to_string()).into(),
                right: CST::Nat(44).into()
            }.into()
        }));
        Ok(())
    }

    fn assert_left_assoc(s: &str, op_kind: OpKind) -> Result<(), ParseError> {
        assert_eq!(parse(&mut Token::lexer(s))?, rooted(Operator {
            kind: op_kind,
            left: Operator {
                kind: op_kind,
                left: CST::Id("x".to_string()).into(),
                right: CST::Id("y".to_string()).into()
            }.into(),
            right: CST::Id("z".to_string()).into(),
        }));
        Ok(())
    }

    fn assert_right_assoc(s: &str, op_kind: OpKind) -> Result<(), ParseError> {
        assert_eq!(parse(&mut Token::lexer(s))?, rooted(Operator {
            kind: op_kind,
            left: CST::Id("x".to_string()).into(),
            right: Operator {
                kind: op_kind,
                left: CST::Id("y".to_string()).into(),
                right: CST::Id("z".to_string()).into()
            }.into(),
        }));
        Ok(())
    }

    #[test]
    fn addition_left_assoc() -> Result<(), ParseError> {
        assert_left_assoc("x + y + z", OpKind::Add)
    }

    #[test]
    fn subtraction_left_assoc() -> Result<(), ParseError> {
        assert_left_assoc("x - y - z", OpKind::Sub)
    }

    #[test]
    fn multiplication_left_assoc() -> Result<(), ParseError> {
        assert_left_assoc("x * y * z", OpKind::Mul)
    }

    #[test]
    fn division_left_assoc() -> Result<(), ParseError> {
        assert_left_assoc("x / y / z", OpKind::Div)
    }

    #[test]
    fn modulo_left_assoc() -> Result<(), ParseError> {
        assert_left_assoc("x % y % z", OpKind::Mod)
    }

    #[test]
    fn apposition_right_assoc() -> Result<(), ParseError> {
        assert_right_assoc("x y z", OpKind::Apposition)
    }
}
