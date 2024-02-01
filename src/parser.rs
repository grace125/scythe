use logos::{Logos, Lexer};
use std::num::{ParseIntError, ParseFloatError};
pub use OpKind::*;



const IF_GROUP: u32                 = 0b___________1;
const THEN_GROUP: u32               = 0b__________10;
const ELSE_GROUP: u32               = 0b_________100;
const COLON_LEFT_GROUP: u32         = 0b________1000;
const COLON_RIGHT_GROUP: u32        = 0b_______10000;
const FUNC_RIGHT_GROUP: u32         = 0b______100000;
const FUNC_LEFT_GROUP: u32          = 0b_____1000000;
const SUM_GROUP: u32                = 0b____10000000;
const PROD_GROUP: u32               = 0b___100000000;
const APPOSITION_RIGHT_GROUP: u32   = 0b__1000000000;
const APPOSITION_LEFT_GROUP: u32    = 0b_10000000000;



const ROOT_SLOT: Slot               = Slot::new(0, 0);
const IF_SLOT: Slot                 = Slot::new(IF_GROUP, 0);
const THEN_SLOT: Slot               = Slot::new(THEN_GROUP, 0);
const ELSE_SLOT: Slot               = Slot::new(ELSE_GROUP, 0);
const COLON_LEFT_SLOT: Slot         = Slot::new(COLON_LEFT_GROUP, FUNC_RIGHT_GROUP | ELSE_GROUP);
const COLON_RIGHT_SLOT: Slot        = Slot::new(COLON_RIGHT_GROUP, FUNC_LEFT_GROUP | SUM_GROUP | PROD_GROUP);
const FUNC_RIGHT_SLOT: Slot         = Slot::new(FUNC_RIGHT_GROUP, 0);
const FUNC_LEFT_SLOT: Slot          = Slot::new(FUNC_LEFT_GROUP, 0);
const SUM_SLOT: Slot                = Slot::new(SUM_GROUP, 0);
const PROD_SLOT: Slot               = Slot::new(PROD_GROUP, 0);
const APPOSITION_LEFT_SLOT: Slot    = Slot::new(APPOSITION_LEFT_GROUP, ELSE_GROUP);
const APPOSITION_RIGHT_SLOT: Slot   = Slot::new(APPOSITION_RIGHT_GROUP, SUM_GROUP | PROD_GROUP);




#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex(r"[a-zA-Z_][0-9a-zA-Z_]*", |lex| lex.slice().to_string(), priority = 1)]
    Id(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse(), priority = 2)] 
    Nat(u64),

    #[regex(r"[0-9]+\.[0-9]*", |lex| lex.slice().parse(), priority = 2)]
    Float(f64),

    // #[regex("\"\"")]
    // String,

    #[regex(r"([\[\]{}();:,+*-/%]|=>|if|then|else)", |lex| op_kind_from_str(lex.slice()), priority = 2)]
    Op(OpKind),
}

fn op_kind_from_str(s: &str) -> OpKind {
    match s {
        "+" => OpKind::Add,
        "*" => OpKind::Mul,
        "-" => OpKind::Sub,
        "/" => OpKind::Div,
        "%" => OpKind::Mod,
        "if" => OpKind::If,
        "then" => OpKind::Then,
        "else" => OpKind::Else,
        "=>" => OpKind::Func,
        ":" => OpKind::Colon,
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
    If,
    Then,
    Else,
    Func,
    Colon,
    Apposition,
    Root,
}

#[derive(Clone, PartialEq)]
pub struct Op {
    kind: OpKind,
    left: Option<Box<CST>>,
    right: Option<Box<CST>>
}

pub fn op(kind: OpKind, left: impl Into<Option<Box<CST>>>, right: impl Into<Option<Box<CST>>>) -> Op {
    Op { kind, left: left.into(), right: right.into() }
}

impl std::fmt::Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Op { kind, left, right } = self;
        
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

impl Into<CST> for Op {
    fn into(self) -> CST {
        CST::Op(self)
    }
}

impl Into<Box<CST>> for Op {
    fn into(self) -> Box<CST> {
        Box::new(CST::Op(self))
    }
}

impl Into<Option<Box<CST>>> for Op {
    fn into(self) -> Option<Box<CST>> {
        Some(Box::new(CST::Op(self)))
    }
}

impl Op {
    pub fn new(kind: OpKind) -> Op {
        Op { 
            kind, 
            left: None,
            right: None 
        }
    }

    fn left_slot(&self) -> Option<Slot> {
        match self.kind {
            OpKind::Add | OpKind::Sub               => Some(SUM_SLOT),
            OpKind::Mul | OpKind::Div | OpKind::Mod => Some(PROD_SLOT),
            OpKind::Apposition => Some(APPOSITION_LEFT_SLOT),
            OpKind::Root => Some(ROOT_SLOT),
            OpKind::If => None,
            OpKind::Then => Some(THEN_SLOT),
            OpKind::Else => Some(ELSE_SLOT),
            OpKind::Func => Some(FUNC_LEFT_SLOT),
            OpKind::Colon => Some(COLON_LEFT_SLOT)
        }
    }

    fn right_slot(&self) -> Option<Slot> {
        match self.kind {
            OpKind::Add | OpKind::Sub => Some(
                if self.left.is_none() { PROD_SLOT } else { SUM_SLOT }
            ),
            OpKind::Mul | OpKind::Div | OpKind::Mod => Some(PROD_SLOT),
            OpKind::Apposition => Some(APPOSITION_RIGHT_SLOT),
            OpKind::Root => Some(ROOT_SLOT),
            OpKind::If => Some(IF_SLOT),
            OpKind::Then => Some(THEN_SLOT),
            OpKind::Else => Some(ELSE_SLOT),
            OpKind::Func => Some(FUNC_RIGHT_SLOT),
            OpKind::Colon => Some(COLON_RIGHT_SLOT)
        }
    }
}

pub struct Slot {
    precedence: u32,
    conflicts: u32,
}

impl Slot {
    const fn new(precedence: u32, conflicts: u32) -> Self {
        Slot { precedence, conflicts }
    }

    fn takes_precedence_over(&self, other: &Slot) -> Result<bool, ParseError> {
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
    Op(Op),
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
    _parse(lex, Op::new(OpKind::Root), None).map(|op| CST::Op(op))
}

fn _parse(lex: &mut Lexer<Token>, mut last_op: Op, item: Option<Box<CST>>) -> Result<Op, ParseError> {
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
            let mut this_op = Op::new(op_kind);
            match (last_op.right_slot(), item, this_op.left_slot()) {
                (Some(last_slot), item, Some(this_slot)) => parse_comparison(lex, last_op, last_slot, this_op, this_slot, item, None),
                (None, None, Some(_)) => {
                    this_op.left = Some(Box::new(CST::Op(last_op)));
                    _parse(lex, this_op, None)
                },
                (None, Some(item), Some(_)) => {
                    this_op.left = Some(item);
                    Ok(Op {
                        kind: OpKind::Apposition, 
                        left: last_op.into(), 
                        right: _parse(lex, this_op, None)?.into()
                    })
                },
                (Some(_), None, None) => {
                    last_op.right = _parse(lex, this_op, None)?.into();
                    Ok(last_op)
                },
                (Some(_), Some(item), None) => Ok(Op {
                    kind: OpKind::Apposition, 
                    left: Some(item), 
                    right: _parse(lex, this_op, None)?.into()
                }),
                (None, None, None) => Ok(Op { 
                    kind: OpKind::Apposition, 
                    left: last_op.into(), 
                    right: _parse(lex, this_op, None)?.into()
                }),
                (None, Some(item), None) => Ok(Op {
                    kind: OpKind::Apposition,
                    left: last_op.into(),
                    right: Op {
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
fn parse_literal(lex: &mut Lexer<Token>, last_op: Op, item: Option<Box<CST>>, literal: CST) -> Result<Op, ParseError> {
    if item.is_some() {
        match last_op.right_slot() {
            Some(last_slot) => parse_comparison(
                lex, 
                last_op, last_slot, 
                Op::new(OpKind::Apposition), APPOSITION_LEFT_SLOT, 
                item, Some(Box::new(literal))
            ),
            None => {
                let this_op = Op {
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
    mut last_op: Op,  last_slot: Slot, 
    mut this_op: Op,  this_slot: Slot, 
    item: Option<Box<CST>>, next_item: Option<Box<CST>>
) -> Result<Op, ParseError> {
    if last_slot.takes_precedence_over(&this_slot)? {
        last_op.right = item;
        this_op.left = last_op.into();
        _parse(lex, this_op, next_item)
    }
    else {
        this_op.left = item;
        last_op.right = _parse(lex, this_op, next_item)?.into();
        Ok(last_op)
    }
}


#[derive(Debug, PartialEq)]
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

    fn root(op: Op) -> CST {
        CST::Op(Op {
            kind: OpKind::Root,
            left: None,
            right: op.into()
        })
    }

    fn id(s: &str) -> CST {
        CST::Id(s.to_owned())
    }

    fn nat(s: u64) -> CST {
        CST::Nat(s)
    }

    fn if_then_else(condition: impl Into<CST>, case_1: impl Into<CST>, case_2: impl Into<CST>) -> Op {
        op(If, None, op(Then, condition.into(), op(Else, case_1.into(), case_2.into())))
    }

    fn if_then(condition: impl Into<CST>, case: impl Into<CST>) -> Op {
        op(If, None, op(Then, condition.into(), case.into()))
    }

    fn assert_left_assoc(s: &str, kind: OpKind, id1: CST, id2: CST, id3: CST) -> Result<(), ParseError> {
        Ok(assert_eq!(parse(&mut Token::lexer(s))?, root(op(kind, op(kind, id1, id2), id3))))
    }
    fn assert_right_assoc(s: &str, kind: OpKind, id1: CST, id2: CST, id3: CST) -> Result<(), ParseError> {
        Ok(assert_eq!(parse(&mut Token::lexer(s))?, root(op(kind, id1, op(kind, id2, id3)))))
    }

    mod apposition {
        use super::*;

        #[test]
        fn apposition_right_assoc() -> Result<(), ParseError> {
            assert_right_assoc("x y z", Apposition, id("x"), id("y"), id("z"))
        }
    }

    mod brackets {
        // use super::*;
    }

    mod arithmetic {
        use super::*;

        #[test]
        fn addition_left_assoc() -> Result<(), ParseError> {
            assert_left_assoc("x + y + z", Add, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn subtraction_left_assoc() -> Result<(), ParseError> {
            assert_left_assoc("x - y - z", Sub, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn multiplication_left_assoc() -> Result<(), ParseError> {
            assert_left_assoc("x * y * z", Mul, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn division_left_assoc() -> Result<(), ParseError> {
            assert_left_assoc("x / y / z", Div, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn modulo_left_assoc() -> Result<(), ParseError> {
            assert_left_assoc("x % y % z", Mod, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn arithmetic() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("x * y + z"))?, 
                root(op(Add, op(Mul, id("x"), id("y")), id("z")))
            ))
        }
    
        #[test]
        fn arithmetic_2() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("2 % x - yy / 44"))?, 
                root(op(Sub, op(Mod, nat(2), id("x")), op(Div, id("yy"), nat(44))))
            ))
        }
    
        #[test]
        fn unary_subtraction() -> Result<(), ParseError> {
            Ok(assert_eq!(parse(&mut Token::lexer("-a * b"))?, root(op(Mul, op(Sub, None, id("a")), id("b")))))
        }
    
        #[test]
        fn apposition_addition_conflict() {
            assert_eq!(parse(&mut Token::lexer("f x + y")), Err(ParseError::OperatorConflict));
        }
    
        #[test]
        fn addition_apposition() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("x + f y"))?, 
                root(op(Add, id("x"), op(Apposition, id("f"), id("y"))))
            ))
        }
    
    }

    mod if_then_conditionals {
        use super::*;
            
        #[test]
        fn if_then_else_right_assoc() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("if x then y else z"))?, 
                root(if_then_else(id("x"), id("y"), id("z")))
            ))
        }

        #[test]
        fn else_if() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("if a then b else if c then d else e"))?,
                root(if_then_else(id("a"), id("b"), if_then_else(id("c"), id("d"), id("e"))))
            ))
        }

        #[test]
        fn else_apposition_conflict() {
            assert_eq!(parse(&mut Token::lexer("if a then f else g x")), Err(ParseError::OperatorConflict));
        }

        #[test]
        fn if_then_apposition() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("a if f x then g y"))?,
                root(op(Apposition, id("a"), if_then(
                    op(Apposition, id("f"), id("x")), 
                    op(Apposition, id("g"), id("y"))
                )))
            ))
        }

        #[test]
        fn if_then_else_arithmetic() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("if w + x then x % y else y * z"))?, 
                root(if_then_else(op(Add, id("w"), id("x")), op(Mod, id("x"), id("y")), op(Mul, id("y"), id("z"))))
            ))
        }
    }

    mod func {
        use super::*;
        
        #[test]
        fn func_right_assoc() -> Result<(), ParseError> {
            assert_right_assoc("x => y => z", Func, id("x"), id("y"), id("z"))
        }

        // TODO: figure out semantics for this case, or make it a conflict
        #[test]
        fn apposition_func() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("map x => x + x"))?,
                root(op(Apposition, id("map"), op(Func, id("x"), op(Add, id("x"), id("x")))))
            ))
        }

        #[test]
        fn func_apposition_conflict() {
            assert_eq!(
                parse(&mut Token::lexer("a => f x")),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn func_arithmetic() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("A * B => a + b"))?,
                root(op(Func, op(Mul, id("A"), id("B")), op(Add, id("a"), id("b"))))
            ))
        }

        #[test]
        fn func_if_then_else() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("a => if a then 1 else x"))?,
                root(op(Func, id("a"), if_then_else(id("a"), nat(1), id("x"))))
            ))
        }

        #[test]
        fn if_func_then_func_else_func() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("if a => b then c => d else e => f"))?,
                root(if_then_else(
                    op(Func, id("a"), id("b")), 
                    op(Func, id("c"), id("d")), 
                    op(Func, id("e"), id("f"))
                ))
            ))
        }
    }

    mod colon {
        use super::*;

        // FIXME: should this be a conflict?
        #[test]
        fn colon_left_assoc() -> Result<(), ParseError> {
            assert_left_assoc("a: A: Type", Colon, id("a"), id("A"), id("Type"))
        }

        #[test]
        fn colon_apposition() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("list: Vec T n"))?,
                root(op(Colon, id("list"), op(Apposition, id("Vec"), op(Apposition, id("T"), id("n")))))
            ))
        }

        // FIXME: should this be a conflict?
        #[test]
        fn apposition_colon() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("f x: T"))?,
                root(op(Colon, op(Apposition, id("f"), id("x")), id("T")))
            ))
        }

        #[test]
        fn arithmetic_colon() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("a / b: A"))?,
                root(op(Colon, op(Div, id("a"), id("b")), id("A")))
            ))
        }

        #[test]
        fn colon_arithmetic_conflict() {
            assert_eq!(
                parse(&mut Token::lexer("a: A * B")),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn colon_if_then() -> Result<(), ParseError> {
            Ok(assert_eq!(
                parse(&mut Token::lexer("if a: A then b: B"))?,
                root(if_then(
                    op(Colon, id("a"), id("A")),
                    op(Colon, id("b"), id("B")),
                ))
            ))
        }

        #[test]
        fn colon_else_conflict() {
            assert_eq!(
                parse(&mut Token::lexer("if a then b else c: D")),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn colon_func_conflict() {
            assert_eq!(
                parse(&mut Token::lexer("a: A => b")),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn func_colon_conflict() {
            assert_eq!(
                parse(&mut Token::lexer("a => a: B")),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn func_colon_conflict_2() {
            assert_eq!(
                parse(&mut Token::lexer("a: A + A => a + b")),
                Err(ParseError::OperatorConflict)
            )
        }
    }

    mod func_type {
        // use super::*;

    }
}
