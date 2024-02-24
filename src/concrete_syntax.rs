use std::collections::VecDeque;

use super::lexing::{OpKind, LexingError};
pub use OpKind::*;

const TEMPLATE_PRECEDENCE: u32          = 0;
const BRACE_PRECEDENCE: u32             = 1;
const SEMICOLON_RIGHT_PRECEDENCE: u32   = 2;
const SEMICOLON_LEFT_PRECEDENCE: u32    = 3;
const IF_PRECEDENCE: u32                = 4;
const THEN_PRECEDENCE: u32              = 5;
const ELSE_PRECEDENCE: u32              = 6;
const COLON_RIGHT_PRECEDENCE: u32       = 7;
const COLON_LEFT_PRECEDENCE: u32        = 8;
const FUNC_RIGHT_PRECEDENCE: u32        = 9;
const FUNC_LEFT_PRECEDENCE: u32         = 10;
const FUNC_TYPE_RIGHT_PRECEDENCE: u32   = 11;
const FUNC_TYPE_LEFT_PRECEDENCE: u32    = 12;
const SUM_PRECEDENCE: u32               = 13;
const PROD_PRECEDENCE: u32              = 14;
const APPOSITION_RIGHT_PRECEDENCE: u32  = 15;
const APPOSITION_LEFT_PRECEDENCE: u32   = 16;
const NO_SLOT_PRECEDENCE: u32           = u32::MAX;

const BRACE_GROUP: u32              = 0b____________1;
const SEMICOLON_GROUP: u32          = 0b___________10;
const IF_GROUP: u32                 = 0b__________100;
const THEN_GROUP: u32               = 0b_________1000;
const ELSE_GROUP: u32               = 0b________10000;
const COLON_GROUP: u32              = 0b_______100000;
const FUNC_GROUP: u32               = 0b______1000000;
const FUNC_TYPE_GROUP: u32          = 0b_____10000000;
const SUM_GROUP: u32                = 0b____100000000;
const PROD_GROUP: u32               = 0b___1000000000;
const APPOSITION_GROUP: u32         = 0b__10000000000;

pub struct Slot {
    precedence: u32,
    conflict_groups: u32,
    conflict_masks: u32
}

impl Slot {
    pub const ROOT_SLOT: Slot               = Slot::new(TEMPLATE_PRECEDENCE,            0,                      0);
    pub const BRACE_SLOT: Slot              = Slot::new(BRACE_PRECEDENCE,               BRACE_GROUP,            0);
    pub const SEMICOLON_LEFT_SLOT: Slot     = Slot::new(SEMICOLON_LEFT_PRECEDENCE,      SEMICOLON_GROUP,        0);
    pub const SEMICOLON_RIGHT_SLOT: Slot    = Slot::new(SEMICOLON_RIGHT_PRECEDENCE,     SEMICOLON_GROUP,        0);
    pub const IF_SLOT: Slot                 = Slot::new(IF_PRECEDENCE,                  IF_GROUP,               0);
    pub const THEN_SLOT: Slot               = Slot::new(THEN_PRECEDENCE,                THEN_GROUP,             0);
    pub const ELSE_SLOT: Slot               = Slot::new(ELSE_PRECEDENCE,                ELSE_GROUP,             0);
    pub const COLON_LEFT_SLOT: Slot         = Slot::new(COLON_LEFT_PRECEDENCE,          COLON_GROUP,            FUNC_GROUP | ELSE_GROUP);
    pub const COLON_RIGHT_SLOT: Slot        = Slot::new(COLON_RIGHT_PRECEDENCE,         COLON_GROUP,            FUNC_GROUP | SUM_GROUP | PROD_GROUP);
    pub const FUNC_RIGHT_SLOT: Slot         = Slot::new(FUNC_RIGHT_PRECEDENCE,          FUNC_GROUP,             0);
    pub const FUNC_LEFT_SLOT: Slot          = Slot::new(FUNC_LEFT_PRECEDENCE,           FUNC_GROUP,             0);
    pub const FUNC_TYPE_RIGHT_SLOT: Slot    = Slot::new(FUNC_TYPE_RIGHT_PRECEDENCE,     FUNC_TYPE_GROUP,        FUNC_GROUP);
    pub const FUNC_TYPE_LEFT_SLOT: Slot     = Slot::new(FUNC_TYPE_LEFT_PRECEDENCE,      FUNC_TYPE_GROUP,        0);
    pub const SUM_SLOT: Slot                = Slot::new(SUM_PRECEDENCE,                 SUM_GROUP,              0);
    pub const PROD_SLOT: Slot               = Slot::new(PROD_PRECEDENCE,                PROD_GROUP,             0);
    pub const APPOSITION_RIGHT_SLOT: Slot   = Slot::new(APPOSITION_RIGHT_PRECEDENCE,    APPOSITION_GROUP,       FUNC_GROUP | FUNC_TYPE_GROUP);
    pub const APPOSITION_LEFT_SLOT: Slot    = Slot::new(APPOSITION_LEFT_PRECEDENCE,     APPOSITION_GROUP,       ELSE_GROUP);
    pub const NO_SLOT: Slot                 = Slot::new(NO_SLOT_PRECEDENCE,             0,                      0);

    const fn new(precedence: u32, conflict_groups: u32, conflict_masks: u32) -> Self {
        Slot { precedence, conflict_groups, conflict_masks }
    }

    fn conflicts_with(&self, other: &Slot) -> Result<(), ParseError> {
        if (self.conflict_groups & other.conflict_masks) | (other.conflict_groups & self.conflict_masks) != 0 {
            Err(ParseError::OperatorConflict)
        }
        else {
            Ok(())
        }
    }

    fn takes_precedence_over(&self, other: &Slot) -> bool {
        self.precedence >= other.precedence
    }

    const fn with_template_precedence(mut self) -> Slot {
        self.precedence = TEMPLATE_PRECEDENCE;
        return self;
    }
}

impl PartialEq for Slot {
    fn eq(&self, other: &Self) -> bool {
        self.precedence == other.precedence
    }
}

pub enum TemplateOutput {
    Item(CST),
    Op(Op),
    Miss(Op, Option<CST>, Op),
}

#[derive(Clone, PartialEq)]
pub struct Op {
    kind: OpKind,
    has_left: bool,
    children: VecDeque<CST>
}

impl Op {
    pub fn new(kind: OpKind) -> Op {
        Op { kind, has_left: false, children: VecDeque::new() }
    }

    pub fn left_slot(&self) -> Slot {
        if self.has_left { return Slot::NO_SLOT; }
        match self.kind {
            OpKind::Brace | 
            OpKind::Bracket | 
            OpKind::Parenthesis |
            OpKind::If                  => Slot::NO_SLOT,
            OpKind::BraceEnd | 
            OpKind::BracketEnd | 
            OpKind::ParenthesisEnd      => Slot::BRACE_SLOT.with_template_precedence(),
            OpKind::Add | 
            OpKind::Sub                 => Slot::SUM_SLOT,
            OpKind::Mul | 
            OpKind::Div | 
            OpKind::Mod                 => Slot::PROD_SLOT,
            OpKind::Apposition          => Slot::APPOSITION_LEFT_SLOT,
            OpKind::Root                => Slot::ROOT_SLOT,
            OpKind::Then                => Slot::THEN_SLOT.with_template_precedence(),
            OpKind::Else                => Slot::ELSE_SLOT.with_template_precedence(),
            OpKind::Semicolon           => Slot::SEMICOLON_LEFT_SLOT,
            OpKind::Func                => Slot::FUNC_LEFT_SLOT,
            OpKind::FuncType            => Slot::FUNC_TYPE_LEFT_SLOT,
            OpKind::Colon               => Slot::COLON_LEFT_SLOT,
        }
    }

    pub fn right_slot(&self) -> Slot {
        match self.kind {
            OpKind::Brace | 
            OpKind::Bracket | 
            OpKind::Parenthesis    => Slot::BRACE_SLOT,
            OpKind::BraceEnd | 
            OpKind::BracketEnd | 
            OpKind::ParenthesisEnd      => unreachable!(),
            OpKind::Add | 
            OpKind::Sub                 => if !self.has_left && self.children.len() == 0 { Slot::PROD_SLOT } else { Slot::SUM_SLOT },
            OpKind::Mul | 
            OpKind::Div | 
            OpKind::Mod                 => Slot::PROD_SLOT,
            OpKind::Apposition          => Slot::APPOSITION_RIGHT_SLOT,
            OpKind::Root                => Slot::ROOT_SLOT,
            OpKind::If                  => match self.children.len() {
                0 => Slot::IF_SLOT,
                1 => Slot::THEN_SLOT,
                _ => Slot::ELSE_SLOT
            },
            OpKind::Then                => unreachable!(),
            OpKind::Else                => unreachable!(),
            OpKind::Semicolon           => Slot::SEMICOLON_RIGHT_SLOT,
            OpKind::Func                => Slot::FUNC_RIGHT_SLOT,
            OpKind::FuncType            => Slot::FUNC_TYPE_RIGHT_SLOT,
            OpKind::Colon               => Slot::COLON_RIGHT_SLOT
        }
    }

    fn push_left(&mut self, l: CST) {
        debug_assert!(!self.has_left);
        self.has_left = true;
        self.children.push_front(l);
    }

    fn push_right(&mut self, r: CST) {
        self.children.push_back(r);
    }

    fn push_left_option(&mut self, l: Option<CST>) {
        if let Some(l) = l {
            self.push_left(l);
        }
    }

    fn push_right_option(&mut self, r: Option<CST>) {
        if let Some(r) = r {
            self.push_right(r);
        }
    }

    fn template_exclusive_op(&self) -> Result<(), ParseError> {
        match self.kind {
            OpKind::BraceEnd | OpKind::BracketEnd | OpKind::ParenthesisEnd | OpKind::Then | OpKind::Else => Err(ParseError::UnusedTemplate(self.kind)),
            OpKind::Brace | OpKind::Bracket | OpKind::Parenthesis | 
            OpKind::Add | OpKind::Sub | OpKind::Mul | OpKind::Div | OpKind::Mod | 
            OpKind::If | OpKind::Semicolon | OpKind::Func | OpKind::FuncType | 
            OpKind::Colon | OpKind::Apposition | OpKind::Root                                           => Ok(())   
        }
    }

    // TODO: name this better
    fn template(mut self, item: Option<CST>, other: Op) -> Result<TemplateOutput, ParseError> {
        match (self.kind, other.kind) {
            (OpKind::Brace, OpKind::BraceEnd) | 
            (OpKind::Bracket, OpKind::BracketEnd) | 
            (OpKind::Parenthesis, OpKind::ParenthesisEnd) => {
                self.push_right_option(item);
                Ok(TemplateOutput::Item(self.try_into()?))
            },
            (OpKind::If, OpKind::Then) => {
                if self.children.len() == 0 {
                    let Some(item) = item else {
                        return Err(ParseError::InvalidEmptySlot("Expected condition between `if` and `else`, got nothing".to_owned()))
                    };
                    self.push_right(item);
                    Ok(TemplateOutput::Op(self))
                }
                else {
                    Err(ParseError::InvalidTemplate("`if` statement has more than one `then`".to_owned()))
                }
            },
            (OpKind::If, OpKind::Else) => match self.children.len() {
                0 => Err(ParseError::InvalidTemplate("Expected `then`, got `else`".to_owned())),
                1 => if let Some(item) = item {
                    self.push_right(item);
                    Ok(TemplateOutput::Op(self))
                } else {
                    Err(ParseError::InvalidEmptySlot("Expected branch between `then` and `else`, got nothing".to_owned()))
                },
                _ => Err(ParseError::InvalidTemplate("`if` statement has more than one `else`".to_owned()))
            }
            _ => Ok(TemplateOutput::Miss(self, item, other))
        }
    }
}

impl std::fmt::Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Op { kind, has_left, children } = self;
        
        f.write_fmt(format_args!("{:?}(", kind))?;
        if children.is_empty() {
            f.write_str(",)")
        }
        else {
            if !has_left {
                f.write_str(", ")?;
            }
            let mut iter = children.iter();
            let mut r = iter.next().unwrap();
            loop {
                (*r).fmt(f)?;
                if let Some(n) = iter.next() {
                    r = n;
                    f.write_str(", ")?;
                }
                else {
                    break;
                }
            }
            f.write_str(")")
        }
    }
}

impl TryInto<CST> for Op {
    type Error = ParseError;

    fn try_into(self) -> Result<CST, Self::Error> {
        match (self.kind, self.children.len()) {
            (OpKind::Then, _) | 
            (OpKind::Else, _) | 
            (OpKind::BraceEnd, _) | 
            (OpKind::BracketEnd, _) | 
            (OpKind::ParenthesisEnd, _)             => unreachable!(),
            (OpKind::If, 2 | 3) | 
            (OpKind::Add, 1 | 2) | 
            (OpKind::Sub, 1 | 2) |
            (OpKind::Mul, 2) | 
            (OpKind::Div, 2) | 
            (OpKind::Mod, 2) |
            (OpKind::Apposition, 2) | 
            (OpKind::Colon, 2) |
            (OpKind::Semicolon, 1 | 2) |
            (OpKind::Func, 2) |
            (OpKind::FuncType, 2) |
            (OpKind::Root, 0) |
            (OpKind::Brace, 1) | 
            (OpKind::Bracket, 1) | 
            (OpKind::Parenthesis, 1)                => Ok(CST::Op(self)),
            (OpKind::If, _) |
            (OpKind::Add, _) |
            (OpKind::Sub, _) |
            (OpKind::Mul, _) | 
            (OpKind::Div, _) | 
            (OpKind::Mod, _) |
            (OpKind::Apposition, _) | 
            (OpKind::Colon, _) |
            (OpKind::Semicolon, _) |
            (OpKind::Func, _) |
            (OpKind::FuncType, _) |
            (OpKind::Root, _) |
            (OpKind::Brace, _) | 
            (OpKind::Bracket, _) | 
            (OpKind::Parenthesis, _)                => Err(ParseError::IncompleteOperator(self)),
        }
    }
}

impl From<OpKind> for Op {
    fn from(value: OpKind) -> Op {
        Op::new(value)
    }
}

#[derive(Clone, PartialEq)]
pub enum CST {
    Op(Op),
    Nat(u64),
    Float(f64),
    Id(String),
}

impl CST {
    pub fn id_from(s: impl Into<String>) -> Self {
        CST::Id(s.into())
    }

    pub fn op_from(k: impl Into<Op>) -> Self {
        CST::Op(k.into())
    }

    pub fn nat_from(n: u64) -> Self {
        CST::Nat(n)
    }
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

pub fn parse<L: Iterator<Item = Result<CST, LexingError>>>(lex: &mut L) -> Result<Option<CST>, ParseError> {
    let (first_item, first_op) = next(lex, None)?;

    let Some(mut first_op) = first_op else {
        return Ok(first_item);
    };

    first_op.push_left_option(first_item);

    loop {
        let (new_item, new_op) = parse_inner(lex, first_op)?;
        match new_op {
            None => break Ok(new_item),
            Some(mut new_op) => {
                new_op.push_left_option(new_item);
                first_op = new_op;
            }
        }
    }
}

fn parse_inner<L>(lex: &mut L, mut prev_op: Op) -> Result<(Option<CST>, Option<Op>), ParseError> 
where L: Iterator<Item = Result<CST, LexingError>>
{
    prev_op.template_exclusive_op()?;

    let (mut item, mut maybe_this_op) = next(lex, None)?;

    let prev_slot = prev_op.right_slot();

    loop {
        let Some(mut this_op) = maybe_this_op else {
            prev_op.push_right_option(item);
            return Ok((Some(prev_op.try_into()?), None));
        };

        let this_slot = this_op.left_slot();

        prev_slot.conflicts_with(&this_slot)?;

        (prev_op, item, this_op) = match prev_op.template(item, this_op)? {
            TemplateOutput::Item(item) => {
                return next(lex, Some(item));
            },
            TemplateOutput::Op(new_op) => {
                return parse_inner(lex, new_op);
            }
            TemplateOutput::Miss(prev_op, item, this_op) => (prev_op, item, this_op),
        };

        if prev_slot.takes_precedence_over(&this_slot) {
            prev_op.push_right_option(item);
            return Ok((Some(prev_op.try_into()?), Some(this_op)));
        }

        this_op.push_left_option(item);
    
        (item, maybe_this_op) = parse_inner(lex, this_op)?;
    }

}

fn next<L>(lex: &mut L, item: Option<CST>) -> Result<(Option<CST>, Option<Op>), ParseError> 
where L: Iterator<Item = Result<CST, LexingError>>
{
    match lex.next() {
        Some(Ok(CST::Op(op))) => Ok((item, Some(op))),
        Some(Err(e)) => Err(e.into()),
        Some(Ok(new_item)) => {
            debug_assert!(item.is_none());
            next(lex, new_item.into())
        },
        None => Ok((item, None))
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
    InvalidTemplate(String),
    UnusedTemplate(OpKind),
    IncompleteOperator(Op)
}

impl From<LexingError> for ParseError {
    fn from(value: LexingError) -> Self {
        ParseError::LexingError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use helpers::*;
    use crate::lexing::lex;

    mod helpers {
        use super::*;

        pub fn parse_str(s: &str) -> Result<Option<CST>, ParseError> {
            parse(&mut lex(s))
        }
    
        pub fn root(cst: CST) -> Result<Option<CST>, ParseError> {
            Ok(Some(cst))
        }
    
        pub fn id(s: &str) -> CST {
            CST::Id(s.to_owned())
        }
    
        pub fn nat(s: u64) -> CST {
            CST::Nat(s)
        }
    
        pub fn bracket(item: CST) -> CST {
            Op { 
                kind: Bracket, 
                has_left: false,
                children: VecDeque::from(vec![item])
            }.try_into().unwrap()
        }
    
        pub fn brace(item: CST) -> CST {
            Op { 
                kind: Brace, 
                has_left: false,
                children: VecDeque::from(vec![item])
            }.try_into().unwrap()
        }
    
        pub fn parenthesis(item: CST) -> CST {
            Op { 
                kind: Parenthesis, 
                has_left: false,
                children: VecDeque::from(vec![item])
            }.try_into().unwrap()
        }
    
        pub fn if_then_else(condition: CST, case_1: CST, case_2: CST) -> CST {
            Op { 
                kind: If, 
                has_left: false,
                children: VecDeque::from(vec![condition, case_1, case_2])
            }.try_into().unwrap()
        }
    
        pub fn if_then(condition: CST, case: CST) -> CST {
            Op { 
                kind: If, 
                has_left: false,
                children: VecDeque::from(vec![condition, case])
            }.try_into().unwrap()
        }

        pub fn apposition(left: CST, right: CST) -> CST {
            Op {
                kind: Apposition,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn add(left: CST, right: CST) -> CST {
            Op {
                kind: Add,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn sub(left: CST, right: CST) -> CST {
            Op {
                kind: Sub,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn unary_add(item: CST) -> CST {
            Op {
                kind: Add,
                has_left: false,
                children: VecDeque::from(vec![item])
            }.try_into().unwrap()
        }

        pub fn unary_sub(item: CST) -> CST {
            Op {
                kind: Sub,
                has_left: false,
                children: VecDeque::from(vec![item])
            }.try_into().unwrap()
        }

        pub fn mul(left: CST, right: CST) -> CST {
            Op {
                kind: Mul,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn div(left: CST, right: CST) -> CST {
            Op {
                kind: Div,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn modulo(left: CST, right: CST) -> CST {
            Op {
                kind: Mod,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn func(left: CST, right: CST) -> CST {
            Op {
                kind: Func,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn func_type(left: CST, right: CST) -> CST {
            Op {
                kind: FuncType,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn colon(left: CST, right: CST) -> CST {
            Op {
                kind: Colon,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn semicolon(left: CST, right: CST) -> CST {
            Op {
                kind: Semicolon,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }
    }

    mod apposition {
        use super::*;

        #[test]
        fn apposition_right_assoc() {
            assert_eq!(
                parse_str("x y z"),
                root(apposition(id("x"), apposition(id("y"), id("z"))))
            );
        }
    }
    
    mod arithmetic {
        use super::*;

        #[test]
        fn addition_left_assoc() {
            assert_eq!(
                parse_str("x + y + z"),
                root(add(add(id("x"), id("y")), id("z")))
            );
        }
    
        #[test]
        fn subtraction_left_assoc() {
            assert_eq!(
                parse_str("x - y - z"),
                root(sub(sub(id("x"), id("y")), id("z")))
            );
        }
    
        #[test]
        fn multiplication_left_assoc() {
            assert_eq!(
                parse_str("x * y * z"),
                root(mul(mul(id("x"), id("y")), id("z")))
            );
        }
    
        #[test]
        fn division_left_assoc() {
            assert_eq!(
                parse_str("x / y / z"),
                root(div(div(id("x"), id("y")), id("z")))
            );
        }
    
        #[test]
        fn modulo_left_assoc() {
            assert_eq!(
                parse_str("x % y % z"),
                root(modulo(modulo(id("x"), id("y")), id("z")))
            );
        }
    
        #[test]
        fn arithmetic() {
            assert_eq!(
                parse_str("x * y + z"), 
                root(add(mul(id("x"), id("y")), id("z")))
            );
        }
    
        #[test]
        fn arithmetic_2() {
            assert_eq!(
                parse_str("2 % x - yy / 44"), 
                root(sub(modulo(nat(2), id("x")), div(id("yy"), nat(44))))
            )
        }
    
        #[test]
        fn unary_subtraction() {
            assert_eq!(parse_str("-a * b"), root(mul(unary_sub(id("a")), id("b"))))
        }
    
        #[test]
        fn apposition_addition_conflict() {
            assert_eq!(parse_str("f x + y"), root(add(apposition(id("f"), id("x")), id("y"))));
        }
    
        #[test]
        fn addition_apposition() {
            assert_eq!(
                parse_str("x + f y"), 
                root(add(id("x"), apposition(id("f"), id("y"))))
            )
        }
    
    }

    mod brackets {
        use super::*;

        #[test]
        fn single_bracket() {
            assert_eq!(parse_str("[x]"), root(bracket(id("x"))))
        }

        #[test]
        fn single_brace() {
            assert_eq!(parse_str("{x}"), root(brace(id("x"))))
        }

        #[test]
        fn single_parenthesis() {
            assert_eq!(parse_str("(x)"), root(parenthesis(id("x"))))
        }

        #[test]
        fn parens_arithmetic() {
            assert_eq!(
                parse_str("(x) + 5"),
                root(add(parenthesis(id("x")), nat(5)))
            )
        }

        #[test]
        fn parens_item_arithmetic() {
            assert_eq!(parse_str("(f) 3 + 5"), root(add(apposition(parenthesis(id("f")), nat(3)), nat(5))))
        }

        #[test]
        fn arithmetic_parens() {
            assert_eq!(parse_str("5 + (x)"), root(add(nat(5), parenthesis(id("x")))))
        }

        #[test]
        fn arithmetic_item_parens() {
            assert_eq!(
                parse_str("5 + f(x)"), 
                root(add(nat(5), apposition(id("f"), parenthesis(id("x")))))
            )
        }

        #[test]
        fn parens_parens() {
            assert_eq!(parse_str("(f) (x)"), root(apposition(parenthesis(id("f")), parenthesis(id("x")))))
        }

        #[test]
        fn parens_item_parens() {
            assert_eq!(
                parse_str("(f) a (x)"), 
                root(apposition(parenthesis(id("f")), apposition(id("a"), parenthesis(id("x")))))
            )
        }

        #[test]
        fn parens_around_arithmetic() {
            assert_eq!(
                parse_str("(a + b)"),
                root(parenthesis(add(id("a"), id("b"))))
            )
        }

        #[test]
        fn parens_around_arithmetic_arithmetic() {
            assert_eq!(
                parse_str("(a + b) + (c + d)"),
                root(add(
                    parenthesis(add(id("a"), id("b"))), 
                    parenthesis(add(id("c"), id("d")))
                ))
            )
        }

        #[test]
        fn parens_nested() {
            assert_eq!(
                parse_str("[(c + d)]"),
                root(bracket(parenthesis(add(id("c"), id("d")))))
            )
        }
    }
    
    mod if_then_conditionals {
        use super::*;
            
        #[test]
        fn if_then_else_right_assoc() {
            assert_eq!(
                parse_str("if x then y else z"), 
                root(if_then_else(id("x"), id("y"), id("z")))
            )
        }

        #[test]
        fn else_if() {
            assert_eq!(
                parse_str("if a then b else if c then d else e"),
                root(if_then_else(id("a"), id("b"), if_then_else(id("c"), id("d"), id("e"))))
            )
        }

        #[test]
        fn else_apposition_conflict() {
            assert_eq!(parse_str("if a then f else g x"), Err(ParseError::OperatorConflict));
        }

        #[test]
        fn if_then_apposition() {
            assert_eq!(
                parse_str("a if f x then g y"),
                root(apposition(id("a"), if_then(
                    apposition(id("f"), id("x")), 
                    apposition(id("g"), id("y"))
                )))
            )
        }

        #[test]
        fn if_then_else_arithmetic() {
            assert_eq!(
                parse_str("if w + x then x % y else y * z"), 
                root(if_then_else(add(id("w"), id("x")), modulo(id("x"), id("y")), mul(id("y"), id("z"))))
            )
        }


        #[test]
        fn incomplete_if() {
            assert_eq!(
                parse_str("if a"),
                Err(ParseError::IncompleteOperator(Op { kind: If, has_left: false, children: VecDeque::from(vec![id("a")]) }))
            )
        }
    }
    
    mod func {
        use super::*;
        
        #[test]
        fn func_right_assoc() {
            assert_eq!(
                parse_str("x => y => z"),
                root(func(id("x"), func(id("y"), id("z"))))
            );
        }

        // TODO: figure out semantics for this case, or make it a conflict
        #[test]
        fn apposition_func_conflict() {
            assert_eq!(
                parse_str("map x => x"),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn func_apposition() {
            assert_eq!(parse_str("a => f x"), root(func(id("a"), apposition(id("f"), id("x")))))
        }

        #[test]
        fn func_arithmetic() {
            assert_eq!(
                parse_str("A * B => a + b"),
                root(func(mul(id("A"), id("B")), add(id("a"), id("b"))))
            )
        }

        #[test]
        fn func_if_then_else() {
            assert_eq!(
                parse_str("a => if a then 1 else x"),
                root(func(id("a"), if_then_else(id("a"), nat(1), id("x"))))
            )
        }

        #[test]
        fn if_func_then_func_else_func() {
            assert_eq!(
                parse_str("if a => b then c => d else e => f"),
                root(if_then_else(
                    func(id("a"), id("b")), 
                    func(id("c"), id("d")), 
                    func(id("e"), id("f"))
                ))
            )
        }
    }

    
    mod func_type {
        use super::*;

        #[test]
        fn func_type_right_assoc() {
            assert_eq!(
                parse_str("x -> y -> z"),
                root(func_type(id("x"), func_type(id("y"), id("z"))))
            );
        }

        // TODO: figure out semantics for this case, or make it a conflict
        #[test]
        fn apposition_func_type_conflict() {
            assert_eq!(
                parse_str("map x -> x"),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn func_type_apposition() {
            assert_eq!(parse_str("a -> f x"), root(func_type(id("a"), apposition(id("f"), id("x")))))
        }

        #[test]
        fn func_type_arithmetic() {
            assert_eq!(
                parse_str("A * B -> A + B"),
                root(func_type(mul(id("A"), id("B")), add(id("A"), id("B"))))
            )
        }

        #[test]
        fn func_type_if_then_else() {
            assert_eq!(
                parse_str("a -> if a then A else B"),
                root(func_type(id("a"), if_then_else(id("a"), id("A"), id("B"))))
            )
        }

        #[test]
        fn if_func_type_then_func_type_else_func_type() {
            assert_eq!(
                parse_str("if A -> B then C -> D else E -> F"),
                root(if_then_else(
                    func_type(id("A"), id("B")), 
                    func_type(id("C"), id("D")), 
                    func_type(id("E"), id("F"))
                ))
            )
        }

        #[test]
        fn func_func_type() {
            assert_eq!(
                parse_str("A => A -> B"),
                root(func(id("A"), func_type(id("A"), id("B"))))
            )
        }

        #[test]
        fn func_type_func_conflict() {
            assert_eq!(
                parse_str("A -> B => B"),
                Err(ParseError::OperatorConflict)
            )
        }
    }
    
    
    mod colon {
        use super::*;


        // FIXME: should this be a conflict?
        #[test]
        fn colon_right_assoc() {
            assert_eq!(
                parse_str("a: A: Type"),
                root(colon(id("a"), colon(id("A"), id("Type"))))
            );
        }

        #[test]
        fn colon_apposition() {
            assert_eq!(
                parse_str("list: Vec T n"),
                root(colon(id("list"), apposition(id("Vec"), apposition(id("T"), id("n")))))
            )
        }

        // FIXME: should this be a conflict?
        #[test]
        fn apposition_colon() {
            assert_eq!(
                parse_str("f x: T"),
                root(colon(apposition(id("f"), id("x")), id("T")))
            )
        }

        #[test]
        fn arithmetic_colon() {
            assert_eq!(
                parse_str("a / b: A"),
                root(colon(div(id("a"), id("b")), id("A")))
            )
        }

        #[test]
        fn colon_arithmetic_conflict() {
            assert_eq!(
                parse_str("a: A * B"),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn colon_if_then() {
            assert_eq!(
                parse_str("if a: A then b: B"),
                root(if_then(
                    colon(id("a"), id("A")),
                    colon(id("b"), id("B")),
                ))
            )
        }

        #[test]
        fn colon_else_conflict() {
            assert_eq!(
                parse_str("if a then b else c: D"),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn colon_func_conflict() {
            assert_eq!(
                parse_str("a: A => b"),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn func_colon_conflict() {
            assert_eq!(
                parse_str("a => a: B"),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn func_colon_conflict_2() {
            assert_eq!(
                parse_str("a: A + A => a + b"),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn colon_func_type() {
            assert_eq!(
                parse_str("f: A -> B"),
                root(colon(id("f"), func_type(id("A"), id("B"))))
            )
        }

        #[test]
        fn func_type_colon() {
            assert_eq!(
                parse_str("A -> B: Type"),
                root(colon(func_type(id("A"), id("B")), id("Type")))
            )
        }
    }

    mod semicolon {
        use super::*;

        #[test]
        fn func_type_right_assoc() {
            assert_eq!(
                parse_str("x; y; z"),
                root(semicolon(id("x"), semicolon(id("y"), id("z"))))
            );
        }

        #[test]
        fn semicolon_apposition() {
            assert_eq!(
                parse_str("f x; g y"),
                root(semicolon(apposition(id("f"), id("x")), apposition(id("g"), id("y"))))
            )
        }

        #[test]
        fn semicolon_arithmetic() {
            assert_eq!(
                parse_str("a + b; c * d"),
                root(semicolon(add(id("a"), id("b")), mul(id("c"), id("d"))))
            )
        }

        #[test]
        fn semicolon_parens() {
            assert_eq!(
                parse_str("(a + b; c); {d - e}"),
                root(semicolon(
                    parenthesis(semicolon(add(id("a"), id("b")), id("c"))), 
                    brace(sub(id("d"), id("e")))
                ))
            )
        }

        #[test]
        fn if_semicolon_conflict() {
            assert_eq!(
                parse_str("if a; b then c else d"),
                Err(ParseError::IncompleteOperator(Op { kind: If, has_left: false, children: VecDeque::from(vec![id("a")])}))
            )
        }

        #[test]
        fn if_then_semicolon() {
            assert_eq!(
                parse_str("if a then b; c"),
                root(semicolon(if_then(id("a"), id("b")), id("c")))
            )
        }

        #[test]
        fn else_semicolon() {
            assert_eq!(
                parse_str("if a then b else c; d"),
                root(semicolon(if_then_else(id("a"), id("b"), id("c")), id("d")))
            )
        }
    }
}