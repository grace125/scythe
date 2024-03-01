use std::collections::VecDeque;

use super::lexing::{OpKind, LexingError};
pub use OpKind::*;

const TEMPLATE_PRECEDENCE: u32          = 0;
const BRACE_PRECEDENCE: u32             = 1;
const SEMICOLON_RIGHT_PRECEDENCE: u32   = 2;
const SEMICOLON_LEFT_PRECEDENCE: u32    = 3;
const TUPLE_PRECEDENCE: u32             = 4;
const MATCH_ARM_PRECEDENCE: u32         = 5;
const LET_PRECEDENCE: u32               = 6;
const ASSIGN_PRECEDENCE: u32            = 7;
const MATCH_PRECEDENCE: u32             = 8;
const ON_PRECEDENCE: u32                = 9;
const IF_PRECEDENCE: u32                = 10;
const THEN_PRECEDENCE: u32              = 11;
const ELSE_PRECEDENCE: u32              = 12;
const COLON_RIGHT_PRECEDENCE: u32       = 13;
const COLON_LEFT_PRECEDENCE: u32        = 14;
const FUNC_RIGHT_PRECEDENCE: u32        = 15;
const FUNC_LEFT_PRECEDENCE: u32         = 16;
const FUNC_TYPE_RIGHT_PRECEDENCE: u32   = 17;
const FUNC_TYPE_LEFT_PRECEDENCE: u32    = 18;
const SUM_PRECEDENCE: u32               = 19;
const PROD_PRECEDENCE: u32              = 20;
const APPOSITION_RIGHT_PRECEDENCE: u32  = 21;
const APPOSITION_LEFT_PRECEDENCE: u32   = 22;
const NO_SLOT_PRECEDENCE: u32           = u32::MAX;

const BRACE_GROUP: u32              = 0b_________________1;
const SEMICOLON_GROUP: u32          = 0b________________10;
const TUPLE_GROUP: u32              = 0b_______________100;
const MATCH_ARM_GROUP: u32          = 0b______________1000;
const LET_GROUP: u32                = 0b_____________10000;
const ASSIGN_GROUP: u32             = 0b____________100000;
const MATCH_GROUP: u32              = 0b___________1000000;
const ON_GROUP: u32                 = 0b__________10000000;
const IF_GROUP: u32                 = 0b_________100000000;
const THEN_GROUP: u32               = 0b________1000000000;
const ELSE_GROUP: u32               = 0b_______10000000000;
const COLON_GROUP: u32              = 0b______100000000000;
const FUNC_GROUP: u32               = 0b_____1000000000000;
const FUNC_TYPE_GROUP: u32          = 0b____10000000000000;
const SUM_GROUP: u32                = 0b___100000000000000;
const PROD_GROUP: u32               = 0b__1000000000000000;
const APPOSITION_GROUP: u32         = 0b_10000000000000000;






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
    pub const LET_SLOT: Slot                = Slot::new(LET_PRECEDENCE,                 LET_GROUP,              0);
    pub const ASSIGN_SLOT: Slot             = Slot::new(ASSIGN_PRECEDENCE,              ASSIGN_GROUP,           0);
    pub const TUPLE_SLOT: Slot              = Slot::new(TUPLE_PRECEDENCE,               TUPLE_GROUP,            SEMICOLON_GROUP | ASSIGN_GROUP | LET_GROUP | IF_GROUP | THEN_GROUP);
    pub const MATCH_SLOT: Slot              = Slot::new(MATCH_PRECEDENCE,               MATCH_GROUP,            0);
    pub const ON_SLOT: Slot                 = Slot::new(ON_PRECEDENCE,                  ON_GROUP,               0);
    pub const MATCH_ARM_SLOT: Slot          = Slot::new(MATCH_ARM_PRECEDENCE,           MATCH_ARM_GROUP,        0);
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
            OpKind::If |
            OpKind::Match |
            OpKind::Let                 => Slot::NO_SLOT,
            OpKind::BraceEnd | 
            OpKind::BracketEnd | 
            // TODO: should I be giving these templates super low precedence?
            OpKind::ParenthesisEnd      => Slot::BRACE_SLOT.with_template_precedence(), 
            OpKind::Assignment          => Slot::ASSIGN_SLOT.with_template_precedence(),
            OpKind::Then                => Slot::THEN_SLOT.with_template_precedence(),
            OpKind::Else                => Slot::ELSE_SLOT.with_template_precedence(),
            OpKind::On                  => Slot::MATCH_SLOT.with_template_precedence(),
            OpKind::MatchArm            => Slot::MATCH_ARM_SLOT,
            OpKind::Add | 
            OpKind::Sub                 => Slot::SUM_SLOT,
            OpKind::Mul | 
            OpKind::Div | 
            OpKind::Mod                 => Slot::PROD_SLOT,
            OpKind::Apposition          => Slot::APPOSITION_LEFT_SLOT,
            OpKind::Root                => Slot::ROOT_SLOT,
            OpKind::Semicolon           => Slot::SEMICOLON_LEFT_SLOT,
            OpKind::Func                => Slot::FUNC_LEFT_SLOT,
            OpKind::FuncType            => Slot::FUNC_TYPE_LEFT_SLOT,
            OpKind::Colon               => Slot::COLON_LEFT_SLOT,
            OpKind::Tuple               => Slot::TUPLE_SLOT,
        }
    }

    pub fn right_slot(&self) -> Slot {
        match self.kind {
            OpKind::Brace | 
            OpKind::Bracket | 
            OpKind::Parenthesis         => Slot::BRACE_SLOT,
            OpKind::BraceEnd | 
            OpKind::BracketEnd | 
            OpKind::ParenthesisEnd |
            OpKind::Then |
            OpKind::Else |
            OpKind::On |
            OpKind::Assignment          => unreachable!(),
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
            
            OpKind::Semicolon           => Slot::SEMICOLON_RIGHT_SLOT,
            OpKind::Func                => Slot::FUNC_RIGHT_SLOT,
            OpKind::FuncType            => Slot::FUNC_TYPE_RIGHT_SLOT,
            OpKind::Colon               => Slot::COLON_RIGHT_SLOT,
            OpKind::Let                 => match self.children.len() {
                0 => Slot::LET_SLOT,
                _ => Slot::ASSIGN_SLOT 
            },
            OpKind::Tuple               => Slot::TUPLE_SLOT,
            OpKind::Match               => match self.children.len() {
                0 => Slot::MATCH_SLOT,
                _ => Slot::ON_SLOT
            },
            OpKind::MatchArm            => Slot::MATCH_ARM_SLOT
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
            OpKind::BraceEnd | 
            OpKind::BracketEnd |
            OpKind::ParenthesisEnd | 
            OpKind::Then | 
            OpKind::Else |
            OpKind::On |
            OpKind::Assignment          => Err(ParseError::UnusedTemplate(self.kind)),
            OpKind::Brace | 
            OpKind::Bracket | 
            OpKind::Parenthesis | 
            OpKind::Add | 
            OpKind::Sub | 
            OpKind::Mul | 
            OpKind::Div | 
            OpKind::Mod | 
            OpKind::If | 
            OpKind::Semicolon | 
            OpKind::Func | 
            OpKind::FuncType | 
            OpKind::Colon | 
            OpKind::Apposition | 
            OpKind::Root |
            OpKind::Tuple |
            OpKind::Match |
            OpKind::MatchArm |
            OpKind::Let                 => Ok(())   
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
                    Err(ParseError::InvalidEmptySlot("Expected branch between `then` and `else`".to_owned()))
                },
                _ => Err(ParseError::InvalidTemplate("`if` statement has more than one `else`".to_owned()))
            },
            (OpKind::Let, OpKind::Assignment) => {
                if self.children.len() == 0 {
                    let Some(item) = item else {
                        return Err(ParseError::InvalidEmptySlot("Expected something between `let` and `:=`".to_owned()));
                    };
                    self.push_right(item);
                    Ok(TemplateOutput::Op(self))
                }
                else {
                    Err(ParseError::InvalidTemplate("`let` statement has more than one `:=`".to_owned()))
                }
            },
            (OpKind::Match, OpKind::On) => {
                if self.children.len() == 0 {
                    let Some(item) = item else {
                        return Err(ParseError::InvalidEmptySlot("Expected something between `match` and `on`".to_owned()));
                    };
                    self.push_right(item);
                    Ok(TemplateOutput::Op(self))
                }
                else {
                    Err(ParseError::InvalidTemplate("`match` statement has more than one `on`".to_owned()))
                }
            },
            (OpKind::Tuple, OpKind::Tuple) => {
                self.push_right_option(item);
                Ok(TemplateOutput::Op(self))
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
            (OpKind::On, _) |
            (OpKind::Assignment, _) |
            (OpKind::BraceEnd, _) | 
            (OpKind::BracketEnd, _) | 
            (OpKind::ParenthesisEnd, _)             => unreachable!(),
            (OpKind::Tuple, 0)                      => Ok(CST::Op(self)),
            (OpKind::Tuple, _) if self.has_left     => Ok(CST::Op(self)),
            (OpKind::Tuple, _)                      => Err(ParseError::IncompleteOperator(self)),
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
            (OpKind::Let, 2) |
            (OpKind::Match, 2) |
            (OpKind::MatchArm, 2) |
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
            (OpKind::Let, _) |
            (OpKind::Match, _) |
            (OpKind::MatchArm, _) |
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

        pub fn tuple(children: impl IntoIterator<Item = CST>) -> CST {
            let children: VecDeque<_> = children.into_iter().collect();
            Op {
                kind: Tuple,
                has_left: children.len() > 0,
                children,
            }.try_into().unwrap()
        }

        pub fn semicolon(left: CST, right: CST) -> CST {
            Op {
                kind: Semicolon,
                has_left: true,
                children: VecDeque::from(vec![left, right])
            }.try_into().unwrap()
        }

        pub fn let_statement(identifier: CST, content: CST) -> CST {
            Op {
                kind: Let,
                has_left: false,
                children: VecDeque::from(vec![identifier, content])
            }.try_into().unwrap()
        }

        pub fn match_statement(object: CST, arms: impl IntoIterator<Item = CST>) -> CST {
            Op {
                kind: Match,
                has_left: false,
                children: VecDeque::from(vec![object, brace(tuple(arms))])
            }.try_into().unwrap()
        }

        pub fn match_arm(pattern: CST, body: CST) -> CST {
            Op {
                kind: MatchArm,
                has_left: true,
                children: VecDeque::from(vec![pattern, body])
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

    mod tup {
        use super::*;

        #[test]
        fn three_tuple() {
            assert_eq!(
                parse_str("a, b, c"),
                root(tuple([
                    id("a"),
                    id("b"),
                    id("c"),
                ]))
            )
        }

        #[test]
        fn tuple_apposition() {
            assert_eq!(
                parse_str("f x, g y"),
                root(tuple([apposition(id("f"), id("x")), apposition(id("g"), id("y"))]))
            )
        }

        #[test]
        fn tuple_arithmetic() {
            assert_eq!(
                parse_str("a + b, c * d"),
                root(tuple([
                    add(id("a"), id("b")),
                    mul(id("c"), id("d"))
                ]))
            )
        }

        #[test]
        fn tuple_parens() {
            assert_eq!(
                parse_str("((a, b), c)"),
                root(parenthesis(tuple([
                    parenthesis(tuple([id("a"), id("b")])),
                    id("c")
                ])))
            )
        }

        #[test]
        fn tuple_if_conflict() {
            assert_eq!(
                parse_str("if a, b then c else d"),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn tuple_then_conflict() {
            assert_eq!(
                parse_str("if a then b, c else d"),
                Err(ParseError::OperatorConflict)
            )
        }

        #[test]
        fn tuple_else() {
            assert_eq!(
                parse_str("if a then b else c, d"),
                root(tuple([
                    if_then_else(id("a"), id("b"), id("c")),
                    id("d")
                ]))
            )
        }

        #[test]
        fn tuple_func() {
            assert_eq!(
                parse_str("x => y, x => z"),
                root(tuple([
                    func(id("x"), id("y")),
                    func(id("x"), id("z")),
                ]))
            )
        }

        #[test]
        fn tuple_func_type() {
            assert_eq!(
                parse_str("A -> B, A -> C"),
                root(tuple([
                    func_type(id("A"), id("B")),
                    func_type(id("A"), id("C")),
                ]))
            )
        }

        #[test]
        fn tuple_colon() {
            assert_eq!(
                parse_str("a: A, b: B"),
                root(tuple([
                    colon(id("a"), id("A")),
                    colon(id("b"), id("B"))
                ]))
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

        #[test]
        fn semicolon_func() { 
            assert_eq!(
                parse_str("a => b; c => d"),
                root(semicolon(func(id("a"), id("b")), func(id("c"), id("d"))))
            )
        }

        // #[test]
        // fn semicolon_tuple_conflict() {
        //     todo!()
        // }
    }

    mod let_statements {
        use super::*;

        #[test]
        fn let_apposition() {
            assert_eq!(
                parse_str("let y := f x"),
                root(let_statement(id("y"), apposition(id("f"), id("x"))))
            );
        }

        #[test]
        fn let_arithmetic() {
            assert_eq!(
                parse_str("let y := 5 + x; let z := +y % 2"),
                root(semicolon(
                    let_statement(id("y"), add(nat(5), id("x"))), 
                    let_statement(id("z"), modulo(unary_add(id("y")), nat(2))), 
                ))
            );
        }

        #[test]
        fn let_parenthesis() {
            assert_eq!(
                parse_str("let b := (let a := 5 / 10; a + 3)"),
                root(let_statement(id("b"), parenthesis(semicolon(let_statement(id("a"), div(nat(5), nat(10))), add(id("a"), nat(3))))))
            );
        }

        #[test]
        fn let_if() {
            assert_eq!(
                parse_str("let a := if b then c else let d := 2"),
                root(let_statement(id("a"), if_then_else(id("b"), id("c"), let_statement(id("d"), nat(2)))))
            );
        }

        #[test]
        fn let_func() {
            assert_eq!(
                parse_str("let f := (x: Nat) => y"),
                root(let_statement(id("f"), func(parenthesis(colon(id("x"), id("Nat"))), id("y"))))
            );
        }

        #[test]
        fn let_func_type() {
            assert_eq!(
                parse_str("let T := A -> B"),
                root(let_statement(id("T"), func_type(id("A"), id("B"))))
            );
        }

        // #[test]
        // fn let_tuple_conflict() {
        //     todo!()
        // }
    }

    mod match_statements {
        use super::*;

        #[test]
        fn match_apposition() {
            assert_eq!(
                parse_str("match f x on { g y ==> h z, h z ==> i w }"),
                root(match_statement(apposition(id("f"), id("x")), [
                    match_arm(apposition(id("g"), id("y")), apposition(id("h"), id("z"))),
                    match_arm(apposition(id("h"), id("z")), apposition(id("i"), id("w")))
                ]))
            )
        }

        // TODO: test match conflicts
        // TODO: test match against above elements
    }
}