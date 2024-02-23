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
}

impl PartialEq for Slot {
    fn eq(&self, other: &Self) -> bool {
        self.precedence == other.precedence
    }
}

pub enum TemplateOutput {
    Item(Box<CST>),
    Op(Op),
    Miss(Op, Option<Box<CST>>, Op),
}

#[derive(Clone, PartialEq)]
pub struct Op {
    kind: OpKind,
    left: Option<Box<CST>>,
    right: Option<Box<CST>>
}

impl Op {
    pub fn new(kind: OpKind) -> Op {
        Op { kind, left: None, right: None }
    }

    pub fn left_slot(&self) -> Slot {
        if self.left.is_some() { return Slot::NO_SLOT; }
        match self.kind {
            OpKind::BraceStart | 
            OpKind::BracketStart | 
            OpKind::ParenthesisStart |
            OpKind::If                  => Slot::NO_SLOT,
            OpKind::BraceEnd | 
            OpKind::BracketEnd | 
            OpKind::ParenthesisEnd      => Slot::BRACE_SLOT,
            OpKind::Add | 
            OpKind::Sub                 => Slot::SUM_SLOT,
            OpKind::Mul | 
            OpKind::Div | 
            OpKind::Mod                 => Slot::PROD_SLOT,
            OpKind::Apposition          => Slot::APPOSITION_LEFT_SLOT,
            OpKind::Root                => Slot::ROOT_SLOT,
            OpKind::Then                => Slot::THEN_SLOT,
            OpKind::Else                => Slot::ELSE_SLOT,
            OpKind::Semicolon           => Slot::SEMICOLON_LEFT_SLOT,
            OpKind::Func                => Slot::FUNC_LEFT_SLOT,
            OpKind::FuncType            => Slot::FUNC_TYPE_LEFT_SLOT,
            OpKind::Colon               => Slot::COLON_LEFT_SLOT,
        }
    }

    pub fn right_slot(&self) -> Slot {
        if self.right.is_some() { return Slot::NO_SLOT; }
        match self.kind {
            OpKind::BraceStart | 
            OpKind::BracketStart | 
            OpKind::ParenthesisStart    => Slot::BRACE_SLOT,
            OpKind::BraceEnd | 
            OpKind::BracketEnd | 
            OpKind::ParenthesisEnd      => Slot::NO_SLOT,
            OpKind::Add | 
            OpKind::Sub                 => if self.left.is_some() { Slot::SUM_SLOT } else { Slot::PROD_SLOT },
            OpKind::Mul | 
            OpKind::Div | 
            OpKind::Mod                 => Slot::PROD_SLOT,
            OpKind::Apposition          => Slot::APPOSITION_RIGHT_SLOT,
            OpKind::Root                => Slot::ROOT_SLOT,
            OpKind::If                  => Slot::IF_SLOT,
            OpKind::Then                => Slot::THEN_SLOT,
            OpKind::Else                => Slot::ELSE_SLOT,
            OpKind::Semicolon           => Slot::SEMICOLON_RIGHT_SLOT,
            OpKind::Func                => Slot::FUNC_RIGHT_SLOT,
            OpKind::FuncType            => Slot::FUNC_TYPE_RIGHT_SLOT,
            OpKind::Colon               => Slot::COLON_RIGHT_SLOT
        }
    }

    fn update_left(&mut self, l: Option<Box<CST>>) {
        if l.is_some() {
            debug_assert!(self.left.is_none());
            self.left = l;
        }
    }

    fn update_right(&mut self, r: Option<Box<CST>>) {
        if r.is_some() {
            debug_assert!(self.right.is_none());
            self.right = r;
        }
    }

    fn template(mut self, item: Option<Box<CST>>, mut other: Op) -> Result<TemplateOutput, ParseError> {
        match (self.kind, other.kind) {
            (OpKind::BraceStart, OpKind::BraceEnd) | 
            (OpKind::BracketStart, OpKind::BracketEnd) | 
            (OpKind::ParenthesisStart, OpKind::ParenthesisEnd) => {
                other.update_left(item);
                self.right = other.into();
                Ok(TemplateOutput::Item(self.into()))
            },
            _ => Ok(TemplateOutput::Miss(self, item, other))
        }
    }
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

pub fn parse<L: Iterator<Item = Result<CST, LexingError>>>(lex: &mut L) -> Result<Option<Box<CST>>, ParseError> {
    let (first_item, first_op) = next(lex, None)?;

    let Some(mut first_op) = first_op else {
        return Ok(first_item);
    };

    first_op.left = first_item;

    loop {
        let (new_item, new_op) = parse_inner(lex, first_op)?;
        match new_op {
            None => break Ok(new_item),
            Some(mut new_op) => {
                new_op.update_left(new_item);
                first_op = new_op;
            }
        }
    }
}

fn parse_inner<L>(lex: &mut L, mut prev_op: Op) -> Result<(Option<Box<CST>>, Option<Op>), ParseError> 
where L: Iterator<Item = Result<CST, LexingError>>
{
    let (mut item, mut maybe_this_op) = next(lex, None)?;

    let prev_slot = prev_op.right_slot();

    loop {
        let Some(mut this_op) = maybe_this_op else {
            prev_op.update_right(item);
            return Ok((prev_op.into(), None));
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
            prev_op.update_right(item);
            return Ok((prev_op.into(), Some(this_op)));
        }

        this_op.update_left(item);
    
        (item, maybe_this_op) = parse_inner(lex, this_op)?;
    }

}

fn next<L>(lex: &mut L, item: Option<Box<CST>>) -> Result<(Option<Box<CST>>, Option<Op>), ParseError> 
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

        pub fn parse_str(s: &str) -> Result<Option<Box<CST>>, ParseError> {
            parse(&mut lex(s))
        }
    
        pub fn root(op: Op) -> Result<Option<Box<CST>>, ParseError> {
            Ok(op.into())
        }

        pub fn op(kind: OpKind, left: impl Into<Option<Box<CST>>>, right: impl Into<Option<Box<CST>>>) -> Op {
            Op { kind, left: left.into(), right: right.into() }
        }
    
        pub fn id(s: &str) -> CST {
            CST::Id(s.to_owned())
        }
    
        pub fn nat(s: u64) -> CST {
            CST::Nat(s)
        }
    
        pub fn bracket(item: impl Into<CST>) -> Op {
            op(BracketStart, None, op(BracketEnd, item.into(), None))
        }
    
        pub fn brace(item: impl Into<CST>) -> Op {
            op(BraceStart, None, op(BraceEnd, item.into(), None))
        }
    
        pub fn parenthesis(item: impl Into<CST>) -> Op {
            op(ParenthesisStart, None, op(ParenthesisEnd, item.into(), None))
        }
    
        pub fn if_then_else(condition: impl Into<CST>, case_1: impl Into<CST>, case_2: impl Into<CST>) -> Op {
            op(If, None, op(Then, condition.into(), op(Else, case_1.into(), case_2.into())))
        }
    
        pub fn if_then(condition: impl Into<CST>, case: impl Into<CST>) -> Op {
            op(If, None, op(Then, condition.into(), case.into()))
        }
    
        pub fn assert_left_assoc(s: &str, kind: OpKind, id1: CST, id2: CST, id3: CST) {
            assert_eq!(parse_str(s), root(op(kind, op(kind, id1, id2), id3)))
        }
        
        pub fn assert_right_assoc(s: &str, kind: OpKind, id1: CST, id2: CST, id3: CST) {
            assert_eq!(parse_str(s), root(op(kind, id1, op(kind, id2, id3))))
        }
    }

    mod apposition {
        use super::*;

        #[test]
        fn apposition_right_assoc() {
            assert_right_assoc("x y z", Apposition, id("x"), id("y"), id("z"))
        }
    }

    mod arithmetic {
        use super::*;

        #[test]
        fn addition_left_assoc() {
            assert_left_assoc("x + y + z", Add, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn subtraction_left_assoc() {
            assert_left_assoc("x - y - z", Sub, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn multiplication_left_assoc() {
            assert_left_assoc("x * y * z", Mul, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn division_left_assoc() {
            assert_left_assoc("x / y / z", Div, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn modulo_left_assoc() {
            assert_left_assoc("x % y % z", Mod, id("x"), id("y"), id("z"))
        }
    
        #[test]
        fn arithmetic() {
            assert_eq!(
                parse_str("x * y + z"), 
                root(op(Add, op(Mul, id("x"), id("y")), id("z")))
            )
        }
    
        #[test]
        fn arithmetic_2() {
            assert_eq!(
                parse_str("2 % x - yy / 44"), 
                root(op(Sub, op(Mod, nat(2), id("x")), op(Div, id("yy"), nat(44))))
            )
        }
    
        #[test]
        fn unary_subtraction() {
            assert_eq!(parse_str("-a * b"), root(op(Mul, op(Sub, None, id("a")), id("b"))))
        }
    
        #[test]
        fn apposition_addition_conflict() {
            assert_eq!(parse_str("f x + y"), root(op(Add, op(Apposition, id("f"), id("x")), id("y"))));
        }
    
        #[test]
        fn addition_apposition() {
            assert_eq!(
                parse_str("x + f y"), 
                root(op(Add, id("x"), op(Apposition, id("f"), id("y"))))
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
                root(op(Add, parenthesis(id("x")), nat(5)))
            )
        }

        #[test]
        fn parens_item_arithmetic() {
            assert_eq!(parse_str("(f) 3 + 5"), root(op(Add, op(Apposition, parenthesis(id("f")), nat(3)), nat(5))))
        }

        // Tests "case 3" in parse
        #[test]
        fn arithmetic_parens() {
            assert_eq!(parse_str("5 + (x)"), root(op(Add, nat(5), parenthesis(id("x")))))
        }

        // Tests "case 4" in parse
        #[test]
        fn arithmetic_item_parens() {
            assert_eq!(
                parse_str("5 + f(x)"), 
                root(op(Add, nat(5), op(Apposition, id("f"), parenthesis(id("x")))))
            )
        }

        // Tests "case 5" in parse
        #[test]
        fn parens_parens() {
            assert_eq!(parse_str("(f) (x)"), root(op(Apposition, parenthesis(id("f")), parenthesis(id("x")))))
        }

        // Tests "case 6" in parse
        #[test]
        fn parens_item_parens() {
            assert_eq!(
                parse_str("(f) a (x)"), 
                root(op(Apposition, parenthesis(id("f")), op(Apposition, id("a"), parenthesis(id("x")))))
            )
        }

        #[test]
        fn parens_around_arithmetic() {
            assert_eq!(
                parse_str("(a + b)"),
                root(parenthesis(op(Add, id("a"), id("b"))))
            )
        }

        #[test]
        fn parens_around_arithmetic_arithmetic() {
            assert_eq!(
                parse_str("(a + b) + (c + d)"),
                root(op(Add, 
                    parenthesis(op(Add, id("a"), id("b"))), 
                    parenthesis(op(Add, id("c"), id("d")))
                ))
            )
        }

        #[test]
        fn parens_nested() {
            assert_eq!(
                parse_str("[(c + d)]"),
                root(bracket(parenthesis(op(Add, id("c"), id("d")))))
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
                root(op(Apposition, id("a"), if_then(
                    op(Apposition, id("f"), id("x")), 
                    op(Apposition, id("g"), id("y"))
                )))
            )
        }

        #[test]
        fn if_then_else_arithmetic() {
            assert_eq!(
                parse_str("if w + x then x % y else y * z"), 
                root(if_then_else(op(Add, id("w"), id("x")), op(Mod, id("x"), id("y")), op(Mul, id("y"), id("z"))))
            )
        }
    }

    mod func {
        use super::*;
        
        #[test]
        fn func_right_assoc() {
            assert_right_assoc("x => y => z", Func, id("x"), id("y"), id("z"))
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
            assert_eq!(parse_str("a => f x"), root(op(Func, id("a"), op(Apposition, id("f"), id("x")))))
        }

        #[test]
        fn func_arithmetic() {
            assert_eq!(
                parse_str("A * B => a + b"),
                root(op(Func, op(Mul, id("A"), id("B")), op(Add, id("a"), id("b"))))
            )
        }

        #[test]
        fn func_if_then_else() {
            assert_eq!(
                parse_str("a => if a then 1 else x"),
                root(op(Func, id("a"), if_then_else(id("a"), nat(1), id("x"))))
            )
        }

        #[test]
        fn if_func_then_func_else_func() {
            assert_eq!(
                parse_str("if a => b then c => d else e => f"),
                root(if_then_else(
                    op(Func, id("a"), id("b")), 
                    op(Func, id("c"), id("d")), 
                    op(Func, id("e"), id("f"))
                ))
            )
        }
    }

    mod func_type {
        use super::*;

        #[test]
        fn func_type_right_assoc() {
            assert_right_assoc("x -> y -> z", FuncType, id("x"), id("y"), id("z"))
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
            assert_eq!(parse_str("a -> f x"), root(op(FuncType, id("a"), op(Apposition, id("f"), id("x")))))
        }

        #[test]
        fn func_type_arithmetic() {
            assert_eq!(
                parse_str("A * B -> A + B"),
                root(op(FuncType, op(Mul, id("A"), id("B")), op(Add, id("A"), id("B"))))
            )
        }

        #[test]
        fn func_type_if_then_else() {
            assert_eq!(
                parse_str("a -> if a then A else B"),
                root(op(FuncType, id("a"), if_then_else(id("a"), id("A"), id("B"))))
            )
        }

        #[test]
        fn if_func_type_then_func_type_else_func_type() {
            assert_eq!(
                parse_str("if A -> B then C -> D else E -> F"),
                root(if_then_else(
                    op(FuncType, id("A"), id("B")), 
                    op(FuncType, id("C"), id("D")), 
                    op(FuncType, id("E"), id("F"))
                ))
            )
        }

        #[test]
        fn func_func_type() {
            assert_eq!(
                parse_str("A => A -> B"),
                root(op(Func, id("A"), op(FuncType, id("A"), id("B"))))
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
            assert_right_assoc("a: A: Type", Colon, id("a"), id("A"), id("Type"))
        }

        #[test]
        fn colon_apposition() {
            assert_eq!(
                parse_str("list: Vec T n"),
                root(op(Colon, id("list"), op(Apposition, id("Vec"), op(Apposition, id("T"), id("n")))))
            )
        }

        // FIXME: should this be a conflict?
        #[test]
        fn apposition_colon() {
            assert_eq!(
                parse_str("f x: T"),
                root(op(Colon, op(Apposition, id("f"), id("x")), id("T")))
            )
        }

        #[test]
        fn arithmetic_colon() {
            assert_eq!(
                parse_str("a / b: A"),
                root(op(Colon, op(Div, id("a"), id("b")), id("A")))
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
                    op(Colon, id("a"), id("A")),
                    op(Colon, id("b"), id("B")),
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
                root(op(Colon, id("f"), op(FuncType, id("A"), id("B"))))
            )
        }

        #[test]
        fn func_type_colon() {
            assert_eq!(
                parse_str("A -> B: Type"),
                root(op(Colon, op(FuncType, id("A"), id("B")), id("Type")))
            )
        }
    }

    mod semicolon {
        use super::*;

        #[test]
        fn semicolon_right_assoc() {
            assert_right_assoc("a; b ; c", Semicolon, id("a"), id("b"), id("c"))
        }

        #[test]
        fn semicolon_apposition() {
            assert_eq!(
                parse_str("f x; g y"),
                root(op(Semicolon, op(Apposition, id("f"), id("x")), op(Apposition, id("g"), id("y"))))
            )
        }

        #[test]
        fn semicolon_arithmetic() {
            assert_eq!(
                parse_str("a + b; c * d"),
                root(op(Semicolon, op(Add, id("a"), id("b")), op(Mul, id("c"), id("d"))))
            )
        }

        #[test]
        fn semicolon_parens() {
            assert_eq!(
                parse_str("(a + b; c); {d - e}"),
                root(op(Semicolon, 
                    parenthesis(op(Semicolon, op(Add, id("a"), id("b")), id("c"))), 
                    brace(op(Sub, id("d"), id("e")))
                ))
            )
        }

        // TODO: find a way to have a conflict that doesn't ruin if_then_semicolon (?)
        // #[test]
        // fn if_semicolon_conflict() {
        //     assert_eq!(
        //         parse_str("if a; b then c else d"),
        //         Err(ParseError::OperatorConflict)
        //     )
        // }

        #[test]
        fn if_then_semicolon() {
            assert_eq!(
                parse_str("if a then b; c"),
                root(op(Semicolon, if_then(id("a"), id("b")), id("c")))
            )
        }

        #[test]
        fn else_semicolon() {
            assert_eq!(
                parse_str("if a then b else c; d"),
                root(op(Semicolon, if_then_else(id("a"), id("b"), id("c")), id("d")))
            )
        }
    }
}
