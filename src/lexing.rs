use logos::{Logos, Lexer};
use crate::concrete_syntax::{Op, CST};

#[derive(Default)]
struct TokenExtras {
    no_right_slot: bool
}

#[derive(Debug, PartialEq, Clone)]
struct TokenInfo {
    apposition: bool,
    syntax: SyntaxInfo,
}

// TODO: carry row/column data to later parts of compilation for debugging
#[derive(Debug, PartialEq, Clone)]
pub struct SyntaxInfo;
// {
//     row_start: usize,
//     column_start: usize
//     row_end: usize,
//     column_end: usize
// }

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = LexingError)]
#[logos(extras = TokenExtras)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    // #[regex("\"\"")]
    #[regex(r"[a-zA-Z_][0-9a-zA-Z_]*",                  id_callback,    priority = 1)]
    #[regex(r"[0-9]+",                                  nat_callback,   priority = 2)] 
    #[regex(r"[0-9]+\.[0-9]*",                          float_callback, priority = 2)]
    #[regex(r"([\[\]{}();:,+*-/%]|=>|->|if|then|else)", op_callback,    priority = 2)]
    Token((TokenInfo, CST)),
}

fn get_info_for_literals(lex: &mut Lexer<Token>) -> TokenInfo {
    let info = TokenInfo {
        apposition: lex.extras.no_right_slot,
        syntax: SyntaxInfo
    };
    lex.extras.no_right_slot = true;
    info
}

fn id_callback(lex: &mut Lexer<Token>) -> (TokenInfo, CST) {
    (get_info_for_literals(lex), CST::Id(lex.slice().to_string()))
    
}

fn nat_callback(lex: &mut Lexer<Token>) -> Result<(TokenInfo, CST), LexingError> {
    lex .slice()
        .parse()
        .map(|n| (get_info_for_literals(lex), CST::Nat(n)))
        .or_else(|_e| Err(LexingError::InvalidNatLiteral))
}

fn float_callback(lex: &mut Lexer<Token>) -> Result<(TokenInfo, CST), LexingError> {
    lex .slice()
        .parse()
        .map(|f| (get_info_for_literals(lex), CST::Float(f)))
        .or_else(|_e| Err(LexingError::InvalidFloatLiteral))
}

fn op_callback(lex: &mut Lexer<Token>) -> (TokenInfo, CST) {
    let kind = match lex.slice() {
        "{" => OpKind::BraceStart,
        "}" => OpKind::BraceEnd,
        "[" => OpKind::BracketStart,
        "]" => OpKind::BracketEnd,
        "(" => OpKind::ParenthesisStart,
        ")" => OpKind::ParenthesisEnd,
        "+" => OpKind::Add,
        "*" => OpKind::Mul,
        "-" => OpKind::Sub,
        "/" => OpKind::Div,
        "%" => OpKind::Mod,
        "if" => OpKind::If,
        "then" => OpKind::Then,
        "else" => OpKind::Else,
        ";" => OpKind::Semicolon,
        "=>" => OpKind::Func,
        "->" => OpKind::FuncType,
        ":" => OpKind::Colon,
        _ => unreachable!()
    };
    let info = TokenInfo {
        apposition: lex.extras.no_right_slot && !kind.has_left_slot(),
        syntax: SyntaxInfo,
    };
    lex.extras.no_right_slot = !kind.has_right_slot();
    (info, CST::Op(Op::new(kind)))
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingError {
    InvalidNatLiteral,
    InvalidFloatLiteral,

    #[default]
    NonAsciiCharacter,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OpKind {
    BraceStart,
    BraceEnd,
    BracketStart,
    BracketEnd,
    ParenthesisStart,
    ParenthesisEnd,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    If,
    Then,
    Else,
    Semicolon,
    Func,
    FuncType,
    Colon,
    Apposition,
    Root,
}

impl OpKind {

    /// INVARIANT:  must agree with Op::left_slot
    pub fn has_left_slot(&self) -> bool {
        match self {
            OpKind::BraceStart | OpKind::BracketStart | OpKind::ParenthesisStart | OpKind::If => false,
            OpKind::BraceEnd   | OpKind::BracketEnd   | OpKind::ParenthesisEnd |
            OpKind::Add | OpKind::Sub |
            OpKind::Mul | OpKind::Div | OpKind::Mod  |
            OpKind::Apposition |
            OpKind::Root |
            OpKind::Then |
            OpKind::Else |
            OpKind::Semicolon |
            OpKind::Func |
            OpKind::FuncType |
            OpKind::Colon => true
        }
    }

    /// INVARIANT:  must agree with Op::right_slot
    pub fn has_right_slot(&self) -> bool {
        match self {
            OpKind::BraceEnd   | OpKind::BracketEnd   | OpKind::ParenthesisEnd => false,
            OpKind::BraceStart | OpKind::BracketStart | OpKind::ParenthesisStart |
            OpKind::Add | OpKind::Sub |
            OpKind::Mul | OpKind::Div | OpKind::Mod  |
            OpKind::Apposition |
            OpKind::Root |
            OpKind::If |
            OpKind::Then |
            OpKind::Else |
            OpKind::Semicolon |
            OpKind::Func |
            OpKind::FuncType |
            OpKind::Colon => true
        }
    }
}

pub fn lex<'l>(s: &'l str) -> impl Iterator<Item = Result<CST, LexingError>> + 'l {
    Token::lexer(s).flat_map(|res| {
        match res {
            Ok(Token::Token((info, cst))) => {
                let TokenInfo { apposition, syntax: _ } = info;
                if apposition {
                    vec![Ok(Op::new(OpKind::Apposition).into()), Ok(cst)].into_iter()
                    
                }
                else {
                    vec![Ok(cst)].into_iter()
                }
            },
            Err(e) => vec![Err(e)].into_iter()
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use OpKind::*;

    #[test]
    fn test_apposition() {
        let mut iter = lex("(f x) (g 5 3) + 2 + (h 1)");
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(ParenthesisStart));
        assert_eq!(iter.next().unwrap().unwrap(), CST::id_from("f"));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(Apposition));
        assert_eq!(iter.next().unwrap().unwrap(), CST::id_from("x"));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(ParenthesisEnd));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(Apposition));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(ParenthesisStart));
        assert_eq!(iter.next().unwrap().unwrap(), CST::id_from("g"));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(Apposition));
        assert_eq!(iter.next().unwrap().unwrap(), CST::nat_from(5));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(Apposition));
        assert_eq!(iter.next().unwrap().unwrap(), CST::nat_from(3));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(ParenthesisEnd));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(Add));
        assert_eq!(iter.next().unwrap().unwrap(), CST::nat_from(2));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(Add));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(ParenthesisStart));
        assert_eq!(iter.next().unwrap().unwrap(), CST::id_from("h"));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(Apposition));
        assert_eq!(iter.next().unwrap().unwrap(), CST::nat_from(1));
        assert_eq!(iter.next().unwrap().unwrap(), CST::op_from(ParenthesisEnd));
        assert_eq!(iter.next(), None);
    }

}