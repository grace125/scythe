use logos::Source;
use num_bigint::BigUint;

use crate::{concrete_syntax::{Op, CST}, elaboration::{SurfacePattern, SurfaceTerm}, lexing::OpKind};

pub fn abstract_parse(cst: Option<CST>) -> SurfaceTerm {
    match cst {
        Some(CST::Float(_)) => todo!(),
        Some(CST::Id(id)) => SurfaceTerm::Generic(id),
        Some(CST::Nat(n)) => SurfaceTerm::NatNum(BigUint::from(n)), // TODO: move conversion to BigUInt earlier in compilation
        Some(CST::Str(s)) => SurfaceTerm::StrLiteral(s),
        Some(CST::Op(mut op)) => match op.kind {
            OpKind::Bracket |
            OpKind::Parenthesis |
            OpKind::Brace => match op.children.len() {
                0 => SurfaceTerm::EmptyTuple,
                1 => abstract_parse(op.children.pop_front()),
                _ => panic!()
            },
            OpKind::Func => {
                let left = op.children.pop_front().unwrap();
                let right = op.children.pop_front().unwrap();
                SurfaceTerm::Func(abstract_parse_pattern(left), Box::new(abstract_parse(Some(right))))
            },
            OpKind::Apposition => {
                let left = op.children.pop_front().unwrap();
                let right = op.children.pop_front().unwrap();
                SurfaceTerm::Call(Box::new(abstract_parse(Some(left))), Box::new(abstract_parse(Some(right))))
            },
            OpKind::Tuple => abstract_parse_tuple(op),

            OpKind::Add => todo!(),
            OpKind::Sub => todo!(),
            OpKind::Mul => todo!(),
            OpKind::Div => todo!(),
            OpKind::Mod => todo!(),

            OpKind::If => todo!(),
            OpKind::Then => todo!(),
            OpKind::Else => todo!(),

            OpKind::Semicolon => todo!(),
            OpKind::Let => todo!(),
            OpKind::Assignment => todo!(),
            
            OpKind::FuncType => todo!(),

            OpKind::Colon => todo!(),

            OpKind::Root => todo!(),
            
            OpKind::Match => todo!(),
            OpKind::On => todo!(),
            OpKind::MatchArm => todo!(),

            OpKind::BraceEnd => panic!("Unmatched brace"),
            OpKind::BracketEnd => panic!("Unmatched bracket"),
            OpKind::ParenthesisEnd => panic!("Unmatched parenthesis"),
        }
        None => todo!(),
    }
}

fn abstract_parse_tuple(mut op: Op) -> SurfaceTerm {
    let mut result = abstract_parse(op.children.pop_back());
    while !op.children.is_empty() {
        let next = abstract_parse(op.children.pop_back());
        result = SurfaceTerm::BinaryTuple(Box::new(next), Box::new(result));
    }
    result
}

fn abstract_parse_pattern(cst: CST) -> SurfacePattern {
    match cst {
        CST::Float(_) => todo!(),
        CST::Nat(_) => todo!(),
        CST::Str(_) => todo!(),
        CST::Id(id) => SurfacePattern::Generic(id),
        
        CST::Op(mut op) => match op.kind {
            OpKind::Bracket |
            OpKind::Parenthesis |
            OpKind::Brace => match op.children.len() {
                0 => SurfacePattern::EmptyTuple,
                1 => abstract_parse_pattern(op.children.pop_front().unwrap()),
                _ => panic!()
            },
            OpKind::Colon => {
                let left = op.children.pop_front().unwrap();
                let right = op.children.pop_front().unwrap();
                SurfacePattern::Annotation(
                    Box::new(abstract_parse_pattern(left)), 
                    Box::new(abstract_parse(Some(right)))
                )
            },

            OpKind::Tuple => todo!(),
            OpKind::Add => todo!(),
            OpKind::Sub => todo!(),
            OpKind::Mul => todo!(),
            OpKind::Div => todo!(),
            OpKind::Mod => todo!(),
            OpKind::If => todo!(),
            OpKind::Then => todo!(),
            OpKind::Else => todo!(),
            OpKind::Semicolon => todo!(),
            OpKind::Func => todo!(),
            OpKind::FuncType => todo!(),
            
            OpKind::Apposition => todo!(),
            OpKind::Root => todo!(),
            OpKind::Let => todo!(),
            OpKind::Assignment => todo!(),
            
            OpKind::Match => todo!(),
            OpKind::On => todo!(),
            OpKind::MatchArm => todo!(),

            OpKind::BraceEnd => panic!("Unmatched brace"),
            OpKind::BracketEnd => panic!("Unmatched bracket"),
            OpKind::ParenthesisEnd => panic!("Unmatched parenthesis"),
        },
    }
}

