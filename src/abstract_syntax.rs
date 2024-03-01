use std::borrow::Cow;


pub enum AST {
    Nat(u64),
    Float(f64),
    Id(Identifier),

    Match(Match),
    Add(Vec<AST>),
    Sub(Vec<AST>),
    Mul(Vec<AST>),
    Div(Vec<AST>),
    Mod(Vec<AST>),
    Colon(Box<AST>, Box<AST>),
    Let(Box<AST>, Box<AST>, Box<AST>)
}

pub struct Identifier(String);

pub struct Match {
    pub object: Box<AST>,
    pub arms: Vec<MatchArm>,
}

pub struct MatchArm {
    pattern: Pattern,
    body: AST,
}


pub struct Pattern {
    
}

pub enum Type {
    Type,
    Nat,
    Float,
    Unit,
    Dep(Identifier, Box<Type>),
    Prod(Vec<Type>),
    
    Match(Match),
}


pub struct DeBruijnIndex<'a>(Cow<'a, str>, u64);