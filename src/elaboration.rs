use std::fmt::Debug;

pub struct Universe {
    // counter: Cell<u32>, 
    bindings: Vec<(Binding, BindingRef)>,
    substitutions: Vec<Substitution>
}

impl Universe {
    #[inline]
    pub fn sub_lookup(&self, val: GenericValue) -> Option<Value> {
        Some((*self.substitutions[val.0.0].val?).clone())
    }

    #[inline]
    pub fn bind_lookup(&self, term: GenericTerm) -> GenericValue {
        GenericValue(self.bindings[term.0.0].0.val)
    }

    #[inline]
    pub fn lookup(&self, term: GenericTerm) -> Value {
        let gen_val = self.bind_lookup(term);
        self.sub_lookup(gen_val).unwrap_or(Value::Generic(gen_val))
    }

    #[inline]
    pub fn string_bind_lookup(&self, start: BindingRef, id: &str) -> Option<GenericValue> {
        if start == BindingRef(0) {
            return None;
        }
        let (bind, prev) = self.bindings[start.0];
        if bind.identifier.as_str() == id {
            Some(GenericValue(bind.val))
        }
        else {
            self.string_bind_lookup(prev, id)
        }
    }

    #[inline]
    pub fn string_lookup(&self, start: BindingRef, id: &str) -> Option<Value> {
        let gen_val = self.string_bind_lookup(start, id)?;
        Some(self.sub_lookup(gen_val).unwrap_or(Value::Generic(gen_val)))
    }

    #[inline]
    pub fn evaluate(&self, env: Environment, t: Term) -> Result<Value, ElaborationError> {
        match t {
            Term::Identifier(id) => match self.string_lookup(env.0, id.as_str()) {
                Some(v) => Ok(v),
                None => Err(ElaborationError::IdentifierNotFound(id)),
            }
            Term::Generic(x) => Ok(self.lookup(x)),
        }
    }

    #[inline]
    pub fn canonical_generic_term(&self, val: GenericValue) -> GenericTerm {
        GenericTerm(self.substitutions[val.0.0].canonical_term)
    }

    #[inline]
    pub fn quote(&self, v: Value) -> Result<Term, ElaborationError> {
        match v {
            Value::Generic(u) => Ok(Term::Generic(self.canonical_generic_term(u))),
            Value::Func(env, arg, body) => Ok(Term::Func((), ())),
            Value::Call(f, x) => Ok(Term::Call(Box::new(self.quote(*f)?), Box::new(self.quote(*x)?))),
            Value::EmptyTuple => Ok(Term::EmptyTuple)
        }
    }
}

pub struct Binding {
    val: SubstitutionRef,
    identifier: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct BindingRef(usize);

pub struct Substitution {
    val: Option<Box<Value>>,
    canonical_term: BindingRef,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct SubstitutionRef(usize);

#[derive(Clone, Copy)]
pub struct Environment(BindingRef, SubstitutionRef);





#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Identifier(String),
    Generic(GenericTerm),
    Func(GenericTerm, Box<Term>),
    Call(Box<Term>, Box<Term>),
    EmptyTuple
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct GenericTerm(BindingRef);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
    Generic(GenericValue),
    Func(Environment, GenericTerm, Box<Term>),
    Call(Box<Value>, Box<Value>),
    EmptyTuple
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct GenericValue(SubstitutionRef);

pub enum ElaborationError {
    IdentifierNotFound(String)
}