
use std::{borrow::Borrow, collections::HashMap, sync::atomic::{AtomicU32, Ordering}};

const NEW_BINDING_ID: AtomicU32 = AtomicU32::new(0);
const NEW_SUBSTITUTION_ID: AtomicU32 = AtomicU32::new(0);

#[derive(Clone, Debug, Default)]
pub struct Environment {
    ids: HashMap<GenericTerm, String>, // TODO: switch to Cow<str>
    variables: HashMap<String, GenericTerm>,
    bindings: HashMap<GenericTerm, GenericValue>,
    unbindings: HashMap<GenericValue, GenericTerm>,
    substitutions: HashMap<GenericValue, Value>,
    history: Vec<HistoryItem>,
}

impl Environment {
    pub fn binding_lookup(&self, term: GenericTerm) -> Result<GenericValue, ElaborationError> {
        self.bindings.get(&term).copied().ok_or(ElaborationError::BindingNotFound(term))
    }

    pub fn unbinding_lookup(&self, val: GenericValue) -> Result<GenericTerm, ElaborationError> {
        self.unbindings.get(&val).copied().ok_or(ElaborationError::UnbindingNotFound(val))
    }

    pub fn substitution_lookup(&self, val: GenericValue) -> Option<Value> {
        self.substitutions.get(&val).cloned()
    }

    pub fn term_lookup(&self, s: impl Borrow<String> + Into<String>) -> Result<GenericTerm, ElaborationError> {
        self.variables.get(s.borrow()).cloned().ok_or(ElaborationError::IdentifierNotFound(s.into()))
    }

    pub fn name_lookup(&self, s: GenericTerm) -> Option<&str> {
        self.ids.get(&s).map(|s| s.as_str())
    }

    pub fn binding_insert(&mut self, x: GenericTerm, g: GenericValue) {
        let prev_g = self.bindings.insert(x, g);
        let prev_x = self.unbindings.insert(g, x);
        self.history.push(HistoryItem::Binding(x, prev_g));
        self.history.push(HistoryItem::Unbinding(g, prev_x));
    }

    pub fn substitution_insert(&mut self, g: GenericValue, val: Value) {
        let prev_val = self.substitutions.insert(g, val);
        self.history.push(HistoryItem::Substitution(g, prev_val))
    }

    pub fn id_insert(&mut self, id: String) -> GenericTerm {
        let term = GenericTerm::new();
        let prev_id = self.ids.insert(term, id.clone());
        let prev_term = self.variables.insert(id.clone(), term);
        self.history.push(HistoryItem::Id(term, prev_id));
        self.history.push(HistoryItem::Var(id, prev_term));
        term
    }

    pub fn clone_for_closure(&self) -> Self {
        Environment {
            ids: self.ids.clone(),
            variables: self.variables.clone(),
            bindings: self.bindings.clone(), 
            unbindings: self.unbindings.clone(),
            substitutions: self.substitutions.clone(),
            history: Default::default()
        }
    }

    pub fn start_scope(&mut self) {
        self.history.push(HistoryItem::Scope);
    }

    pub fn end_scope(&mut self) {
        while let Some(item) = self.history.pop() {
            match item {
                HistoryItem::Scope => break,
                HistoryItem::Binding(x, Some(g))        => { self.bindings.insert(x, g); },
                HistoryItem::Binding(x, None)           => { self.bindings.remove(&x); },
                HistoryItem::Unbinding(g, Some(x))      => { self.unbindings.insert(g, x); },
                HistoryItem::Unbinding(g, None)         => { self.unbindings.remove(&g); },
                HistoryItem::Substitution(g, Some(v))   => { self.substitutions.insert(g, v); },
                HistoryItem::Substitution(g, None)      => { self.substitutions.remove(&g); },
                HistoryItem::Id(g, Some(id))            => { self.ids.insert(g, id); },
                HistoryItem::Id(g, None)                => { self.ids.remove(&g); },
                HistoryItem::Var(id, Some(g))           => { self.variables.insert(id, g); },
                HistoryItem::Var(id, None)              => { self.variables.remove(&id); }
            }
        }
    }

    pub fn evaluate_term(&mut self, term: Term) -> Result<Value, ElaborationError> {
        match term {
            Term::Id(id) => {
                let x = self.term_lookup(&id)?;
                let g = self.binding_lookup(x)?;
                self.evaluate_neutral_value(g.into())
            },
            Term::Generic(term) => self.evaluate_neutral_value(self.binding_lookup(term)?.into()),
            Term::IdFunc(id, body) => {
                self.start_scope();

                let arg = self.id_insert(id);
                let arg_val = GenericValue::new();
                self.binding_insert(arg, arg_val);
                let body_val = self.evaluate_term(*body)?;
                let f = Value::Func(self.clone_for_closure(), arg, Box::new(self.quote(body_val)?));

                self.end_scope();
                Ok(f)
            }
            Term::Func(arg, body) => {
                self.start_scope();

                let arg_val = GenericValue::new();
                self.binding_insert(arg, arg_val);
                let body_val = self.evaluate_term(*body)?;
                let f = Value::Func(self.clone_for_closure(), arg, Box::new(self.quote(body_val)?));

                self.end_scope();
                Ok(f)
            },
            Term::Call(f, x) => {
                let f_val = self.evaluate_term(*f)?;
                let x_val = self.evaluate_term(*x)?;
                self.evaluate_call(f_val, Box::new(x_val))
            },
            Term::EmptyTuple => Ok(Value::EmptyTuple)
        }
    }

    #[inline]
    pub fn evaluate_neutral_value(&mut self, neutral: NeutralValue) -> Result<Value, ElaborationError> {
        match neutral {
            NeutralValue::Generic(g) => match self.substitution_lookup(g) {
                Some(v) => if let Value::Neutral(n) = v {
                    self.evaluate_neutral_value(n)
                } else {
                    Ok(v)
                }
                None => Ok(neutral.into()),
            },
            NeutralValue::Call(f, v) => self.evaluate_call((*f).into(), v),
        }
    }

    #[inline]
    fn evaluate_call(&mut self, f: Value, v: Box<Value>) -> Result<Value, ElaborationError> {
        match f {
            Value::Neutral(n) => Ok(NeutralValue::Call(Box::new(n), v).into()),
            Value::Func(mut env, arg, body) => {
                env.start_scope();

                let g = env.binding_lookup(arg).unwrap();
                env.substitution_insert(g, *v);
                let y = env.evaluate_term(*body)?;
                
                env.end_scope();
                Ok(y)
            },
            Value::EmptyTuple => Err(ElaborationError::InvalidCall(Value::EmptyTuple)),
        }
    }

    pub fn quote(&mut self, value: Value) -> Result<Term, ElaborationError> {
        match value {
            Value::Neutral(NeutralValue::Generic(g)) => self.unbinding_lookup(g).map(|x| x.into()),
            Value::EmptyTuple => Ok(Term::EmptyTuple),
            Value::Func(mut env, arg, body) => {
                let body_val = env.evaluate_term(*body)?;
                Ok(Term::Func(arg, Box::new(env.quote(body_val)?)))
            },
            Value::Neutral(NeutralValue::Call(n, v)) => Ok(Term::Call(
                Box::new(self.quote((*n).into())?), 
                Box::new(self.quote(*v)?)
            ))
        }
    }
}


#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct GenericTerm(u32);

impl GenericTerm {
    fn new() -> GenericTerm {
        let id = NEW_BINDING_ID.load(Ordering::Relaxed);
        NEW_BINDING_ID.store(id + 1, Ordering::Relaxed);
        GenericTerm(id)
    }
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct GenericValue(u32);

impl GenericValue {
    fn new() -> GenericValue {
        let id = NEW_SUBSTITUTION_ID.load(Ordering::Relaxed);
        NEW_SUBSTITUTION_ID.store(id + 1, Ordering::Relaxed);
        GenericValue(id)
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Id(String), // TODO: separate out stage of converting IDs to GenericTerms as abstract binding
    Generic(GenericTerm),
    IdFunc(String, Box<Term>),
    Func(GenericTerm, Box<Term>),
    Call(Box<Term>, Box<Term>),
    EmptyTuple,
}

impl From<GenericTerm> for Term {
    fn from(term: GenericTerm) -> Self {
        Term::Generic(term)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Neutral(NeutralValue),
    Func(Environment, GenericTerm, Box<Term>),
    EmptyTuple
}

impl From<GenericValue> for Value {
    fn from(g: GenericValue) -> Self {
        Value::Neutral(NeutralValue::Generic(g))
    }
}

impl From<NeutralValue> for Value {
    fn from(n: NeutralValue) -> Self {
        Value::Neutral(n)
    }
}

#[derive(Clone, Debug)]
pub enum NeutralValue {
    Generic(GenericValue),
    Call(Box<NeutralValue>, Box<Value>)
}

impl From<GenericValue> for NeutralValue {
    fn from(g: GenericValue) -> Self {
        NeutralValue::Generic(g)
    }
}

#[derive(Clone, Debug)]
pub enum ElaborationError {
    IdentifierNotFound(String),
    BindingNotFound(GenericTerm),
    UnbindingNotFound(GenericValue),
    InvalidCall(Value)
}

#[derive(Clone, Debug)]
pub enum HistoryItem {
    Scope,
    Binding(GenericTerm, Option<GenericValue>),
    Unbinding(GenericValue, Option<GenericTerm>),
    Substitution(GenericValue, Option<Value>),
    Id(GenericTerm, Option<String>),
    Var(String, Option<GenericTerm>)
}


#[cfg(test)]
pub mod tests {
    use super::*;

    fn func(arg: impl Into<String>, body: Term) -> Term {
        Term::IdFunc(arg.into(), Box::new(body))
    }

    fn call(f: Term, x: Term) -> Term {
        Term::Call(Box::new(f), Box::new(x))
    }

    fn var(id: impl Into<String>) -> Term {
        Term::Id(id.into())
    }

    #[test]
    fn basic() {
        println!("{:?}", Environment::default().evaluate_term(call(func("x", Term::EmptyTuple), Term::EmptyTuple)));
    }

    #[test]
    fn basic_2() {
        println!("{:?}", Environment::default().evaluate_term(call(func("x", var("x")), Term::EmptyTuple)));
    }

    #[test]
    fn basic_3() {
        println!("{:?}", Environment::default().evaluate_term(call(func("x", func("y", var("x"))), Term::EmptyTuple)))
    }
}