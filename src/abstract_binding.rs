use std::{
    // borrow::Borrow, 
    borrow::Borrow, cell::{Ref, RefCell}, rc::Rc, sync::{atomic::{AtomicU32, Ordering}, Mutex}
};

pub const UNIQUE_BINDING_ID: AtomicU32 = AtomicU32::new(0);
pub const UNIQUE_SUBSTITUTION_ID: AtomicU32 = AtomicU32::new(0);


#[derive(Debug, Clone)]
pub struct Binding {
    id: u32,
    name: String,
    to: GenericValue,
    prev: Option<Rc<RefCell<Binding>>>,
}

impl Binding {
    fn new(env: &Environment, name: String) -> Rc<RefCell<Binding>> {
        let id = UNIQUE_BINDING_ID.load(Ordering::Relaxed);
        UNIQUE_BINDING_ID.store(id + 1, Ordering::Relaxed);
        let mut b = Rc::new(RefCell::new(Binding {
            id,
            name,
            to: todo!(),
            prev: env.bindings.clone()
        }));
        // let mut generic_value = Substitution::new_unknown(env, b.clone());
        // b.to = GenericValue(s);
    }
}

impl PartialEq for Binding {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Binding {}

impl Binding {
    pub fn prev<'a>(&'a self) -> Option<Ref<'a, Binding>> {
        let Some(prev) = &self.prev else { return None; };
        let prev: &RefCell<Binding> = prev.borrow(); 
        Some(prev.borrow())
    }

    pub fn lookup(&self, mut v: VariableRef) -> Option<GenericValue> {
        if self.name == v.name {
            if v.index == 0 {
                Some(self.to.clone())
            }
            else {
                v.index -= 1;
                self.prev()?.lookup(v)
            }
        }
        else {
            self.prev()?.lookup(v)
        }
    }

    pub fn inverse_lookup(&self, mut v: GenericValue) -> Option<VariableRef> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Substitution {
    id: u32,
    from: GenericValue,
    to: Value,
    prev: Option<Rc<RefCell<Substitution>>>,
}

impl PartialEq for Substitution {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Substitution {}

impl Substitution {
    pub fn prev<'a>(&'a self) -> Option<Ref<'a, Substitution>> {
        let Some(prev) = &self.prev else { return None; };
        let prev: &RefCell<Substitution> = prev.borrow(); 
        Some(prev.borrow())
    }

    pub fn lookup(&self, v: GenericValue) -> Option<Value> {
        if self.from == v {
            Some(self.to.clone())
        }
        else {
            self.prev()?.lookup(v)
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Environment {
    pub bindings: Option<Rc<RefCell<Binding>>>,
    pub substitutions: Option<Rc<RefCell<Substitution>>>,
    pub counter: u32
}

impl Environment {
    pub fn shadowed_by(&self, name: Name, value: Value) -> Environment {
        if let Value::GenericValue(g) = value {
            Environment {
                bindings: Some(Rc::new(RefCell::new(
                    Binding {
                        name,
                        to: g,
                        prev: self.bindings.clone()
                    },
                ))),
                substitutions: self.substitutions.clone(),
                counter: self.counter
            }
        }
        else {
            let new_generic = GenericValue(self.counter);
            Environment {
                bindings: Some(Rc::new(RefCell::new(
                    Binding {
                        name,
                        to: new_generic,
                        prev: self.bindings.clone()
                    }
                ))),
                substitutions: Some(Rc::new(RefCell::new(Substitution {
                    from: new_generic,
                    to: value,
                    prev: self.substitutions.clone()
                }))),
                counter: self.counter + 1
            }
        }
    }

    pub fn bindings<'a>(&'a self) -> Option<Ref<'a, Binding>> {
        let Some(bindings) = &self.bindings else { return None; };
        let bindings: &RefCell<Binding> = bindings.borrow(); 
        Some(bindings.borrow())
    }

    pub fn substitutions<'a>(&'a self) -> Option<Ref<'a, Substitution>> {
        let Some(substitutions) = &self.substitutions else { return None; };
        let substitutions: &RefCell<Substitution> = substitutions.borrow(); 
        Some(substitutions.borrow())
    }

    pub fn lookup(&self, var: VariableRef) -> Option<Value> {
        let gen = self.bindings()?.lookup(var)?;
        Some(match self.substitutions()?.lookup(gen) {
            Some(v) => v,
            None => Value::GenericValue(gen)
        })
    }

    pub fn evaluate(&self, t: Term) -> Result<Value, EvaluationError> {
        match t {
            Term::Call(f_term, x_term) => {
                let f_val = self.evaluate(*f_term)?;
                let x_val = self.evaluate(*x_term)?;

                match f_val {
                    Value::Func(env, f_arg, f_body) => {
                        env.shadowed_by(f_arg, x_val).evaluate(*f_body)
                    },
                    Value::GenericValue(f_gen) => {
                        Ok(Value::Call(Box::new(NeutralValue::GenericValue(f_gen)), Box::new(x_val)))
                    },
                    Value::Call(_, _) => todo!(),
                    Value::EmptyTuple => todo!()
                }
            }
            Term::Variable(var) => {
                self.lookup(var.clone().last_ref()).ok_or(EvaluationError::VariableNotFound(var))
            },
            Term::Func(arg, body) => Ok(Value::Func(self.clone(), arg.clone(), body)), // Box::new(self.canonical_with_unknown(arg, *body)?)
            Term::EmptyTuple => Ok(Value::EmptyTuple)
        }
    }

    pub fn quote(&self, v: Value) -> Result<Term, EvaluationError> {
        match v {
            Value::GenericValue(_) => todo!(),
            Value::Call(_, _) => todo!(),
            Value::Func(_, _, _) => todo!(),
            Value::EmptyTuple => Ok(Term::EmptyTuple),
        }
    }

    pub fn canonical(&self, term: Term) -> Result<Term, EvaluationError> {
        todo!()
    }

    pub fn canonical_with_unknown(&self, arg: Name, term: Term) -> Result<Term, EvaluationError> {
        todo!()
    }
}

// TODO: Name could be wayy more efficient
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Name(String);

impl Name {
    pub fn last_ref(self) -> VariableRef {
        VariableRef {
            index: 0,
            name: self
        }
    }
}

pub struct VariableRef {
    index: usize,
    name: Name
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GenericTerm(Rc<RefCell<Binding>>);



#[derive(Debug, Clone)]
pub enum Term {
    Variable(Name),
    Call(Box<Term>, Box<Term>),
    Func(Name, Box<Term>),
    EmptyTuple,
    // Let(Name, )
    // Tuple(Vec<Term>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GenericValue(Rc<RefCell<Substitution>>);

#[derive(Clone, Debug)]
pub enum NeutralValue {
    GenericValue(GenericValue),
    Call(Box<NeutralValue>, Box<Value>)
}

#[derive(Clone, Debug)]
pub enum Value {
    GenericValue(GenericValue),
    Call(Box<NeutralValue>, Box<Value>),
    Func(Environment, Name, Box<Term>),
    EmptyTuple,
    // Tuple(Vec<Value>)
}

#[derive(Debug)]
pub enum EvaluationError {
    Todo,
    VariableNotFound(Name),
}


#[cfg(test)]
mod tests {
    use super::*;

    fn func(arg: impl Into<String>, body: Term) -> Term {
        Term::Func(Name(arg.into()), Box::new(body))
    }

    fn call(func: Term, arg: Term) -> Term {
        Term::Call(Box::new(func), Box::new(arg))
    }

    fn var(name: impl Into<String>) -> Term {
        Term::Variable(Name(name.into()))
    }

    #[test]
    fn a() {
        let env = Environment::default(); 
        println!("{:?}", env.evaluate(call(func("x", Term::EmptyTuple), Term::EmptyTuple)));
    }

    #[test]
    fn b() {
        let env = Environment::default();
        println!("{:?}", env.evaluate(call(func("x", var("x")), func("x", Term::EmptyTuple))));
    }

    #[test]
    fn c() {
        let env = Environment::default();
        println!("{:?}", env.evaluate(
            call(
                call(
                    func("x", func("x", var("x"))), 
                    func("x", Term::EmptyTuple)
                ), 
                Term::EmptyTuple
            )
        ));
    }
}
