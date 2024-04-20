use std::{collections::{HashMap, HashSet}, fmt::Debug};
use super::*;
use std::hash::Hash;

#[derive(Clone, Debug)]
pub struct Environment {
    depth: usize,
    bindings:       HashMap<GenericTerm,  Depthed<GenericValue>>,
    unbindings:     HashMap<GenericValue, Depthed<GenericTerm>>,
    substitutions:  HashMap<GenericValue, Depthed<Value>>,
    pub(crate) holes:          HashMap<Hole,         Depthed<Option<Value>>>,
    history: History
}

pub struct Depthed<T> {
    item: T,
    depth: usize
}

impl<T: Clone> Clone for Depthed<T> {
    fn clone(&self) -> Self { Self { item: self.item.clone(), depth: self.depth.clone() }}
}

impl<T: Debug> Debug for Depthed<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.item)
    }
}

impl Environment {
    pub fn get_binding(&self, x: GenericTerm) -> Option<GenericValue> {
        self.bindings.get(&x).map(|g| g.item)
    }

    pub fn get_unbinding(&self, g: GenericValue) -> Option<GenericTerm> {
        self.unbindings.get(&g).map(|x| x.item)
    }

    pub fn get_substitution(&self, g: GenericValue) -> Option<Value> {
        self.substitutions.get(&g).map(|v| v.item.clone())
    }

    pub fn try_get_binding(&self, x: GenericTerm) -> Result<GenericValue, ElaborationError> {
        self.get_binding(x).ok_or(ElaborationError::BindingNotFound(x))
    }

    pub fn try_get_unbinding(&self, g: GenericValue) -> Result<GenericTerm, ElaborationError> {
        self.get_unbinding(g).ok_or(ElaborationError::UnbindingNotFound(g))
    }

    pub fn get(&self, x: GenericTerm) -> Option<Value> {
        let g = self.get_binding(x)?;
        Some(self.get_substitution(g).unwrap_or(g.into()))
    }

    pub fn insert_binding(&mut self, x: GenericTerm, g: GenericValue) {
        let prev_bind_entry = self.bindings.insert(x, Depthed { item: g, depth: self.depth });
        let prev_unbind_entry = self.unbindings.insert(g, Depthed { item: x, depth: self.depth });
        self.history.remember_binding(x, prev_bind_entry, self.depth);
        self.history.remember_unbinding(g, prev_unbind_entry, self.depth);
    }

    pub fn insert_substitution(&mut self, g: GenericValue, v: Value) {
        debug_assert!(!v.is_generic_value());
        let prev_entry = self.substitutions.insert(g, Depthed { item: v, depth: self.depth });
        self.history.remember_substitution(g, prev_entry, self.depth)
    }

    pub fn insert(&mut self, x: GenericTerm, v: Value) {
        match v {
            Value::Neutral(NeutralValue::Generic(g)) => {
                self.insert_binding(x, g);
            },
            v => {
                let g = GenericValue::new();
                self.insert_binding(x, g);
                self.insert_substitution(g, v);
            }
        }
    }

    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn start_scope(&mut self) {
        self.history.push_row();
        self.depth += 1;
    }

    pub fn end_scope(&mut self) {
        let HistoryRow { bindings, unbindings, substitutions, holes } = self.history.pop_row();
        for (h, opt_v) in holes {
            let value = self.holes.remove(&h);
            assert!(value.is_some()); // TODO: improve error

            insert_or_remove(&mut self.holes, h, opt_v)
        }
        for (x, opt_g) in bindings {
            insert_or_remove(&mut self.bindings, x, opt_g);
        }
        for (g, opt_x) in unbindings {
            insert_or_remove(&mut self.unbindings, g, opt_x);
        }
        for (g, opt_v) in substitutions {
            insert_or_remove(&mut self.substitutions, g, opt_v);
        }
        
        self.depth -= 1;
    }

    pub fn end_scope_to_depth(&mut self, desired_depth: usize) {
        while self.depth > desired_depth {
            self.end_scope();
        }
    }

    pub fn clone_for_closure(&mut self) -> Self {
        Environment {
            depth: self.depth,
            bindings: self.bindings.clone(),
            unbindings: self.unbindings.clone(),
            substitutions: self.substitutions.clone(),
            holes: HashMap::new(),
            history: History::new(self.depth),
        }
    }

    pub fn fresh_hole(&mut self, h: Hole) {
        let prev_entry = self.holes.insert(h, Depthed { item: None, depth: self.depth });
        self.history.remember_hole(h, prev_entry, self.depth)
    }

    pub fn new_hole(&mut self) -> Hole {
        let h = Hole::new();
        self.fresh_hole(h);
        dbg!(&self.holes);
        h
    }

    pub fn get_hole(&self, h: Hole) -> Option<Value> {
        self.holes.get(&h).map(|v| v.item.clone()).flatten()
    }

    pub(crate) fn unify_empty_holes(&mut self, h1: Hole, h2: Hole) -> Result<(), ElaborationError> {
        debug_assert_ne!(h1, h2);
        let Some(v1) = self.holes.remove(&h1) else {
            return Err(ElaborationError::HoleNotFound(h1));
        };
        let Some(v2) = self.holes.remove(&h2) else {
            return Err(ElaborationError::HoleNotFound(h2));
        };

        debug_assert!(v1.item.is_none());
        debug_assert!(v2.item.is_none());

        let ((_h1, mut v1), (h2, v2)) = if v1.depth > v2.depth || (v1.depth == v2.depth && h1 > h2) {
            ((h1, v1), (h2, v2))
        } else {
            ((h2, v2), (h1, v1))
        };

        v1.item = Some(Value::Hole(h2));

        self.holes.insert(h1, v1);
        self.holes.insert(h2, v2);

        Ok(())
    }

    pub(crate) fn assign_to_empty_hole(&mut self, h: Hole, v: Value) -> Result<(), ElaborationError> {
        let h = self.holes.get_mut(&h).ok_or(ElaborationError::HoleNotFound(h))?;
        debug_assert!(h.item.is_none());
        h.item = Some(v);
        Ok(())
    }

    /// Updates `v` to either be a non-hole value, or a hole which is bound to nothing
    pub(crate) fn update_hole(&mut self, v: &mut Value) -> Result<(), ElaborationError> {
        while let Value::Hole(h) = v {
            if let Some(v_next) = self.get_hole(*h) {
                *v = v_next;
            }
            else {
                break
            }
        }
        Ok(()) // TODO: make function not return result
        // loop {
        //     if let Value::Hole(h) = v {
        //         if let Some(v2) = self.holes.get(h) {
        //             *v = v2.item
        //         }
        //         else {
        //             return Err(ElaborationError::HoleNotFound(*h));
        //         }
        //     }
        //     else {
        //         break Ok(());
        //     }
        // }
    }
}

#[inline]
fn insert_or_remove<U: Hash + Eq, V>(set: &mut HashMap<U, V>, k: U, opt_v: Option<V>) {
    match opt_v {
        Some(v) => set.insert(k, v),
        None => set.remove(&k)
    };
}

#[derive(Clone, Debug)]
struct History {
    start_depth: usize,
    rows: Vec<HistoryRow>
}

impl Default for History {
    fn default() -> Self {
        History::new(0)
    }
}

impl History {
    fn new(start_depth: usize) -> Self {
        History { start_depth, rows: vec![HistoryRow::default()] }
    }

    fn push_row(&mut self) {
        self.rows.push(HistoryRow::default());
    } 

    fn pop_row(&mut self) -> HistoryRow {
        self.rows.pop().unwrap()
    }

    fn remember_binding(&mut self, x: GenericTerm, g: Option<Depthed<GenericValue>>, depth: usize) {
        self.rows[depth - self.start_depth].bindings.insert(x, g);
    }

    fn remember_unbinding(&mut self, g: GenericValue, x: Option<Depthed<GenericTerm>>, depth: usize) {
        self.rows[depth - self.start_depth].unbindings.insert(g, x);
    }

    fn remember_substitution(&mut self, g: GenericValue, v: Option<Depthed<Value>>, depth: usize) {
        self.rows[depth - self.start_depth].substitutions.insert(g, v);
    }

    fn remember_hole(&mut self, h: Hole, v: Option<Depthed<Option<Value>>>, depth: usize) {
        self.rows[depth - self.start_depth].holes.insert(h, v);
        dbg!(&self.rows[depth - self.start_depth].holes);
    }
}

#[derive(Default, Clone, Debug)]
struct HistoryRow {
    bindings: HashMap<GenericTerm, Option<Depthed<GenericValue>>>,
    unbindings: HashMap<GenericValue, Option<Depthed<GenericTerm>>>,
    substitutions: HashMap<GenericValue, Option<Depthed<Value>>>,
    holes: HashMap<Hole, Option<Depthed<Option<Value>>>>,
}

#[derive(Debug)]
pub struct Context {
    depth: usize,
    types: HashMap<GenericTerm, (Value, usize)>,
    pub(crate) ids: HashMap<GenericTerm, String>, // TODO: should this be pub?
    history: TypeHistory,
    global_environment: Environment,
    pub(crate) surface_variables: HashMap<String, GenericTerm>,
    pub(crate) keywords: HashSet<String> // TODO: add keywords
}

impl Context {
    pub fn empty() -> Self {
        let mut ctx = Self { 
            depth: 0, 
            types: Default::default(), 
            ids: Default::default(), 
            history: Default::default(), 
            global_environment: Environment { 
                depth: 0, 
                bindings: Default::default(), 
                unbindings: Default::default(), 
                substitutions: Default::default(), 
                holes: Default::default(),
                history: Default::default()
            }, 
            surface_variables: Default::default(),
            keywords: Default::default() 
        };
        ctx .bind("Unit", Value::Unit)
            .bind("Type", Value::Type)
            .bind("Nat", Value::Nat)
            .bind("Str", Value::Str)
            .bind("_ + _", binary_nat_func(|l, r| l + r))
            .bind("_ - _", binary_nat_func(|l, r| l - r))
            .bind("_ * _", binary_nat_func(|l, r| l * r))
            .bind("_ / _", binary_nat_func(|l, r| l / r))
            .bind("_ % _", binary_nat_func(|l, r| l % r))
            .add_keyword("Unit")
            .add_keyword("Type")
            .add_keyword("Nat")
            .add_keyword("Str")
            .add_keyword("_ + _")
            .add_keyword("_ - _")
            .add_keyword("_ * _")
            .add_keyword("_ / _")
            .add_keyword("_ % _")

            .add_keyword("Void");
        ctx
    }

    pub fn get_type(&self, x: GenericTerm) -> Option<Value> {
        self.types.get(&x).cloned().map(|(ty, _)| ty)
    }

    pub fn try_get_type(&self, x: GenericTerm) -> Result<Value, ElaborationError> {
        self.get_type(x).ok_or(ElaborationError::TypeNotFound(x))
    }

    pub fn insert_type(&mut self, x: GenericTerm, ty: Value) {
        let prev_entry = self.types.insert(x, (ty, self.depth));
        self.history.remember(x, prev_entry, self.depth);
    }

    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn start_scope(&mut self) {
        self.history.push_row();
        self.depth += 1;
    }

    pub fn end_scope(&mut self) {
        let row = self.history.pop_row();
        for (x, opt_ty) in row {
            insert_or_remove(&mut self.types, x, opt_ty);
        }
        self.depth -= 1;
    }

    pub fn end_scope_to_depth(&mut self, desired_depth: usize) {
        while self.depth > desired_depth {
            self.end_scope();
        }
    }

    pub fn global_environment(&self) -> Environment {
        self.global_environment.clone()
    }

    pub(crate) fn add_keyword(&mut self, kw: impl Into<String>) -> &mut Self {
        self.keywords.insert(kw.into());
        self
    }

    pub fn bind(&mut self, id: impl Into<String>, v: Value) -> &mut Self {
        let x = GenericTerm::new();
        let g = GenericValue::new();
        self.surface_variables.insert(id.into(), x);
        self.global_environment.insert_binding(x, g);
        self.global_environment.insert_substitution(g, v);
        self
    }

    pub fn bind_and_eval(&mut self, id: impl Into<String>, s: SurfaceTerm) -> &mut Self {
        let Ok(t) = to_core(self, s) else { todo!() /* TODO: improve error */ };
        let Ok(v) = evaluate(self, &mut self.global_environment(), t) else { todo!() /* TODO: improve error */};
        
        self.bind(id, v)
    }

    
}

fn binary_nat_func(f: fn(BigUint, BigUint) -> BigUint) -> Value { 
    Value::ExternalFunc(ExternalFunction::new(
        Pattern::blank(), 
        Value::TupleType(Pattern::blank(), Box::new(Value::Nat), Box::new(Term::Nat)), 
        Term::Nat, 
        move |v| {
            let Value::Tuple(l, r) = v else { unreachable!() };
            let Value::NatNum(l) = *l else { unreachable!() };
            let Value::NatNum(r) = *r else { unreachable!() };
            Ok(Value::NatNum(f(l, r)))
        }
    ))
}

#[derive(Debug)]
struct TypeHistory {
    start_depth: usize,
    rows: Vec<TypeHistoryRow>
}

impl Default for TypeHistory {
    fn default() -> Self {
        TypeHistory::new(0)
    }
}

type TypeHistoryRow = HashMap<GenericTerm, Option<(Value, usize)>>;

impl TypeHistory {
    fn new(start_depth: usize) -> Self {
        TypeHistory { start_depth, rows: vec![TypeHistoryRow::default()] }
    }

    fn push_row(&mut self) {
        self.rows.push(TypeHistoryRow::default());
    } 

    fn pop_row(&mut self) -> TypeHistoryRow {
        self.rows.pop().unwrap()
    }

    fn remember(&mut self, x: GenericTerm, t: Option<(Value, usize)>, depth: usize) {
        self.rows[depth - self.start_depth].insert(x, t);
    }
}
