use std::collections::{HashMap, HashSet};
use super::*;
use std::hash::Hash;

#[derive(Clone, Debug)]
pub struct Environment {
    depth: usize,
    bindings: HashMap<GenericTerm, (GenericValue, usize)>,
    unbindings: HashMap<GenericValue, (GenericTerm, usize)>,
    substitutions: HashMap<GenericValue, (Value, usize)>,
    history: History
}

impl Environment {
    pub fn get_binding(&self, x: GenericTerm) -> Result<GenericValue, ElaborationError> {
        self.bindings.get(&x).map(|(g, _)| g).copied().ok_or(ElaborationError::BindingNotFound(x))
    }

    pub fn get_unbinding(&self, g: GenericValue) -> Result<GenericTerm, ElaborationError> {
        self.unbindings.get(&g).map(|(x, _)| x).copied().ok_or(ElaborationError::UnbindingNotFound(g))
    }

    pub fn get_substitution(&self, g: GenericValue) -> Option<Value> {
        self.substitutions.get(&g).map(|(v, _)| v).cloned()
    }

    pub fn get(&self, x: GenericTerm) -> Result<Value, ElaborationError> {
        let g = self.get_binding(x)?;
        Ok(self.get_substitution(g).unwrap_or(g.into()))
    }

    pub fn insert_binding(&mut self, x: GenericTerm, g: GenericValue) {
        let prev_bind_entry = self.bindings.insert(x, (g, self.depth));
        let prev_unbind_entry = self.unbindings.insert(g, (x, self.depth));
        self.history.remember_binding(x, prev_bind_entry, self.depth);
        self.history.remember_unbinding(g, prev_unbind_entry, self.depth);
    }

    pub fn insert_substitution(&mut self, g: GenericValue, v: Value) {
        let prev_entry = self.substitutions.insert(g, (v, self.depth));
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

    pub fn start_scope(&mut self) {
        self.history.push_row();
        self.depth += 1;
    }

    pub fn end_scope(&mut self) {
        let HistoryRow { bindings, unbindings, substitutions } = self.history.pop_row();
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

    pub fn clone_for_closure(&mut self) -> Self {
        Environment {
            depth: self.depth,
            bindings: self.bindings.clone(),
            unbindings: self.unbindings.clone(),
            substitutions: self.substitutions.clone(),
            history: History::new(self.depth),
        }
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

    fn remember_binding(&mut self, x: GenericTerm, g: Option<(GenericValue, usize)>, depth: usize) {
        self.rows[depth - self.start_depth].bindings.insert(x, g);
    }

    fn remember_unbinding(&mut self, g: GenericValue, x: Option<(GenericTerm, usize)>, depth: usize) {
        self.rows[depth - self.start_depth].unbindings.insert(g, x);
    }

    fn remember_substitution(&mut self, g: GenericValue, v: Option<(Value, usize)>, depth: usize) {
        self.rows[depth - self.start_depth].substitutions.insert(g, v);
    }
}

#[derive(Default, Clone, Debug)]
struct HistoryRow {
    bindings: HashMap<GenericTerm, Option<(GenericValue, usize)>>,
    unbindings: HashMap<GenericValue, Option<(GenericTerm, usize)>>,
    substitutions: HashMap<GenericValue, Option<(Value, usize)>>,
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
                history: Default::default()
            }, 
            surface_variables: Default::default(),
            keywords: Default::default() 
        };
        ctx .bind("Unit", Value::Unit)
            .bind("Type", Value::Type)
            .add_keyword("Unit")
            .add_keyword("Type")
            .add_keyword("Void")
            .add_keyword("Nat");
        ctx
    }

    pub fn get_type(&self, x: GenericTerm) -> Result<Value, ElaborationError> {
        self.types.get(&x).cloned().map(|(ty, _)| ty).ok_or(ElaborationError::TypeNotFound(x))
    }

    pub fn insert_type(&mut self, x: GenericTerm, ty: Value) {
        let prev_entry = self.types.insert(x, (ty, self.depth));
        self.history.remember(x, prev_entry, self.depth);
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
