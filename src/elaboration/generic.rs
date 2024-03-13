use std::sync::atomic::{AtomicU32, Ordering};
use std::fmt::Debug;

static NEW_BINDING_ID: AtomicU32 = AtomicU32::new(0);
static NEW_SUBSTITUTION_ID: AtomicU32 = AtomicU32::new(0);


#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct GenericTerm(u32);

impl GenericTerm {
    pub fn new() -> GenericTerm {
        let id = NEW_BINDING_ID.fetch_add(1, Ordering::SeqCst);
        GenericTerm(id)
    }
}

impl Debug for GenericTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{}", self.0)
    }
}

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct GenericValue(u32);

impl GenericValue {
    pub fn new() -> GenericValue {
        let id = NEW_SUBSTITUTION_ID.fetch_add(1, Ordering::SeqCst);
        GenericValue(id)
    }
}

impl Debug for GenericValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "g{}", self.0)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn aaa() {
        let a = GenericTerm::new();
        let b = GenericTerm::new();
        println!("{:?}, {:?}", a, b);
    }

    #[test]
    fn bbb() {
        
        dbg!(NEW_BINDING_ID.fetch_add(1, Ordering::SeqCst));
        dbg!(NEW_BINDING_ID.fetch_add(1, Ordering::SeqCst));
        dbg!(NEW_BINDING_ID.fetch_add(1, Ordering::SeqCst));
    }

    #[test]
    fn ccc() {
        let a = AtomicU32::new(0);
        dbg!(a.fetch_add(1, Ordering::SeqCst));
        dbg!(a.fetch_add(1, Ordering::SeqCst));
        dbg!(a.fetch_add(1, Ordering::SeqCst));
    }
}