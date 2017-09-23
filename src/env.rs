use std::borrow::Borrow;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::ops::Deref;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use Expr;

type Key = Rc<String>;

pub struct Env {
    defs: HashMap<Key, Expr>,
    outer: Option<EnvRef>
}
pub type EnvRef = Rc<RefCell<Env>>;

impl Env {
    pub fn new() -> EnvRef {
        let e = Env {
            defs: HashMap::new(),
            outer: None
        };
        Rc::new(RefCell::new(e))
    }
    pub fn extend(env: &EnvRef, bindings: Vec<(Rc<String>, Expr)>) -> EnvRef {
        let e = Env {
            defs: HashMap::from_iter(bindings),
            outer: Some(Rc::clone(env))
        };
        Rc::new(RefCell::new(e))
    }

    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<Expr>
        where Key: Borrow<Q>,
              Q: Hash + Eq {
        self.defs.get(k).map_or_else(
            || self.outer.as_ref().and_then(
                |env| env.deref().borrow().get(k)),
            |e| Some(e.clone()) )
	}

    pub fn set(&mut self, k: Key, v: Expr) -> Option<Expr> {
	    self.defs.insert(k,v)
    }

}
