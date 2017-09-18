use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

use Expr;

type Key = String;

pub struct Env<'a> {
   defs: HashMap<Key, Expr>,
   outer: Option<&'a Env<'a>>
}

impl<'a> Env<'a> {
   pub fn new() -> Self {
      Env {
         defs: HashMap::new(), 
         outer: None
      }
   }

   pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&Expr> 
      where Key: Borrow<Q>,
            Q: Hash + Eq {
      self.defs.get(k)
               .or_else(|| self.outer.and_then(|outer_env| outer_env.get(k)))
	}

   pub fn set(&mut self, k: Key, v: Expr) -> Option<Expr> {
	   self.defs.insert(k,v)
   }
}