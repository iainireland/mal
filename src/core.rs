use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use Expr;
use errors::*;

lazy_static! {
   pub static ref GLOBAL_NAMESPACE: HashMap<&'static str, PrimFn> = {
	   let mut result = HashMap::new();
 		result.insert("+", PrimFn{func: prim_add});
 		result.insert("-", PrimFn{func: prim_sub});
 		result.insert("*", PrimFn{func: prim_mul});
 		result.insert("/", PrimFn{func: prim_div});
      result
   };
}

pub struct PrimFn {
   pub func: fn(&Vec<Expr>) -> Result<Expr>
}
impl Clone for PrimFn {
   fn clone(&self) -> Self {
      PrimFn {
         func: self.func
      }
   }
}

impl Debug for PrimFn {
   fn fmt(&self, f: &mut Formatter) -> ::std::fmt::Result {
      write!(f, "<primitive function>")
   }
}

macro_rules! prim_arith {
   ($name:ident, $op:expr) =>
   (fn $name(operands: &Vec<Expr>) -> Result<Expr> {
      if operands.len() < 2 {
         return Err("Not enough operands".into());
      }
      let nums = operands.iter().map(|x| match *x {
         Expr::Number(n) => Ok(n),
         _ => Err("Non-number in arithmetic".into())
      }).collect::<Result<Vec<i32>>>()?;
      let first = nums[0];
      let result = nums.iter().skip(1).fold(first, $op);
      Ok(Expr::Number(result))
   });
}

prim_arith!(prim_add, |a,b| a+b);
prim_arith!(prim_sub, |a,b| a-b);
prim_arith!(prim_mul, |a,b| a*b);
prim_arith!(prim_div, |a,b| a/b);


