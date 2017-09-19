use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use itertools::Itertools;

use Expr;
use errors::*;

lazy_static! {
   pub static ref GLOBAL_NAMESPACE: HashMap<&'static str, PrimFn> = {
	   let mut result = HashMap::new();
 		result.insert("+", PrimFn{func: prim_add});
 		result.insert("-", PrimFn{func: prim_sub});
 		result.insert("*", PrimFn{func: prim_mul});
 		result.insert("/", PrimFn{func: prim_div});
 		result.insert("=", PrimFn{func: prim_eq});
 		result.insert("<", PrimFn{func: prim_lt});
 		result.insert(">", PrimFn{func: prim_gt});
 		result.insert("<=", PrimFn{func: prim_le});
 		result.insert(">=", PrimFn{func: prim_ge});
 		result.insert("prn", PrimFn{func: prim_prn});
 		result.insert("list", PrimFn{func: prim_list});
 		result.insert("list?", PrimFn{func: prim_listp});
 		result.insert("empty?", PrimFn{func: prim_emptyp});
 		result.insert("count", PrimFn{func: prim_count});
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
impl PartialEq for PrimFn {
   fn eq(&self, _: &PrimFn) -> bool {
      false
   }
}

fn bool_expr(b: bool) -> Expr {
   if b { Expr::True } else { Expr::False }
}

fn expr_num(expr: &Expr) -> Result<i32> {
   match *expr {
      Expr::Number(n) => Ok(n),
      _ => Err("Expected number".into())
   }
}

macro_rules! prim_arith {
   ($name:ident, $op:expr) =>
   (fn $name(operands: &Vec<Expr>) -> Result<Expr> {
      if operands.len() < 2 {
         return Err("Not enough operands".into());
      }
      let nums = operands.iter()
                         .map(expr_num)
                         .collect::<Result<Vec<i32>>>()?;
      let first = nums[0];
      let result = nums.iter().skip(1).fold(first, $op);
      Ok(Expr::Number(result))
   });
}
prim_arith!(prim_add, |a,b| a+b);
prim_arith!(prim_sub, |a,b| a-b);
prim_arith!(prim_mul, |a,b| a*b);
prim_arith!(prim_div, |a,b| a/b);

macro_rules! prim_compare {
   ($name:ident, $op:expr) =>
   (fn $name(operands: &Vec<Expr>) -> Result<Expr> {
      if operands.len() != 2 {
         return Err("Wrong number of operands for comparison".into());
      }
      let arg1 = expr_num(&operands[0])?;
      let arg2 = expr_num(&operands[1])?;
      Ok(bool_expr($op(arg1, arg2)))
   });
}
prim_compare!(prim_lt, |a,b| a<b);
prim_compare!(prim_gt, |a,b| a>b);
prim_compare!(prim_le, |a,b| a<=b);
prim_compare!(prim_ge, |a,b| a>=b);

fn prim_prn(operands: &Vec<Expr>) -> Result<Expr> {
   println!("{}", operands.iter().join(" "));
   Ok(Expr::Nil)
}

fn prim_list(operands: &Vec<Expr>) -> Result<Expr> {
   Ok(Expr::List(operands.clone()))
}

fn prim_listp(operands: &Vec<Expr>) -> Result<Expr> {
   if operands.len() != 1 {
     return Err("Wrong arity for list?".into());
   }
   match operands[0] {
      Expr::List(_) => Ok(Expr::True),
      _ => Ok(Expr::False)
   }
}

fn prim_emptyp(operands: &Vec<Expr>) -> Result<Expr> {
   if operands.len() != 1 {
     return Err("Wrong arity for empty?".into());
   }
   match operands[0] {
      Expr::List(ref l) | Expr::Vector(ref l) => 
         Ok(bool_expr(l.is_empty())),
      _ => Err("Wrong type for empty?".into())
   }
}

fn prim_count(operands: &Vec<Expr>) -> Result<Expr> {
   if operands.len() != 1 {
     return Err("Wrong arity for count".into());
   }
   match operands[0] {
      Expr::Nil => Ok(Expr::Number(0)),
      Expr::List(ref l) | Expr::Vector(ref l) => 
         Ok(Expr::Number(l.len() as i32)),
      _ => Err("Wrong type for count".into())
   }
}

fn prim_eq(operands: &Vec<Expr>) -> Result<Expr> {
   if operands.len() != 2 {
     Err("Wrong arity for count".into())
   } else {
     Ok(bool_expr(operands[0] == operands[1]))
   }
}

