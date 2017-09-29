use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::io::Read;
use std::ops::Deref;
use std::rc::Rc;
use std::path::Path;

use itertools::Itertools;

use eval;
use Expr;
use env::EnvRef;
use errors::*;
use pr_str;

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
 		  result.insert("pr-str", PrimFn{func: prim_pr_str});
 		  result.insert("str", PrimFn{func: prim_str});
 		  result.insert("prn", PrimFn{func: prim_prn});
 		  result.insert("println", PrimFn{func: prim_println});
 		  result.insert("list", PrimFn{func: prim_list});
 		  result.insert("list?", PrimFn{func: prim_listp});
 		  result.insert("empty?", PrimFn{func: prim_emptyp});
 		  result.insert("count", PrimFn{func: prim_count});
 		  result.insert("read-string", PrimFn{func: prim_read_string});
 		  result.insert("slurp", PrimFn{func: prim_slurp});
 		  result.insert("atom", PrimFn{func: prim_atom});
 		  result.insert("atom?", PrimFn{func: prim_atomp});
 		  result.insert("deref", PrimFn{func: prim_deref});
 		  result.insert("reset!", PrimFn{func: prim_reset});
 		  result.insert("swap!", PrimFn{func: prim_swap});
 		  result.insert("cons", PrimFn{func: prim_cons});
 		  result.insert("concat", PrimFn{func: prim_concat});
        result
    };
}

pub struct PrimFn {
    pub func: fn(&[Expr], &EnvRef) -> Result<Expr>
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
        (fn $name(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
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
        (fn $name(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
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

macro_rules! prim_print {
    ($name:ident, $readably:expr, $show:expr, $join:expr) =>
        (fn $name(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
            let result = operands.iter().map(|e| pr_str(e, $readably)).join($join);
            if $show {
                println!("{}", result);
                Ok(Expr::Nil)
            } else {
                Ok(Expr::String(Rc::new(result)))
            }
        });
}
prim_print!(prim_pr_str, true, false, " ");
prim_print!(prim_str, false, false, "");
prim_print!(prim_prn, true, true, " ");
prim_print!(prim_println, false, true, " ");

fn prim_list(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    Ok(Expr::List(operands.to_vec()))
}

fn prim_listp(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 1 {
        return Err("Wrong arity for list?".into());
    }
    match operands[0] {
        Expr::List(_) => Ok(Expr::True),
        _ => Ok(Expr::False)
    }
}

fn prim_emptyp(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 1 {
        return Err("Wrong arity for empty?".into());
    }
    match operands[0] {
        Expr::List(ref l) | Expr::Vector(ref l) =>
            Ok(bool_expr(l.is_empty())),
        _ => Err("Wrong type for empty?".into())
    }
}

fn prim_count(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
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

fn prim_eq(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 2 {
        Err("Wrong arity for count".into())
    } else {
        Ok(bool_expr(operands[0] == operands[1]))
    }
}

fn prim_read_string(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 1 {
        Err("Wrong arity for read_string".into())
    } else if let Expr::String(ref s) = operands[0] {
        ::reader::read_str(s)
    } else {
        Err("Invalid argument for read_string".into())
    }
}

fn prim_slurp(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 1 {
        Err("Wrong arity for slurp".into())
    } else if let Expr::String(ref s) = operands[0] {
        let mut file = File::open(Path::new(s.deref()))?;
        let mut result = String::new();
        file.read_to_string(&mut result)?;
        Ok(Expr::String(Rc::new(result)))
    } else {
        Err("Invalid argument for read_string".into())
    }
}

fn prim_atom(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 1 {
        Err("Wrong arity for atom".into())
    } else {
        Ok(Expr::Atom(Rc::new(RefCell::new(operands[0].clone()))))
    }
}

fn prim_atomp(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 1 {
        Err("Wrong arity for atom?".into())
    } else {
        match operands[0] {
            Expr::Atom(_) => Ok(Expr::True),
            _ => Ok(Expr::False)
        }
    }
}

fn prim_deref(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 1 {
        Err("Wrong arity for deref".into())
    } else {
        match operands[0] {
            Expr::Atom(ref a) => {
                Ok(a.borrow().clone())
            },
            _ => Err("Invalid argument for deref".into())
        }
    }
}

fn prim_reset(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 2 {
        Err("Wrong arity for reset!".into())
    } else {
        match operands[0] {
            Expr::Atom(ref a) => {
                *a.borrow_mut() = operands[1].clone();
                Ok(operands[1].clone())
            },
            _ => Err("Invalid argument for reset!".into())
        }
    }
}

fn prim_swap(operands: &[Expr], env: &EnvRef) -> Result<Expr> {
    if operands.len() < 2 {
        return Err("Not enough arguments for swap!".into())
    }
    match operands[0] {
        Expr::Atom(ref a) => {
            let old_val = a.borrow().clone();
            let mut expr = vec![operands[1].clone()];
            expr.push(old_val);
            expr.extend(operands[2..].iter().cloned());
            let new_val = eval(&Expr::List(expr), env)?;
            *a.borrow_mut() = new_val.clone();
            Ok(new_val)
        },
        _ => Err("First argument to swap must be atom".into())
    }
}

fn prim_cons(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    if operands.len() != 2 {
        return Err("Wrong arity for cons".into());
    }
    match operands[1] {
        Expr::List(ref l) | Expr::Vector(ref l) => {
            let mut new_list = vec![operands[0].clone()];
            new_list.extend(l.iter().cloned());
            Ok(Expr::List(new_list))
        },
        _ => Err("Second argument to cons must be list or vector".into())
    }
}

fn prim_concat(operands: &[Expr], _: &EnvRef) -> Result<Expr> {
    let mut new_list = vec![];
    for operand in operands {
        new_list.extend(match *operand {
            Expr::List(ref l) | Expr::Vector(ref l) => l.iter().cloned(),
            _ => return Err("Second argument to concat must be list or vector".into())
        })
    }
    Ok(Expr::List(new_list))
}
