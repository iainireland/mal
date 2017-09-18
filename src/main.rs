#[macro_use]
extern crate error_chain;
extern crate itertools;
#[macro_use]
extern crate nom;

use std::fmt::{Debug, Formatter};
use std::io;
use std::io::BufRead;
use std::io::Write;
use std::ops::Deref;
use std::rc::Rc;

mod env;
mod reader;
mod printer;
mod errors {
   error_chain! { 
      foreign_links {
         Io(::std::io::Error) #[cfg(unix)];
      }
   }
}

use env::*;
use errors::*;

#[derive(Clone,Debug)]
pub enum Expr{
  Nil,
  True,
  False,
  Symbol(Rc<String>),
  Keyword(Rc<String>),
  Number(i32),
  String(String),
  List(Vec<Expr>),
  Vector(Vec<Expr>),
  PrimFunc(PrimFn),
  Special(SpecialForm)
}

#[derive(Clone,Copy,Debug)]
pub enum SpecialForm {
  Def,
  LetStar
}

#[derive(Clone)]
pub struct PrimFn {
   func: Rc<Box<Fn(i32, i32) -> i32>>
}
impl Debug for PrimFn {
   fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
      write!(f, "<primitive function>")
   }
}

fn prompt() {
  print!("user> ");
  io::stdout().flush().unwrap();
}

fn read(input: &str) -> Result<Expr> {
   reader::read_str(input)
}

fn eval(expr: Expr, env: &mut Env) -> Result<Expr> {
   match expr {
   Expr::Symbol(s) => {
      match env.get(&*s) {
      Some(f) => Ok(f.clone()),
      None => Err("Unknown symbol".into())
      }
   },
   Expr::List(ref l) if l.len() > 0 => {
      if let Expr::Special(_) = l[0] {
         eval_special(l, env)
      } else {
         eval_list(l, env)
      }
   },
   Expr::Vector(v) => {
      let evaluated = v.iter()
                       .map(|expr| eval(expr.clone(), env))
                       .collect::<Result<Vec<Expr>>>()?;
      Ok(Expr::Vector(evaluated))
   }
   _ => Ok(expr)
   }
}



fn eval_list(list: &Vec<Expr>, env: &mut Env) -> Result<Expr> {
   let op = eval(list[0].clone(), env)?;
   let operands = list.iter()
                      .skip(1)
                      .map(|expr| eval(expr.clone(), env))
                      .collect::<Result<Vec<Expr>>>()?;
   match op {
      Expr::PrimFunc(pf) => {
         if operands.len() < 2 {
            return Err("Not enough operands".into());
         }
         let nums = operands.iter().map(|x| match *x {
               Expr::Number(n) => Ok(n),
               _ => Err("Non-number in arithmetic".into())
            }).collect::<Result<Vec<i32>>>()?;
         let first = nums[0];
         let result = nums.iter().skip(1).fold(first, |a,&b| (pf.func)(a,b));
         Ok(Expr::Number(result))
      },
      _ => Err("Non-function cannot be applied".into())
   }
}

fn eval_special(list: &Vec<Expr>, env: &mut Env) -> Result<Expr>{
   let s = match list[0] {
     Expr::Special(s) => s,
     _ => unreachable!()
   };
   match s {
	   SpecialForm::Def => {
         let (key, val): (String, Expr) = if list.len() == 3 {
            let k = match list[1] {
               Expr::Symbol(ref s) => s.deref().clone(),
               _ => return Err("First argument to def! must be a symbol".into())
            };
            let v = eval(list[2].clone(), env)?;
            (k, v)
         } else {
            return Err("Wrong number of arguments in definition".into());
         };
         env.set(key, val.clone());
         return Ok(val);
      },
      SpecialForm::LetStar => {
         unimplemented!("let");
      },
   };
}

fn print(val: &Expr) -> String {
   String::from(val.to_string())
}

fn main() {
   if let Err(ref e) = run() {
      println!("Error: {}", e);
     
      for e in e.iter().skip(1) {
         println!("caused by: {}", e);
      }
      ::std::process::exit(1);
   }
}

fn run() -> Result<()> {
   let mut env: Env = Env::new();

   {
      let mut add_prim_to_env = |key, value| {
         env.set(String::from(key),
                    Expr::PrimFunc(PrimFn { func: Rc::new(value) }));
      };
      add_prim_to_env("+", Box::new(|a,b| a+b));
      add_prim_to_env("-", Box::new(|a,b| a-b));
      add_prim_to_env("*", Box::new(|a,b| a*b));
      add_prim_to_env("/", Box::new(|a,b| a/b));
   }

   let stdin = io::stdin();
   prompt();
   for line in stdin.lock().lines() {
     let line = line?;
     let expr = match read(&line) {
       Ok(e) => e,
       Err(e) => {
          println!("Error: {}", e);
          prompt();
          continue;
       }
     };
     let val = match eval(expr, &mut env) {
        Ok(v) => v,
        Err(e) => {
          println!("Error: {}", e);
          prompt();
          continue;
       }
     };
     let output = print(&val);
     println!("{}", output);
     prompt();
   }  
  println!();
  Ok(())
}
