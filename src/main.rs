#[macro_use]
extern crate error_chain;
extern crate itertools;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;

use std::fmt::{Debug, Formatter};
use std::io;
use std::io::BufRead;
use std::io::Write;
use std::ops::Deref;
use std::rc::Rc;

mod core;
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

use core::*;
use env::*;
use errors::*;

#[derive(Clone,Debug,PartialEq)]
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
    Func(Rc<Closure>),
    PrimFunc(PrimFn),
    Special(SpecialForm)
}

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum SpecialForm {
    Def,
    Do,
    Fn,
    If,
    LetStar
}

#[derive(Clone)]
pub struct Closure {
    params: Vec<Rc<String>>,
    body: Expr,
    env: EnvRef,
}

impl Closure {
    fn new(params: &Expr, body: &Expr, env: &EnvRef) -> Result<Self> {
        let bind_list: &[Expr] = match params {
            &Expr::List(ref l) | &Expr::Vector(ref l) => l,
            _ => return Err("Function parameters must be list or vector".into())
        };
        let bind_strs = bind_list.iter()
            .map(|expr| match expr {
                &Expr::Symbol(ref s) => Ok(s.clone()),
                _ => return Err("Invalid function parameter".into())
            }).collect::<Result<Vec<Rc<String>>>>()?;
        Ok(Closure {
            params: bind_strs,
            body: body.clone(),
            env: env.clone()
        })
    }
}
impl Debug for Closure {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "<function: params={:?} body={:?}>", self.params, self.body)
    }
}
impl PartialEq for Closure {
    fn eq(&self, _: &Closure) -> bool {
        false
    }
}

fn prompt() {
    print!("user> ");
    io::stdout().flush().unwrap();
}

fn read(input: &str) -> Result<Expr> {
    reader::read_str(input)
}

fn eval(expr: &Expr, env: &EnvRef) -> Result<Expr> {
    match expr {
        &Expr::Symbol(ref s) => {
            match env.borrow().get(s.deref()) {
                Some(f) => Ok(f.clone()),
                None => Err("Unknown symbol".into())
            }
        },
        &Expr::List(ref l) if l.len() > 0 => {
            if let Expr::Special(_) = l[0] {
                eval_special(l, env)
            } else {
                apply(l, env)
            }
        },
        &Expr::Vector(ref v) => {
            let evaluated = v.iter()
                .map(|expr| eval(expr, env))
                .collect::<Result<Vec<Expr>>>()?;
            Ok(Expr::Vector(evaluated))
        }
        _ => Ok(expr.clone())
    }
}



fn apply(list: &[Expr], env: &EnvRef) -> Result<Expr> {
    let op = eval(&list[0], env)?;
    let operands = list.iter()
        .skip(1)
        .map(|expr| eval(expr, env))
        .collect::<Result<Vec<Expr>>>()?;
    match op {
        Expr::PrimFunc(pf) => {
            (pf.func)(&operands)
        },
        Expr::Func(ref closure) => {

            let bindings: Vec<_> = closure.params.iter().cloned()
                .zip(operands.iter().cloned()).collect();
		    let new_env = Env::extend(&closure.env, bindings);
            eval(&closure.body, &new_env)
        }
        _ => Err("Non-function cannot be applied".into())
    }
}

fn get_binding(expr: &Expr) -> Result<Rc<String>> {
    match *expr {
        Expr::Symbol(ref s) => Ok(s.clone()),
        _ => Err("Cannot bind to non-symbol".into())
    }
}

fn eval_special(list: &[Expr], env: &EnvRef) -> Result<Expr>{
    let s = match list[0] {
        Expr::Special(s) => s,
        _ => unreachable!()
    };
    match s {
	    SpecialForm::Def => {
            let (key, val) = if list.len() == 3 {
                (get_binding(&list[1])?, eval(&list[2], env)?)
            } else {
                return Err("Wrong number of arguments in definition".into());
            };
            env.borrow_mut().set(key, val.clone());
            Ok(val)
        },
        SpecialForm::Do => {
            let mut evaluated = list.iter().skip(1)
                .map(|expr| eval(expr, env))
                .collect::<Result<Vec<Expr>>>()?;
            Ok(evaluated.pop().unwrap_or(Expr::Nil))
        },
        SpecialForm::Fn => {
            if list.len() != 3 {
                return Err("Wrong number of arguments for fn*".into());
            }
            Ok(Expr::Func(Rc::new(Closure::new(&list[1], &list[2], env)?)))
        },
        SpecialForm::If => {
            if list.len() != 3 && list.len() != 4 {
                return Err("Wrong number of arguments for if".into());
            }
            let test = eval(&list[1], env)?;
            match test {
                Expr::Nil | Expr::False =>
                    if list.len() == 4 {
                        eval(&list[3], env)
                    } else {
                        Ok(Expr::Nil)
                    },
                _ =>
                    eval(&list[2], env)
            }
        },
        SpecialForm::LetStar => {
            let (bindings, body): (&[Expr], &Expr) = if list.len() == 3 {
                (match list[1] {
                    Expr::List(ref l) | Expr::Vector(ref l) => l,
                    _ => return Err("First argument to let* must be list of bindings".into())
                }, &list[2])
            } else {
                return Err("Wrong number of arguments for let*".into());
            };
            if bindings.len() % 2 != 0 {
                return Err("Invalid bindings in let*".into());
            }
			let new_env = Env::extend(env, vec![]);
			if bindings.len() > 0 {
                for binding in bindings.chunks(2) {
                    let (key, expr) = (&binding[0], &binding[1]);
                    let val = eval(expr, &new_env)?;
                    new_env.borrow_mut().set(get_binding(key)?, val.clone());
                }
            }
            eval(body, &new_env)
        },
    }
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
    let env: EnvRef = Env::new();
    for (key, val) in GLOBAL_NAMESPACE.iter() {
        env.borrow_mut().set(Rc::new(String::from(*key)),
                             Expr::PrimFunc(val.clone()));
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
        let val = match eval(&expr, &env) {
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
