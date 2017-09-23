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
use printer::ReadableExpr;

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
    is_variadic: bool
}

impl Closure {
    fn new(params: &Expr, body: &Expr, env: &EnvRef) -> Result<Self> {
        let bind_list: &[Expr] = match *params {
            Expr::List(ref l) | Expr::Vector(ref l) => l,
            _ => return Err("Function parameters must be list or vector".into())
        };
        let mut is_variadic = false;
        let bind_strs = bind_list.iter().enumerate()
            .filter_map(|(idx, expr)| match *expr {
                Expr::Symbol(ref s) => {
                    if **s != "&" {
                        Some(Ok(Rc::clone(s)))
                    } else if idx == bind_list.len() - 2 {
                        is_variadic = true;
                        None
                    } else {
                        Some(Err("Invalid variadic binding".into()))
                    }
                },
                _ => Some(Err("Invalid function parameter".into()))
            })
            .collect::<Result<Vec<Rc<String>>>>()?;
        Ok(Closure {
            params: bind_strs,
            body: body.clone(),
            env: Rc::clone(env),
            is_variadic: is_variadic,
        })
    }
}
impl Debug for Closure {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "<function: params={:?} body={:?}> variadic={:?}", self.params, self.body, self.is_variadic)
    }
}
impl PartialEq for Closure {
    fn eq(&self, _: &Closure) -> bool {
        false
    }
}

fn read(input: &str) -> Result<Expr> {
    reader::read_str(input)
}

fn eval(expr: &Expr, env: &EnvRef) -> Result<Expr> {
    match *expr {
        Expr::Symbol(ref s) => {
            match env.borrow().get(s.deref()) {
                Some(f) => Ok(f.clone()),
                None => Err("Unknown symbol".into())
            }
        },
        Expr::List(ref l) if !l.is_empty() => {
            if let Expr::Special(_) = l[0] {
                eval_special(l, env)
            } else {
                apply(l, env)
            }
        },
        Expr::Vector(ref v) => {
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
    let mut operands = list.iter()
        .skip(1)
        .map(|expr| eval(expr, env))
        .collect::<Result<Vec<Expr>>>()?;
    match op {
        Expr::PrimFunc(pf) => {
            (pf.func)(&operands)
        },
        Expr::Func(ref closure) => {
            if closure.is_variadic {
                if operands.len() < closure.params.len() - 1 {
                    return Err("Too few arguments for variadic function".into());
                }
                let rest = operands.split_off(closure.params.len() - 1);
                operands.push(Expr::List(rest));
            } else if operands.len() != closure.params.len() {
                return Err("Wrong number of arguments".into());
            }
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
        Expr::Symbol(ref s) => Ok(Rc::clone(s)),
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
			if !bindings.is_empty() {
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

fn pr_str(val: &Expr, print_readably: bool) -> String {
    if print_readably {
        let readable: ReadableExpr = val.into();
        String::from(readable.to_string())
    } else {
        String::from(val.to_string())
    }
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

fn rep(line: &str, env: &EnvRef) -> Result<String> {
    let expr = read(&line)?;
    let val = eval(&expr, &env)?;
    let output = pr_str(&val, true);
    Ok(output)
}

struct PromptIterator<T> { inner: T }
impl<T> PromptIterator<T> {
    fn new(inner: T) -> Self {
        PromptIterator { inner: inner }
    }
}
impl<T, A> Iterator for PromptIterator<T>  where
    T: Iterator<Item=A> {
    type Item = A;

    fn next(&mut self) -> Option<A> {
        print!("user> ");
        io::stdout().flush().unwrap();
        self.inner.next()
    }
}

fn run() -> Result<()> {
    let env: EnvRef = Env::new();
    for (key, val) in GLOBAL_NAMESPACE.iter() {
        env.borrow_mut().set(Rc::new(String::from(*key)),
                             Expr::PrimFunc(val.clone()));
    }

    let stdin = io::stdin();
    for line in PromptIterator::new(stdin.lock().lines()) {
        let line: String  = line?;
        let output = match rep(&line, &env) {
            Ok(e) => e,
            Err(e) => {
                println!("Error: {}", e);
                for e in e.iter().skip(1) {
                    println!("caused by: {}", e);
                }
                continue;
            }
        };
        println!("{}", output);
    }
    println!();
    Ok(())
}
