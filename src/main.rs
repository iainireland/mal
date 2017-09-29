#[macro_use] extern crate error_chain;
extern crate itertools;
#[macro_use] extern crate lazy_static;
extern crate regex;

// TODO: reader macro @ in reader.rs
// TODO: atoms

use std::cell::RefCell;
use std::collections::HashMap;
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
    String(Rc<String>),
    List(Vec<Expr>),
    Vector(Vec<Expr>),
    Hash(MalHash),
    Atom(Rc<RefCell<Expr>>),
    Func(Rc<Closure>),
    PrimFunc(PrimFn),
    Special(SpecialForm)
}

impl Expr {
    fn symbol(s: &str) -> Self {
        Expr::Symbol(Rc::new(String::from(s)))
    }
}

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum SpecialForm {
    Def,
    Do,
    Eval,
    Fn,
    If,
    LetStar,
    Quote,
    Quasiquote
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

pub type MalHash = HashMap<(Rc<String>, MalHashType),Expr>;
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
pub enum MalHashType {
    String,
    Keyword
}

fn read(input: &str) -> Result<Expr> {
    reader::read_str(input)
}

fn eval(expr: &Expr, env: &EnvRef) -> Result<Expr> {
    let mut result: Option<Expr> = None;
    let mut curr_env: EnvRef = Rc::clone(&env);
    loop {
        result = Some(match *result.as_ref().unwrap_or(expr) {
            Expr::Symbol(ref s) => {
                return match curr_env.borrow().get(s.deref()) {
                    Some(f) => Ok(f.clone()),
                    None => Err(format!("Unknown symbol: {}", s).into())
                };
            },
            Expr::List(ref list) if !list.is_empty() => {
                if let Expr::Special(s) = list[0] {
                    match s {
	                    SpecialForm::Def => {
                            let (key, val) = if list.len() == 3 {
                                (get_binding(&list[1])?, eval(&list[2], &curr_env)?)
                            } else {
                                return Err("Wrong number of arguments in definition".into());
                            };
                            curr_env.borrow_mut().set(key, val.clone());
                            return Ok(val)
                        },
                        SpecialForm::Do => {
                            // Evaluate all but the last element:
                            list[0..list.len()-1].iter()
                                .skip(1)
                                .map(|expr| eval(expr, &curr_env))
                                .collect::<Result<Vec<Expr>>>()?;
                            list[1..].last().map_or(Expr::Nil, |e| e.clone()) // tco
                        },
                        SpecialForm::Eval => {
                            if list.len() != 2 {
                                return Err("Wrong number of arguments to eval".into());
                            }
                            let ast = eval(&list[1], &curr_env)?;
                            curr_env = Env::root(&curr_env);
                            ast // tco
                        },
                        SpecialForm::Fn => {
                            if list.len() != 3 {
                                return Err("Wrong number of arguments for fn*".into());
                            }
                            Expr::Func(Rc::new(Closure::new(&list[1], &list[2], &curr_env)?)) // tco
                        },
                        SpecialForm::If => {
                            if list.len() != 3 && list.len() != 4 {
                                return Err("Wrong number of arguments for if".into());
                            }
                            let test = eval(&list[1], &curr_env)?;
                            match test {
                                Expr::Nil | Expr::False =>
                                    if list.len() == 4 {
                                        list[3].clone() // tco
                                    } else {
                                        Expr::Nil
                                    },
                                _ =>
                                    list[2].clone() // tco
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
			                curr_env = Env::extend(&curr_env, vec![]);
			                if !bindings.is_empty() {
                                for binding in bindings.chunks(2) {
                                    let (key, expr) = (&binding[0], &binding[1]);
                                    let val = eval(expr, &curr_env)?;
                                    curr_env.borrow_mut().set(get_binding(key)?, val.clone());
                                }
                            }
                            body.clone() // tco
                        },
                        SpecialForm::Quote => {
                            if list.len() != 2 {
                                return Err("Invalid quote".into());
                            } 
                            return Ok(list[1].clone());
                        },
                        SpecialForm::Quasiquote => {
                            if list.len() != 2 {
                                return Err("Invalid quasiquote".into());
                            } 
                            quasiquote(&list[1])? // tco
                        }
                    }
                } else {
                    let op = eval(&list[0], &curr_env)?;
                    let mut operands = list.iter()
                        .skip(1)
                        .map(|expr| eval(expr, &curr_env))
                        .collect::<Result<Vec<Expr>>>()?;
                    match op {
                        Expr::PrimFunc(pf) => {
                            return (pf.func)(&operands, &curr_env)
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
		                      curr_env = Env::extend(&closure.env, bindings);
                            closure.body.clone() // tco
                        }
                        _ => return Err("Non-function cannot be applied".into())
                    }
                }
            },
            Expr::Vector(ref v) => {
                let evaluated = v.iter()
                    .map(|expr| eval(expr, &curr_env))
                    .collect::<Result<Vec<Expr>>>()?;
                Expr::Vector(evaluated)
            },
            Expr::Hash(ref h) => {
                let mut evaluated = h.clone();
                for expr in evaluated.values_mut() {
                    *expr = eval(expr, &curr_env)?;
                }
                Expr::Hash(evaluated)
            },
            _ => return Ok(expr.clone())
        });
        match result {
            Some(Expr::List(_)) | Some(Expr::Symbol(_)) => continue,
            _ => break
        }
    }
    Ok(result.unwrap())
}

fn get_binding(expr: &Expr) -> Result<Rc<String>> {
    match *expr {
        Expr::Symbol(ref s) => Ok(Rc::clone(s)),
        _ => Err("Cannot bind to non-symbol".into())
    }
}

fn quasiquote(ast: &Expr) -> Result<Expr> {
    let list = match *ast {
        Expr::List(ref l) | Expr::Vector(ref l) if !l.is_empty() => l,
        _ => return Ok(Expr::List(vec![Expr::Special(SpecialForm::Quote), ast.clone()]))
    };
    let (head,tail) = list.split_first().unwrap(); // already checked for empty
    match *head {
        Expr::Symbol(ref s) if s.deref() == "unquote" => {
            if tail.len() == 1 {
                return Ok(tail[0].clone());
            } else {
                return Err("Invalid unquote".into());
            }
        },
        Expr::List(ref l) | Expr::Vector(ref l) if !l.is_empty() => {
            match l[0] {
                Expr::Symbol(ref s) if s.deref() == "splice-unquote" => {
                    if l.len() == 2 {
                        let spliced = vec![Expr::symbol("concat"),
                                           l[1].clone(),
                                           quasiquote(&Expr::List(tail.to_vec()))?];
                        return Ok(Expr::List(spliced))
                    } else {
                        return Err("Invalid splice-unquote".into());
                    }
                },
                _ => {}
            }
        }
        _ => {}
    };
    Ok(Expr::List(vec![Expr::symbol("cons"),
                       quasiquote(head)?,
                       quasiquote(&Expr::List(tail.to_vec()))?]))
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

    rep("(def! not (fn* [a] (if a false true)))", &env)?;
    rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))", &env)?;

    let mut args = std::env::args();
    if args.len() > 1 {
        args.next();
        let file: String = args.next().unwrap();
        let arg_exprs: Vec<Expr> = args.map(|s| Expr::String(Rc::new(s))).collect();

        println!("file {}", file);

        env.borrow_mut().set(Rc::new(String::from("*ARGV*")),
                             Expr::List(arg_exprs));
        rep(&format!("(load-file \"{}\")", &file), &env)?;
        return Ok(());
    } else {
        env.borrow_mut().set(Rc::new(String::from("*ARGV*")),
                             Expr::List(vec![]));
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
