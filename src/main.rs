#[macro_use]
extern crate error_chain;
extern crate itertools;
#[macro_use]
extern crate nom;

use std::io;
use std::io::BufRead;
use std::io::Write;

mod reader;
mod printer;
mod errors {
   error_chain! { 
      foreign_links {
         Io(::std::io::Error) #[cfg(unix)];
      }
   }
}

use errors::*;

#[derive(Debug)]
pub enum Expr<'a>{
  Symbol(&'a str),
  Keyword(&'a str),
  Number(i32),
  String(String),
  Nil,
  True,
  False,
  List(Vec<Expr<'a>>),
  Vector(Vec<Expr<'a>>)
}

type Value = String;

fn prompt() {
  print!("user> ");
  io::stdout().flush().unwrap();
}

fn read(input: &str) -> Result<Expr> {
 	reader::read_str(input)
}

fn eval(expr: &Expr) -> Value {
   println!("{:?}", expr);
	String::from(expr.to_string())
}

fn print(val: &Value) -> String {
	val.to_string()
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
	  let val = eval(&expr);
	  let output = print(&val);
	  println!("{}", output);
     prompt();
   }	
  println!();
  Ok(())
}
