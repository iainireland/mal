use std::io;
use std::io::BufRead;
use std::io::Write;

type Expr = String;
type Value = String;

fn prompt() {
  print!("user> ");
  io::stdout().flush().unwrap();
}

fn read(input: &str) -> Expr {
	input.to_string()
}

fn eval(expr: &Expr) -> Value {
	expr.to_string()

}

fn print(val: &Value) -> String {
	val.to_string()
}

fn main() {
	let stdin = io::stdin();
   prompt();
	for line in stdin.lock().lines() {
	  let expr = read(&line.unwrap());
	  let val = eval(&expr);
	  let output = print(&val);
	  println!("{}", output);
     prompt();
   }	
  println!();
}
