use std::fmt::*;

use itertools::Itertools;

use Expr;

impl<'a> Display for Expr<'a> {
   fn fmt(&self, f: &mut Formatter) -> Result {
      match *self {
      Expr::Symbol(ref s) => write!(f, "{}", s),
		Expr::Number(n) => write!(f, "{}", n),
		Expr::String(ref s) => write!(f, "{:?}", s),
		Expr::Keyword(ref s) => write!(f, ":{}", s),
		Expr::Nil => write!(f, "nil"),
		Expr::True => write!(f, "true"),
		Expr::False => write!(f, "false"),
      Expr::PrimFunc(_) => write!(f, "<primitive function>"),
      Expr::List(ref l)   => {
		   write!(f, "(")?;
         write!(f, "{}", l.iter().join(" "))?;
			write!(f, ")")
         }
      Expr::Vector(ref l)   => {
		   write!(f, "[")?;
         write!(f, "{}", l.iter().join(" "))?;
			write!(f, "]")
         }
      }
   }
}