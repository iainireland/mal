use std::fmt::*;

use itertools::Itertools;

use Expr;

impl<'a> Display for Expr<'a> {
   fn fmt(&self, f: &mut Formatter) -> Result {
      match *self {
      Expr::Symbol(ref s) => write!(f, "{}", s),
		Expr::Number(n) => write!(f, "{}", n),
      Expr::List(ref l)   => {
		   write!(f, "(")?;
         write!(f, "{}", l.iter().join(" "))?;
			write!(f, ")")
         }
      }
   }
}