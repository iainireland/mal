use std;

use errors::*;
use nom::*;

use Expr;

pub fn read_str(input: &str) -> Result<Expr> {
	 match get_expr(input.as_bytes()) {
    IResult::Done(bytes, expr) => {
	    if bytes.is_empty() { 
          Ok(expr) 
       } else { 
          bail!("incomplete expression");
       }
    },
    _ => bail!("failed parsing")
    }
}

named!(numeric_string<&str>,
   map_res!(
      digit,
      std::str::from_utf8
   )
);

named!(get_number<Expr>,
   map!(
      map_res!(
   	   numeric_string,
         std::str::FromStr::from_str
      ),
      |n| Expr::Number(n)
   )
);

named!(get_symbol<Expr>,
   map!(
      map_res!(
         is_not_s!(" \t[]{}()'\"`,;"),
         std::str::from_utf8
      ),
      |s| Expr::Symbol(s)
   )
);

named!(get_list<Expr>,
   do_parse!(
      tag!("(") >>
      res: many_till!(call!(get_expr), tag!(")")) >>
      (Expr::List(res.0))
   )
);

named!(get_expr<Expr>,
   ws!(
      alt!(get_list | get_number | get_symbol)
   )
);