use std;
use std::rc::Rc;

use errors::*;
use nom::*;

use Expr;
use SpecialForm;

pub fn read_str(input: &str) -> Result<Expr> {
	match get_expr(input.as_bytes()) {
        IResult::Done(bytes, expr) => {
	        if bytes.is_empty() {
                Ok(expr)
            } else {
                bail!("incomplete expression");
            }
        },
        IResult::Incomplete(needed) => {
            println!("Needed: {:?}", needed);
            bail!("failed parsing");
        },
        IResult::Error(e) => {
            println!("Error: {:?}", e);
            bail!("failed parsing");
        },
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
           tuple!(
               opt!(tag!("-")),
               map_res!(
     	           numeric_string,
                   std::str::FromStr::from_str
               )
           ),
           |(sign,n)| match sign {
               None => Expr::Number(n),
               Some(_) => Expr::Number(-n)
           }
       )
);

named!(get_symbol<Expr>,
       map!(
           map_res!(
               is_not_s!(" \t[]{}()'\"`,;"),
               std::str::from_utf8
           ),
           |s| Expr::Symbol(Rc::new(String::from(s)))
       )
);

named!(get_keyword<Expr>,
       preceded!(
           tag!(":"),
           map!(
               map_res!(
                   is_not_s!(" \t[]{}()'\"`,;"),
                   std::str::from_utf8
               ),
               |s| Expr::Keyword(Rc::new(String::from(s)))
           )
       )
);

named!(get_list<Expr>,
       delimited!(
           tag!("("),
           map!(many0!(get_expr), Expr::List),
           tag!(")")
       )
);

named!(get_vector<Expr>,
       delimited!(
           tag!("["),
           map!(many0!(get_expr), Expr::Vector),
           tag!("]")
       )
);

named!(get_special<Expr>,
       alt!(
           map!(tag!("nil"), |_| Expr::Nil) |
           map!(tag!("true"), |_| Expr::True) |
           map!(tag!("false"), |_| Expr::False) |
           map!(tag!("def!"), |_| Expr::Special(SpecialForm::Def)) |
           map!(tag!("do"), |_| Expr::Special(SpecialForm::Do)) |
           map!(tag!("eval"), |_| Expr::Special(SpecialForm::Eval)) |
           map!(tag!("fn*"), |_| Expr::Special(SpecialForm::Fn)) |
           map!(tag!("if"), |_| Expr::Special(SpecialForm::If)) |
           map!(tag!("let*"), |_| Expr::Special(SpecialForm::LetStar))
       )
);

fn get_string(input: &[u8]) -> IResult<&[u8], Expr> {
    let mut i = match tag!(input, "\"") {
        IResult::Done(rest, _) => rest,
        IResult::Incomplete(n) => return IResult::Incomplete(n),
		IResult::Error(e) => return IResult::Error(e)
    };
    let mut result = String::new();
    while let Some(idx) = i.iter().position(|c| *c == b'"' || *c == b'\\') {
        let chunk = String::from_utf8_lossy(&i[0..idx]);
        result.push_str(&chunk);
        if i[idx] == b'"' {
            return IResult::Done(&i[idx+1..], Expr::String(result));
        } else {
            match i.get(idx+1).map(|&c| c as char) {
                Some('n') => result.push_str("\n"),
                Some('"') => result.push_str("\""),
                Some('\\') => result.push_str("\\"),
				_ => return IResult::Error(::nom::ErrorKind::Escaped)
            }
            i = &i[idx+2..];
        }
    }
    IResult::Incomplete(Needed::Unknown)
}

named!(get_expr<Expr>,
       ws!(
           alt!(get_list |
                get_vector |
                get_number |
                get_special |
                get_string |
                get_keyword |
                get_symbol)
       )
);
