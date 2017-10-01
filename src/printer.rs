use std::fmt::*;
use std::ops::Deref;

use itertools::Itertools;

use Expr;
use SpecialForm;
use MalHashType;

macro_rules! display_expr {
    ($string_format:expr, $recurse:expr) =>
        (fn fmt(&self, f: &mut Formatter) -> Result {
            match *self.deref() {
                Expr::Symbol(ref s) => write!(f, "{}", s),
		          Expr::Number(n) => write!(f, "{}", n),
		          Expr::String(ref s) => write!(f, $string_format, s),
		          Expr::Keyword(ref s) => write!(f, ":{}", s),
		          Expr::Nil => write!(f, "nil"),
		          Expr::True => write!(f, "true"),
		          Expr::False => write!(f, "false"),
                Expr::Func(_) => write!(f, "#<function>"),
                Expr::PrimFunc(_) => write!(f, "#<primitive function>"),
                Expr::Atom(ref a) => write!(f, "(atom {})", $recurse(&*a.borrow())),
                Expr::List(ref l) => {
		            write!(f, "(")?;
                    write!(f, "{}", l.iter().map($recurse).join(" "))?;
			        write!(f, ")")
                },
                Expr::Vector(ref l) => {
		            write!(f, "[")?;
                    write!(f, "{}", l.iter().map($recurse).join(" "))?;
			        write!(f, "]")
                },
                Expr::Hash(ref h) => {
                    write!(f, "{{")?;
                    write!(f, "{}", h.iter().map(|(key,value)| {
                        match key {
                            &(ref s, MalHashType::String) =>
                                format!(concat!($string_format, " {}"), s, $recurse(value)),
                            &(ref s, MalHashType::Keyword) =>
                                format!(":{} {}", s, $recurse(value))
                        }
                    }).join(" "))?;
                    write!(f, "}}")
                }
                Expr::Special(s) => {
                    write!(f, "{}", match s {
                        SpecialForm::Def => "def!",
                        SpecialForm::DefMacro => "defmacro!",
                        SpecialForm::Do => "do",
                        SpecialForm::Eval => "eval",
                        SpecialForm::Fn => "fn*",
                        SpecialForm::If => "if",
                        SpecialForm::LetStar => "let*",
                        SpecialForm::MacroExpand => "macroexpand",
                        SpecialForm::Quote => "quote",
                        SpecialForm::Quasiquote => "quasiquote"
                    })
                }
            }
        }
    )
}

impl Display for Expr {
    display_expr!("{}", |expr| expr);
}

pub struct ReadableExpr<'a> {
    expr: &'a Expr
}

impl<'a> Deref for ReadableExpr<'a> {
    type Target = Expr;
    fn deref(&self) -> &Expr {
        &self.expr
    }
}

impl<'a> From<&'a Expr> for ReadableExpr<'a> {
    fn from(expr: &'a Expr) -> Self {
        ReadableExpr { expr }
    }
}

impl<'a> Display for ReadableExpr<'a> {
    display_expr!("{:?}", |expr| ReadableExpr { expr: expr });
}
