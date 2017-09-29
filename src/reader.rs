use std::collections::HashMap;
use std::iter::Peekable;
use std::rc::Rc;

use regex::Regex;

use errors::*;


use Expr;
use MalHashType;
use SpecialForm;

struct TokenIterator<'a> {
    data: &'a str,
    index: usize,
    regex: Regex,
}

impl<'a> TokenIterator<'a> {
    fn new(input: &'a str) -> Self {
        let token_re = Regex::new(r#"(?x)
            ^
            [\s,]*                 # whitespace (includes commas)
            (                      # start capture group
            ~@                   | # splice-unquote
            [\[\]{}()'`~^@]      | # special chars
            "(?:\\.|[^\\"])*"    | # string literal
            ;.*                  | # comment
            [^\s\[\]{}()'\~^@;,]+  # symbol        
            )"#).unwrap();
        TokenIterator {
            data: input,
            index: 0,
            regex: token_re
        }
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        let re = self.regex.captures(&self.data[self.index..]);
        re.and_then(|capture| capture.get(1))
            .map(|m| {
                self.index += m.end();
                m.as_str()
            })
            // Filter out comments
            .and_then(|tok| if tok.starts_with(';') {
                self.next()
            } else {
                Some(tok)
            })
    }
}

struct Tokenizer<'a> {
    tokens: Peekable<TokenIterator<'a>>
}

impl<'a> Tokenizer<'a> {
    fn new(input: &'a str) -> Self {
        Tokenizer {
            tokens: TokenIterator::new(input).peekable()
        }
    }
    fn read_form(&mut self) -> Result<Expr> {
        let peeked = *self.tokens.peek()
            .ok_or("Unexpected end of expression")?;
        if peeked == "(" || peeked == "[" || peeked == "{" {
            self.read_list()
        } else {
            self.read_atom()
        }
    }
    fn read_list(&mut self) -> Result<Expr> {
        let delim = self.tokens.next().unwrap(); // already peeked
        let close_delim = match delim {
            "(" => ")",
            "[" => "]",
            "{" => "}",
            _ => panic!("Bad delimiter")
        };
        let mut contents = vec![];
        while *self.tokens.peek().ok_or("Mismatched parentheses")? != close_delim {
            let expr = self.read_form()?;
            contents.push(expr);
        }
        self.tokens.next();
        match delim {
            "(" => Ok(Expr::List(contents)),
            "[" => Ok(Expr::Vector(contents)),
            "{" => {
                if contents.len() % 2 != 0 {
                    return Err("Invalid hash table".into());
                }
                let mut hash_map = HashMap::new();
                for chunk in contents.chunks(2) {
                    let (key_expr, val_expr) = (&chunk[0],&chunk[1]);
                    let key = match *key_expr {
                        Expr::String(ref s)  => (Rc::clone(s), MalHashType::String),
                        Expr::Keyword(ref s)  => (Rc::clone(s), MalHashType::Keyword),
                        _ => return Err("Invalid key in hashmap".into())
                    };
                    hash_map.insert(key, val_expr.clone());
                }
                Ok(Expr::Hash(hash_map))
            }
            _ => panic!("Bad delimiter")
        }
    }
    fn read_atom(&mut self) -> Result<Expr> {
        let tok = self.tokens.next().ok_or("Error parsing expression")?;
        if let Ok(n) = tok.parse::<i32>() {
            Ok(Expr::Number(n))
        } else if tok.starts_with(':') {
            Ok(Expr::Keyword(Rc::new(String::from(&tok[1..]))))
        } else if tok.starts_with('"') {
            let mut remaining = &tok[1..tok.len()-1];
            let mut result = String::with_capacity(remaining.len());
            while let Some(idx) = remaining.chars().position(|c| c == '\\') {
                let (chunk, leftovers) = remaining.split_at(idx);
                result.push_str(chunk);
                match leftovers.chars().nth(1) {
                    Some('n') => result.push_str("\n"),
                    Some('"') => result.push_str("\""),
                    Some('\'') => result.push_str("\'"),
                    Some('\\') => result.push_str("\\"),
                    _ => return Err("Bad escape sequence".into())
                }
                remaining = &leftovers[2..];
            }
            result.push_str(remaining);
            Ok(Expr::String(Rc::new(result)))
        } else { 
            match tok {
                "nil" => Ok(Expr::Nil),
                "true" => Ok(Expr::True),
                "false" => Ok(Expr::False),
                "def!" => Ok (Expr::Special(SpecialForm::Def)),
                "do" => Ok (Expr::Special(SpecialForm::Do)),
                "eval" => Ok (Expr::Special(SpecialForm::Eval)),
                "fn*" => Ok (Expr::Special(SpecialForm::Fn)),
                "if" => Ok (Expr::Special(SpecialForm::If)),
                "let*" => Ok (Expr::Special(SpecialForm::LetStar)),
                "quote" => Ok (Expr::Special(SpecialForm::Quote)),
                "quasiquote" => Ok (Expr::Special(SpecialForm::Quasiquote)),
                "@" => Ok(Expr::List(vec![Expr::symbol("deref"), self.read_form()?])),
                "'" => Ok(Expr::List(vec![Expr::Special(SpecialForm::Quote), 
                                          self.read_form()?])),
                "`" => Ok(Expr::List(vec![Expr::Special(SpecialForm::Quasiquote), 
                                          self.read_form()?])),
                "~" => Ok(Expr::List(vec![Expr::symbol("unquote"), self.read_form()?])),
                "~@" => Ok(Expr::List(vec![Expr::symbol("splice-unquote"), self.read_form()?])),
                ")" | "]" | "}" => Err("Mismatched parentheses".into()),
                _ => Ok(Expr::symbol(tok))
            }
        }
    }
}

pub fn read_str(input: &str) -> Result<Expr> {
    let mut tokenizer = Tokenizer::new(input);
    tokenizer.read_form()
}
