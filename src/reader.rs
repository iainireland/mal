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
            [\s,]*                # whitespace (includes commas)
            (                     # start capture group
            ~@                 |  # splice-unquote
            [\[\]{}()'`~^@]    |  # special chars
            "(?:\\.|[^\\"])*"  |  # string literal
            ;.*                |  # comment
            [^\s\[\]{}()'\~^@]+   # symbol        
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
        while *self.tokens.peek().ok_or("Unterminated list")? != close_delim {
            let expr = self.read_form()?;
            contents.push(expr);
        }
        self.tokens.next();
        match delim {
            "(" => Ok(Expr::List(contents)),
            "[" => Ok(Expr::Vector(contents)),
            "{" => unimplemented!(),
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
            unimplemented!();
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
                _ => Ok(Expr::Symbol(Rc::new(String::from(tok))))
            }
        }
    }
}

pub fn read_str(input: &str) -> Result<Expr> {
    let mut tokenizer = Tokenizer::new(input);
    tokenizer.read_form()
}
