#![feature(const_fn)]

use std::fmt::Debug;

struct Yy<'y> {
    pub s: &'y[char],
}

impl<'y> Yy<'y> {
    pub fn new<'a>(s: &'a [char]) -> Yy<'a> { Yy { s } }

    pub fn yynextchar(&mut self) -> Result<char, ()> {
        if self.s.len() == 0 {
            Err(())
        } else {
            let c = self.s[0];
            self.s = &self.s[1..];
            Ok(c)
        }
    }

    pub fn yychar(&mut self, c: char) -> Result<(), ()> {
        if self.s.len() == 0 || self.s[0usize] != c {
            Err(())
        } else {
            self.s = &self.s[1..];
            Ok(())
        }
    }

    pub fn yycharfilter<F, T, G>(&mut self, f: F, eof_error_gen: G) -> Result<char, T>
        where
            F: Fn(char) -> Result<char, T>,
            G: Fn() -> T {
        if self.s.len() == 0 {
            Err((eof_error_gen)())
        } else {
            match (f)(self.s[0]) {
                Ok(ch) => {
                    self.s = &self.s[1..];
                    Ok(ch)
                },
                Err(err) => Err(err)
            }
        }
    }

    pub fn yyconsume_chars(&mut self, chars: &str) {
        while self.s.len() != 0 {
            let c = self.s[0];
            if chars.contains(c) {
                self.s = &self.s[1..];
            } else {
                return
            }
        }
    }

    pub fn yystr(&mut self, s: &str) -> Result<(), ()> {
        let mut other = s;
        let mut s = self.s;
        if other.len() > s.len() { return Err(()) }
        for c in other.chars() {
            if s[0] != c {
                return Err(())
            }
            s = &s[1..];
        }
        self.s = s;
        Ok(())
    }

    pub fn yymatch<T: Clone + Debug>(&mut self, strs: &[(&str, T)]) -> Result<T, String> {
        for (s, t) in strs {
            if let Ok(()) = self.yystr(s) {
                return Ok(t.clone())
            }
        }
        let rstr = format!("Expected one of: {:?}, got '{:?}'",
                           strs,
                           self.yyline().iter().collect::<String>());
        Err(rstr)
    }

    pub fn yyline(&'y self) -> &'y [char] {
        const start: usize = 0;
        let mut end: usize = 0;
        while self.s.len() > end {
            if '\n' == self.s[end] { break }
            end += 1;
        }
        &self.s[start..end]
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Paren(AndExpr),
    Id(char),
    False,
    True,
}

impl Expr {
    pub fn eval(&self, others: &mut Vars) -> Result<bool, Vec<char>> {
        match self {
            Expr::Paren(andexpr) => andexpr.eval(others),
            Expr::Id(ref ch) => {
                if others.get(*ch).is_none() { return Err(vec![*ch]) }
                let (line, expr) = others.get(*ch).take().unwrap();
                let res = expr.eval(others);
                others.set(*ch, line, expr);
                res
            },
            Expr::False => Ok(false),
            Expr::True => Ok(true),
        }
    }
}

#[derive(Debug, Clone)]
enum Statement {
    Assignment(char, AndExpr),
    Query(char),
}

#[derive(Debug, Clone)]
struct AndExpr(pub Vec<OrExpr>);
impl AndExpr {
    pub fn new(x: Vec<OrExpr>) -> Self { AndExpr(x) }

    pub fn eval(&self, others: &mut Vars) -> Result<bool, Vec<char>> {
        let mut errors = None;
        let mut ret = true;
        for orexp in self.0.iter() {
            match orexp.eval(others) {
                Ok(t) => ret &= t,
                Err(e) => {
                    let errs = errors.get_or_insert_with(|| vec![]);
                    errs.extend_from_slice(&e[..]);
                    errs.sort();
                    errs.dedup();
                },
            }
        }
        match errors {
            None => Ok(ret),
            Some(err) => Err(err),
        }
    }
}

#[derive(Debug, Clone)]
struct OrExpr(pub Vec<XorExpr>);
impl OrExpr {
    pub fn new(x: Vec<XorExpr>) -> Self { OrExpr(x) }

    pub fn eval(&self, others: &mut Vars) -> Result<bool, Vec<char>> {
        let mut errors = None;
        let mut ret = false;
        for xorexp in self.0.iter() {
            match xorexp.eval(others) {
                Ok(t) => ret |= t,
                Err(e) => {
                    let errs = errors.get_or_insert_with(|| vec![]);
                    errs.extend_from_slice(&e[..]);
                    errs.sort();
                    errs.dedup();
                },
            }
        }
        match errors {
            None => Ok(ret),
            Some(err) => Err(err),
        }
    }
}

#[derive(Debug, Clone)]
struct XorExpr(pub Vec<NotExpr>);
impl XorExpr {
    pub fn new(x: Vec<NotExpr>) -> Self { XorExpr(x) }

    pub fn eval(&self, others: &mut Vars) -> Result<bool, Vec<char>> {
        let mut errors = None;
        let mut ret = false;
        for notexp in self.0.iter() {
            match notexp.eval(others) {
                Ok(t) => ret ^= t,
                Err(e) => {
                    let errs = errors.get_or_insert_with(|| vec![]);
                    errs.extend_from_slice(&e[..]);
                    errs.sort();
                    errs.dedup();
                },
            }
        }
        match errors {
            None => Ok(ret),
            Some(err) => Err(err),
        }
    }
}

#[derive(Debug, Clone)]
enum NotExpr {
    Negate(Expr),
    Confirm(Expr),
}

impl NotExpr {
    pub fn eval(&self, others: &mut Vars) -> Result<bool, Vec<char>> {
        match self {
            NotExpr::Negate(e)   => e.eval(others).map(|t| !t),
            NotExpr::Confirm(e)  => e.eval(others),
        }
    }
}

struct Parser<'y>(pub Yy<'y>);

impl<'y> Parser<'y> {
    pub fn new(ch: &'y [char]) -> Self { Parser(Yy::new(ch)) }

    pub fn id(&mut self) -> Result<char, String> {
        self.0.yycharfilter(|c| {
            match c {
                'a'...'z' => Ok(c),
                _ => Err(format!("Invalid identifier '{}'", c))
            }
        }, || "Encountered unexpected EOL.".to_owned())
    }

    pub fn statement(&mut self) -> Result<Statement, String> {
        let original = self.0.s;
        let id = self.id()?;
        match self.0.yynextchar() {
            Ok('?') => {
                if self.0.s.len() == 0 {
                    Ok(Statement::Query(id))
                } else {
                    Err(format!("Expected EOL, instead found '{}'", self.0.s[0]))
                }
            },
            Ok('=') => Ok(Statement::Assignment(id, self.and_expr()?)),
            Ok(_) => Err(format!("Expected '=' or '?', found {}", self.0.s[0])),
            Err(e) => Err("Unexpected EOL.".to_string())
        }
    }

    pub fn expr(&mut self) -> Result<Expr, String> {
        match self.0.yynextchar() {
            Ok('(') => {
                let ret = Expr::Paren(self.and_expr()?);
                match self.0.yychar(')') {
                    Ok(()) => Ok(ret),
                    Err(str) => Err(format!("Expected ')'."))
                }
            },
            Ok('1') => Ok(Expr::True),
            Ok('0') => Ok(Expr::False),
            a @ Ok('a'...'z')  => Ok(Expr::Id(a.unwrap())),
            Ok(_) => Err(format!("Expected '1', '0', or '(' ~ Expr ~ ')'.")),
            Err(_) => Err(format!("Unexpected EOL."))
        }
    }

    pub fn and_expr(&mut self) -> Result<AndExpr, String> {
        self.exprrec('&', Self::or_expr).map(AndExpr::new)
    }

    pub fn or_expr(&mut self) -> Result<OrExpr, String> {
        self.exprrec('|', Self::xor_expr).map(OrExpr::new)
    }

    pub fn xor_expr(&mut self) -> Result<XorExpr, String> {
        self.exprrec('^', Self::not_expr).map(XorExpr::new)
    }

    pub fn not_expr(&mut self) -> Result<NotExpr, String> {
        let mut i = 0;
        while self.0.yychar('~').is_ok() { i += 1; }
        let expr = self.expr()?;
        Ok(match i & 1 == 0 {
            true => NotExpr::Confirm(expr),
            false => NotExpr::Negate(expr)
        })
    }

    #[inline(always)]
    fn exprrec<T, F>(&mut self, operator: char, inner: F) -> Result<Vec<T>, String>
        where F: Fn(&mut Self) -> Result<T, String> {
        let mut exps = vec![(inner)(self)?];
        loop {
            if let Err(()) = self.0.yychar(operator) {
                break
            }
            let exp = (inner)(self);
            match exp {
                Err(err) => return Err(err),
                Ok(exp) => exps.push(exp),
            };
        }
        Ok(exps)
    }

    pub fn and(&mut self) -> Result<(), ()> {
        self.0.yychar('&')
    }
}
use std::io::prelude::*;
use std::io::BufReader;
use std::io;

struct Vars(pub Vec<Option<(String, Expr)>>);
impl Vars {
    fn ch_to_ind(ch: char) -> usize { (ch as u8 - 'a' as u8) as usize }
    pub fn set(&mut self, id: char, string: String, expr: Expr) {
        self.0[Vars::ch_to_ind(id)] = Some((string, expr));
    }

    pub fn get<'a>(&'a mut self, id: char) -> &'a mut Option<(String, Expr)> {
        &mut self.0[Vars::ch_to_ind(id)]
    }

    pub fn query(&mut self, id: char) {
        if self.0[Vars::ch_to_ind(id)].is_none() {
            println!("'{}' is undefined", id);
            return;
        }
        let (line, expr) = self.get(id).take().unwrap();
        match expr.eval(self) {
            Ok(truth_value) => println!("{} = {}", line, truth_value),
            Err(unbound_vars) =>
            println!("Failed to evaluate '{}' because the following variables were not sufficiently defined (i.e. they're recursively defined, or rely on unbound variables): {:?}", 
                line, unbound_vars),
        }
        self.set(id, line, expr);
    }
}

fn main() -> io::Result<()> {

    let mut vars: Vars = Vars(vec![None; 26]);
    let mut reader = io::stdin();
    let mut buffer = String::new();
    loop {
        reader.read_line(&mut buffer)?;
        buffer.pop();
        if &buffer[..] == "end" || &buffer[..] == "quit" {
            break
        }
        let chrs = buffer.chars().filter(|x| { !("\t\n\r ".contains(*x)) }).collect::<Vec<char>>();
        let mut parser = Parser(Yy::new(&chrs[..])); 
        match parser.statement() {
            Ok(Statement::Assignment(ch, expr)) => vars.set(ch, buffer.clone(), Expr::Paren(expr)),
            Ok(Statement::Query(ch)) => vars.query(ch),
            Err(e) => println!("{}", e),
        }
        buffer.clear();
    }

    Ok(())
}
