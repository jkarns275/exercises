use std::fmt::Debug;

struct Yy<'y> {
    pub s: &'y[char],
}

impl<'y> Yy<'y> {
    pub fn new<'a>(s: &'a [char]) -> Yy<'a> { Yy { s } }

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

    pub fn consume_ws(&mut self) {
        while self.s.len() != 0 {
            let c = self.s[0];
            if c == ' ' || c == '\t' {
                self.s = &self.s[1..];
            } else {
                return
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

#[derive(Debug)]
enum Expr {
    Paren(AndExpr),
    Id(char),
    False,
    True,
}

#[derive(Debug)]
enum Statement {
    Assignment(char, AndExpr),
    Query(char),
}

#[derive(Debug)]
struct AndExpr(pub Vec<OrExpr>);
impl AndExpr { pub fn new(x: Vec<OrExpr>) -> Self { AndExpr(x) } }

#[derive(Debug)]
struct OrExpr(pub Vec<XorExpr>);
impl OrExpr { pub fn new(x: Vec<XorExpr>) -> Self { OrExpr(x) } }

#[derive(Debug)]
struct XorExpr(pub Vec<NotExpr>);
impl XorExpr { pub fn new(x: Vec<NotExpr>) -> Self { XorExpr(x) } }

#[derive(Debug)]
enum NotExpr {
    Negate(Expr),
    Confirm(Expr),
}

struct Parser<'y>(pub Yy<'y>);

impl<'y> Parser<'y> {
    pub fn new(ch: &'y [char]) -> Self { Parser(Yy::new(ch)) }

    pub fn id(&mut self) -> Result<char, String> {
        self.0.yycharfilter(|c| {
            match c {
                'a'...'z' | 'A'...'Z' => Ok(c),
                _ => Err(format!("Invalid identifier '{}'", c))
            }
        }, || "Encountered unexpected EOF.".to_owned())
    }

    pub fn statement(&mut self) -> Result<Statement, String> {
        let original = self.0.s;
        let ret = match self.query() {
            Ok(query)  => Ok(query),
            Err(_)          => {
                self.0.s = original;
                match self.assignment() {
                    Ok(assignment) => Ok(assignment),
                    Err(e) => {
                        self.0.s = original;
                        Err(format!(r#"
Encountered error while trying to parse a statement:
    Line: "{}"
    Error: "{}""#, self.0.yyline().iter().collect::<String>(), e))
                    }
                }
            }
        };
        self.0.s = original;
        let _ = self.0.yyline();
        ret
    }

    pub fn assignment(&mut self) -> Result<Statement, String> {
        self.0.consume_ws();
        let id = self.id()?;
        self.0.consume_ws();
        let _eq = self.0.yychar('=').map_err(|_| "Expected '='".to_owned())?;
        self.0.consume_ws();
        let expr = self.and_expr()?;
        Ok(Statement::Assignment(id, expr))
    }

    pub fn query(&mut self) -> Result<Statement, String> {
        self.0.consume_ws();
        let id = self.id()?;
        self.0.consume_ws();
        let _qmark = self.0.yychar('?').map_err(|_| format!("Expected '?' after '{}'", id))?;
        Ok(Statement::Query(id))
    }

    pub fn expr(&mut self) -> Result<Expr, String> {
        self.0.consume_ws();
        println!("Next ch: {}", self.0.s.first().unwrap());
        let ret =
            if let Ok(()) = self.0.yychar('(') {
                self.0.consume_ws();
                let ret = Expr::Paren(self.and_expr()?);
                self.0.consume_ws();
                match self.0.yychar(')') {
                    Ok(()) => Ok(ret),
                    Err(str) => Err(format!("Expected ')'."))
                }
            } else if let Ok(()) = self.0.yystr("true") {
                Ok(Expr::True)
            } else if let Ok(()) = self.0.yystr("false") {
                Ok(Expr::False)
            } else if let Ok(id) = self.id() {
                Ok(Expr::Id(id))
            } else {
                Err(format!("Expected 'true', 'false', or '(' ~ Expr ~ ')'."))
            };
        self.0.consume_ws();
        ret
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
        self.0.consume_ws();
        println!("s: {}", self.0.s[0]);
        let mut i = 0;
        while self.0.yychar('!').is_ok() { i += 1; }
        println!("s: {}", self.0.s[0]);
        let expr = self.expr()?;
        Ok(match i & 1 == 0 {
            true => NotExpr::Confirm(expr),
            false => NotExpr::Negate(expr)
        })
    }

    #[inline(always)]
    fn exprrec<T, F>(&mut self, operator: char, inner: F) -> Result<Vec<T>, String>
        where F: Fn(&mut Self) -> Result<T, String> {
        self.0.consume_ws();
        let mut exps = vec![(inner)(self)?];
        loop {
            println!("exprrec: {}", operator);
            self.0.consume_ws();
            if let Err(()) = self.0.yychar(operator) {
                break
            }
            self.0.consume_ws();
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


fn main() {
    let string = "c = !(a | b)\n".chars().collect::<Vec<_>>();
    let mut parser = Parser(Yy::new(&string[..]));

    match parser.statement() {
        Ok(r)   => println!("{:?}", r),
        Err(e)  => println!("{}", e),
    }
}
