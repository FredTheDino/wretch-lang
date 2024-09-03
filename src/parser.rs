use logos::Logos;

use crate::lexer::Token;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Msg<'s> {
    Unexpected {
        at: Span,
        ctx: &'static str,
        got: Option<Token<'s>>,
    },
    UnexpectedEoF {
        at: Span,
        ctx: &'static str,
    },
    GaveUp(Span),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FileId(pub usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span(FileId, usize, usize);

impl Span {
    fn merge(self, b: Span) -> Span {
        let a = self;
        if a.0 == b.0 {
            Span(a.0, a.1.min(b.1), a.2.max(b.2))
        } else {
            a
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct N<'s>(&'s str, Span);

#[rustfmt::skip]
impl<'s> N<'s> { 
    fn span(&self) -> Span { self.1 }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct P<'s>(&'s str, Span);

#[rustfmt::skip]
impl<'s> P<'s> {
    fn span(&self) -> Span { self.1 }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ast<'s>(P<'s>, Vec<Decl<'s>>);

#[rustfmt::skip]
impl<'s> Ast<'s> {
    fn span(&self) -> Span { self.0.span().merge(self.1.last().map_or(self.0.span(), |x| x.span())) }
    fn name(&self) -> P<'s> { self.0 }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct DefTyp<'s>(Span, Vec<Typ<'s>>, Span, Vec<Typ<'s>>);

impl<'s> DefTyp<'s> {
    fn span(&self) -> Span {
        self.0.merge(self.3.last().map_or(self.2, |x| x.span()))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Decl<'s> {
    DefExpr(Option<DefTyp<'s>>, Span, N<'s>, Expr<'s>),
    Typ(Span, P<'s>, Vec<N<'s>>, Typ<'s>),
    Dat(Span, P<'s>, Vec<N<'s>>, Vec<(P<'s>, Option<Typ<'s>>)>),
}

impl<'s> Decl<'s> {
    fn span(&self) -> Span {
        match self {
            Decl::DefExpr(d, s, _, _) => d.as_ref().map_or(*s, |x| x.span()),
            Decl::Typ(s, _, _, _) | Decl::Dat(s, _, _, _) => *s,
        }
        .merge(match self {
            Decl::DefExpr(_, _, _, v) => v.span(),
            Decl::Typ(_, _, _, v) => v.span(),
            Decl::Dat(_, p, e, v) => v
                .last()
                .map_or_else(|| e.last().map_or(p.span(), |x| x.span()), |x| x.0.span()),
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Expr<'s> {
    Chain(Vec<Expr<'s>>),
    Group(Span, Box<Expr<'s>>, Span),
    Ident(N<'s>),
    Int(&'s str, Span),
    If(Span, Box<Expr<'s>>, Box<Expr<'s>>, Box<Expr<'s>>),
}

impl<'s> Expr<'s> {
    fn span(&self) -> Span {
        match self {
            Expr::Chain(x) => match x.first().map(|x| x.span()).zip(x.last().map(|x| x.span())) {
                Some((lo, hi)) => lo.merge(hi),
                None => unreachable!("Broken invariant - there is no empty expression"),
            },
            Expr::Ident(a) => a.span(),
            Expr::If(a, _, _, b) => a.merge(b.span()),
            Expr::Group(a, _, b) => a.merge(*b),
            Expr::Int(_, a) => *a,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Typ<'s> {
    Fn(Vec<Typ<'s>>, Span, Vec<Typ<'s>>),
    Known(P<'s>),
    Var(N<'s>),
}

impl<'s> Typ<'s> {
    fn span(&self) -> Span {
        match self {
            Typ::Fn(a, m, b) => a
                .last()
                .map_or(*m, |x| x.span())
                .merge(b.last().map_or(*m, |x| x.span())),

            Typ::Known(a) => a.span(),
            Typ::Var(a) => a.span(),
        }
    }
}

pub fn parse<'s>(source: &'s str, file_id: FileId) -> (Vec<Msg<'s>>, Option<Ast<'s>>) {
    let mut prs = Parser::new(source, file_id);

    let maybe_ast = if let Some(name) = prs.header() {
        let mut decls = Vec::new();

        while let Ok(_) = prs.recover(|t| matches!(t, Token::Def | Token::Typ)) {
            if prs.eof() { break }
            if let Some(stmt) = prs.decls() {
                decls.push(stmt);
            } else {
                prs.skip();
            }
        }
        assert!(prs.eof());
        Some(Ast(name, decls))
    } else {
        None
    };

    (prs.msgs, maybe_ast)
}

macro_rules! expect {
    ($prs:expr, $pat:pat, $ctx:expr) => {{
        match $prs.next($ctx) {
            Some((x @ $pat, at)) => Some((x, at)),
            Some((got, at)) => {
                $prs.msg(crate::parser::Msg::Unexpected {
                    at,
                    ctx: $ctx,
                    got: Some(got),
                });
                None
            }
            None => {
                $prs.msg(crate::parser::Msg::Unexpected {
                    at: Span($prs.file_id, 0, 0),
                    ctx: $ctx,
                    got: None,
                });
                None
            }
        }
    }};
}

impl<'s> Parser<'s> {
    fn propper(&mut self, msg: &'static str) -> Option<P<'s>> {
        match self.next(msg)? {
            (Token::Propper(name), at) => Some(P(name, at)),
            or => {
                self.unexpected(or, msg);
                None
            }
        }
    }

    fn name(&mut self, msg: &'static str) -> Option<N<'s>> {
        match self.next(msg)? {
            (Token::Name(name), at) => Some(N(name, at)),
            or => {
                self.unexpected(or, msg);
                None
            }
        }
    }

    fn header(&mut self) -> Option<P<'s>> {
        expect!(self, Token::Mod, "Modules should start with `mod`")?;
        self.propper("Module names are propper names")
    }

    fn decls(&mut self) -> Option<Decl<'s>> {
        match expect!(
            self,
            Token::Def | Token::Typ | Token::Dat,
            "Expected start of declaration - `def`, `typ` or `dat`"
        )? {
            (Token::Def, at) => {
                if let Some((Token::Name(":"), _)) = self.peek() {
                    self.decl_def_ty(at)
                } else {
                    self.decl_def_expr(None, at)
                }
            }
            (Token::Typ, at) => self.decl_typ(at),
            (Token::Dat, at) => self.decl_dat(at),
            _ => {
                unreachable!()
            }
        }
    }

    fn fn_typ(&mut self) -> Option<(Vec<Typ<'s>>, Span, Vec<Typ<'s>>)> {
        let mut pre = Vec::new();
        pre.push(self.typ()?);
        while matches!(self.peek(), Some((Token::Comma, _))) {
            self.next("Stack arguments are seperated by `,`");
            pre.push(self.typ()?);
        }
        let (_, arrow_at) = expect!(
            self,
            Token::Name("->"),
            "Seperate the input stack from the output stack with `->`"
        )?;

        let mut post = Vec::new();
        post.push(self.typ()?);
        while matches!(self.peek(), Some((Token::Comma, _))) {
            self.next("Stack arguments are seperated by `,`");
            post.push(self.typ()?);
        }
        Some((pre, arrow_at, post))
    }

    fn decl_def_ty(&mut self, at: Span) -> Option<Decl<'s>> {
        expect!(
            self,
            Token::Name(":"),
            "Type declarations are indicated with a `:`"
        )?;

        let (pre, arrow_at, post) = self.fn_typ()?;

        let (_, def_at) = expect!(
            self,
            Token::Def,
            "Expected a definition to follow a type declaration"
        )?;
        self.decl_def_expr(Some(DefTyp(at, pre, arrow_at, post)), def_at)
    }

    fn decl_def_expr(&mut self, typ: Option<DefTyp<'s>>, at: Span) -> Option<Decl<'s>> {
        let name = self.name("Expected a name for the definition")?;
        expect!(self, Token::Name("="), "Expected `=` followed by an expression")?;
        let body = self.expr(None)?;
        Some(Decl::DefExpr(typ, at, name, body))
    }

    fn decl_typ(&mut self, at: Span) -> Option<Decl<'s>> {
        let name = self.propper("A type alias must have a propper name")?;
        let mut args = Vec::new();
        while let Some((Token::Name(n), at)) = self.peek() {
            self.next("Expected a valid name");
            args.push(N(n, at));
        }
        expect!(self, Token::Name("="), "Expected `=` in typ-declaration");
        let typ = self.typ()?;
        Some(Decl::Typ(at, name, args, typ))
    }

    fn decl_dat(&mut self, at: Span) -> Option<Decl<'s>> {
        todo!()
    }

    fn typ(&mut self) -> Option<Typ<'s>> {
        Some(match self.peek()? {
            (Token::LParen, _open) => {
                self.next("Expected '('")?;
                let (pre, arrow_at, post) = self.fn_typ()?;
                // TODO: point to opening paren
                expect!(
                    self,
                    Token::RParen,
                    "Expected the function call to end here"
                )?;
                Typ::Fn(pre, arrow_at, post)
            }
            // TODO: Higher kinded types
            (Token::Propper(_), _) => Typ::Known(self.propper("Expected the name of a type")?),
            (Token::Name(_), _) => Typ::Var(self.name("Expected the name of a type")?),
            or => {
                self.unexpected(or, "Expected the start of a type - `(`, `Name` or `name´");
                return None;
            }
        })
    }

    fn expr(&mut self, err: Option<&'static str>) -> Option<Expr<'s>> {
        let mut links = vec![self.expr_inner(err.or(Some("Expected an expression")))?];
        while let Some(next) = self.expr_inner(None) {
            links.push(next);
        }
        Some(Expr::Chain(links))
    }

    fn expr_inner(&mut self, err: Option<&'static str>) -> Option<Expr<'s>> {
        Some(match self.peek()? {
            (Token::LParen, open) => {
                self.skip();
                let inner = self.expr(Some("Expected an expression after `(`"))?;
                // TODO: point to opening paren
                let (_, close) = expect!(
                    self,
                    Token::RParen,
                    "Expected `)` to end the expression grouping"
                )?;
                Expr::Group(open, Box::new(inner), close)
            }
            (Token::If, _) => {
                let (_, start) = expect!(self, Token::If, "if-expressions start with `if`")?;
                let cond = self.expr(Some("Expected expression after `if`"))?;
                let (_, _) = expect!(
                    self,
                    Token::Then,
                    "`then` should come after the if-expression condition"
                )?;
                let tru = self.expr(Some("Expected expression after `then`"))?;
                let (_, _) = expect!(
                    self,
                    Token::Else,
                    "`else` should come after the if-expressions true-branch"
                )?;
                let fals = self.expr(Some("Expected expression after `else`"))?;
                Expr::If(start, Box::new(cond), Box::new(tru), Box::new(fals))
            }
            (Token::Name(_), _) => {
                Expr::Ident(self.name("Expected an identifier")?)
            }
            (Token::Int(n), at) => {
                self.skip();
                Expr::Int(n, at)
            }
            or => {
                if let Some(msg) = err {
                    self.unexpected(or, msg);
                }
                return None;
            }
        })
    }
}

struct Parser<'s> {
    file_id: FileId,
    tokens: Vec<Option<(Token<'s>, Span)>>,
    msgs: Vec<Msg<'s>>,
    panicing: bool,
    i: usize,
}

impl<'s> Parser<'s> {
    fn new(source: &'s str, file_id: FileId) -> Parser<'s> {
        Self {
            file_id,
            tokens: Token::lexer(source)
                .spanned()
                .map(|(mt, s)| mt.ok().map(|t| (t, Span(file_id, s.start, s.end))))
                .collect(),
            i: 0,
            panicing: false,
            msgs: Vec::new(),
        }
    }

    fn eof(&self) -> bool {
        self.i >= self.tokens.len()
    }

    fn panic(&mut self) {
        self.panicing = true;
    }

    fn recover<F>(&mut self, f: F) -> Result<(), ()>
    where
        F: Fn(Token<'s>) -> bool,
    {
        if !self.panicing {
            return Ok(());
        }
        loop {
            if let Some(x) = self.peek_() {
                if f(x.0) {
                    self.panicing = true;
                    return Ok(());
                } else {
                    self.i += 1
                }
            } else {
                return Err(());
            }
        }
    }

    fn peek_(&mut self) -> Option<(Token<'s>, Span)> {
        match self.tokens.get(self.i).copied() {
            Some(Some(x)) => Some(x),
            Some(None) => {
                let s = match (self.tokens.iter().take(self.i).rev().find(|x| x.is_some()).copied().flatten().map(|x| x.1),
                self.tokens.iter().skip(self.i).find(|x| x.is_some()).copied().flatten().map(|x| x.1)) {
                    (Some(pre), Some(post)) => pre.merge(post),
                    (Some(pre), None) => pre,
                    (None, Some(post)) => post,
                    (None, None) => Span(self.file_id, 0, 0),
                };
                self.msg(Msg::GaveUp(s));
                None
            },
            None => None,
        }
    }

    fn skip(&mut self) {
        self.i += 1;
    }

    fn next(&mut self, ctx: &'static str) -> Option<(Token<'s>, Span)> {
        if self.panicing {
            return None;
        }
        let out = self.peek_();
        self.skip();
        if out.is_none() {
            self.msg(Msg::UnexpectedEoF {
                ctx,
                at: if let Some(Some((_, at))) = self.tokens.iter().rev().find(|x| x.is_some()) {
                    *at
                } else {
                    Span(self.file_id, 0, 0)
                },
            });
        }
        out
    }

    fn peek(&mut self) -> Option<(Token<'s>, Span)> {
        if self.panicing {
            None
        } else {
            self.peek_()
        }
    }

    fn msg(&mut self, msg: Msg<'s>) {
        // Silence errors until we recover
        if self.panicing {
            return;
        }
        self.msgs.push(msg);
        self.panic();
    }

    fn unexpected(&mut self, (got, at): (Token<'s>, Span), ctx: &'static str) {
        self.msg(Msg::Unexpected {
            at,
            ctx,
            got: Some(got),
        });
    }
}
