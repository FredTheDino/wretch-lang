use crate::lexer::Token;

enum Messages<'s> {
    Unexpected {
        ctx: &'static str,
        got: Token<'s>,
        error: String,
    },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct FileId(usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct Span(FileId, usize, usize);

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
struct N<'s>(Span, &'s str);

#[rustfmt::skip]
impl<'s> N<'s> { 
    fn span(&self) -> Span { self.0 }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct P<'s>(Span, &'s str);

#[rustfmt::skip]
impl<'s> P<'s> {
    fn span(&self) -> Span { self.0 }
}

struct Ast<'s>(FileId, Span, P<'s>, Vec<Decl<'s>>);

#[rustfmt::skip]
impl<'s> Ast<'s> {
    fn span(&self) -> Span { self.1.merge(self.3.last().map_or(self.1, |x| x.span())) }
    fn name(&self) -> P<'s> { self.2 }
}

enum Decl<'s> {
    DefTyp(Span, P<'s>, Typ<'s>, Typ<'s>),
    DefExpr(Span, N<'s>, Expr<'s>),
    Typ(Span, P<'s>, Vec<N<'s>>, Typ<'s>),
    Dat(Span, P<'s>, Vec<N<'s>>, Vec<(P<'s>, Option<Typ<'s>>)>),
}

impl<'s> Decl<'s> {
    fn span(&self) -> Span {
        match self {
            Decl::DefTyp(s, _, _, _)
            | Decl::DefExpr(s, _, _)
            | Decl::Typ(s, _, _, _)
            | Decl::Dat(s, _, _, _) => s,
        }
        .merge(match self {
            Decl::DefTyp(_, _, _, v) => v.span(),
            Decl::DefExpr(_, _, v) => v.span(),
            Decl::Typ(_, _, _, v) => v.span(),
            Decl::Dat(_, p, e, v) => v
                .last()
                .map_or_else(|| e.last().map_or(p.span(), |x| x.span()), |x| x.0.span()),
        })
    }
}

enum Expr<'s> {
    Call(Box<Expr<'s>>, Box<Expr<'s>>),
    Ident(N<'s>),
    If(Span, Box<Expr<'s>>, Box<Expr<'s>>, Box<Expr<'s>>),
}

impl<'s> Expr<'s> {
    fn span(&self) -> Span {
        match self {
            Expr::Call(a, b) => a.span().merge(b.span()),
            Expr::Ident(a) => a.span(),
            Expr::If(a, _, _, b) => a.merge(b.span()),
        }
    }
}

enum Typ<'s> {
    Call(Box<Typ<'s>>, Box<Typ<'s>>),
    Known(P<'s>),
    Var(N<'s>),
}

impl<'s> Typ<'s> {
    fn span(&self) -> Span {
        match self {
            Typ::Call(a, b) => a.span().merge(b.span()),
            Typ::Known(a) => a.span(),
            Typ::Var(a) => a.span(),
        }
    }
}

fn parse<'s>(source: &'s str) -> (Vec<Messages<'s>>, Option<Ast<'s>>) {
    todo!()
}
