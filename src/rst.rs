use drop_bomb::DropBomb;

use crate::ast::{self, FileId, Span, N, P};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Rerror {
    Unknown(String, Span),
    Collision(String, Span, String, Span),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct I(pub usize);

#[derive(Debug)]
pub struct At(usize, DropBomb);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Export {
    Pub,
    Priv,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Resolver<'s> {
    names: Vec<(&'s str, Span)>,
    stack: Vec<(&'s str, I)>,
    errors: Vec<Rerror>,
    export_state: Export,
}

impl<'s> Resolver<'s> {
    fn new() -> Self {
        Self {
            names: vec![(" error ", Span(FileId(0), 0, 0))],
            stack: Vec::new(),
            errors: Vec::new(),
            export_state: Export::Pub,
        }
    }

    fn push(&mut self, name: &'s str, span: Span) {
        let i = I(self.names.len());
        self.names.push((name, span));
        self.stack.push((name, i));
    }

    fn at(&self) -> At {
        At(
            self.stack.len(),
            DropBomb::new("PUSHED A VALUE AND FOREGOT TO POP IT"),
        )
    }

    fn pop(&mut self, At(l, mut bomb): At) {
        bomb.defuse();
        self.stack.truncate(l);
    }

    fn resolve_(&mut self, s: &'s str) -> Option<I> {
        self.stack
            .iter()
            .rev()
            .find(|(n, _)| *n == s)
            .map(|(_, i)| i)
            .copied()
    }

    fn resolve(&mut self, s: &'s str, at: Span) -> I {
        self.resolve_(s).unwrap_or_else(|| {
            self.errors.push(Rerror::Unknown(s.to_string(), at));
            I(0)
        })
    }

    fn p(&mut self, p: &P<'s>) -> I {
        self.resolve(p.0, p.1)
    }
    fn n(&mut self, n: &N<'s>) -> I {
        self.resolve(n.0, n.1)
    }

    fn decl_pre(&mut self, dec: &ast::Decl<'s>) {
        let (n, s) = match dec {
            ast::Decl::Sep(_) => return,
            ast::Decl::DefExpr(_, _, name, _) => (name.0, name.1),
            ast::Decl::Typ(_, name, _, _) | ast::Decl::Dat(_, name, _, _) => (name.0, name.1),
        };
        if let Some(I(i)) = self.resolve_(n) {
            self.errors.push(Rerror::Collision(
                n.into(),
                s,
                self.names[i].0.into(),
                self.names[i].1,
            ));
        } else {
            self.push(n, s)
        }
    }

    fn decl(&mut self, dec: &ast::Decl<'s>) -> Option<Decl> {
        Some(match dec {
            ast::Decl::Sep(_) => {
                self.export_state = Export::Priv;
                return None;
            }
            ast::Decl::DefExpr(m_typ, span, n, body) => Decl::DefExpr(
                self.export_state,
                m_typ.as_ref().map(|typ| {
                    let at = self.at();
                    let d = DefTyp(
                        typ.0,
                        typ.1.iter().map(|x| self.typ(x)).collect(),
                        typ.2,
                        typ.3.iter().map(|x| self.typ(x)).collect(),
                    );
                    self.pop(at);
                    d
                }),
                *span,
                self.n(n),
                {
                    let at = self.at();
                    let d = self.expr(body);
                    self.pop(at);
                    d
                },
            ),
            ast::Decl::Typ(span, p, args, ty) => {
                let at = self.at();
                let resolved = args
                    .iter()
                    .map(|x| {
                        self.push(x.0, x.1);
                        self.n(x)
                    })
                    .collect();
                let ty = self.typ(ty);
                self.pop(at);
                Decl::Typ(self.export_state, *span, self.p(p), resolved, ty)
            }
            ast::Decl::Dat(_, _, _, _) => todo!(),
        })
    }

    fn typ(&mut self, dec: &ast::Typ<'s>) -> Typ {
        match dec {
            ast::Typ::Fn(ins, arr, outs) => Typ::Fn(
                ins.iter().map(|x| self.typ(x)).collect(),
                *arr,
                outs.iter().map(|x| self.typ(x)).collect(),
            ),
            ast::Typ::Known(p) => Typ::Known(self.p(p)),
            ast::Typ::Var(n) => Typ::Var(self.n(n)),
            ast::Typ::Foreign(at) => Typ::Foreign(*at),
        }
    }

    fn expr(&mut self, expr: &ast::Expr<'s>) -> Expr {
        match expr {
            ast::Expr::Chain(chain) => Expr::Chain(chain.iter().map(|x| self.expr(x)).collect()),
            ast::Expr::Group(pre, expr, post) => {
                Expr::Group(*pre, Box::new(self.expr(expr)), *post)
            }
            ast::Expr::Ident(n) => Expr::Ident(self.n(n)),
            ast::Expr::Int(s, _at) => Expr::Int(s.parse().unwrap()),
            ast::Expr::If(at, a, b, c) => Expr::If(
                *at,
                Box::new(self.expr(a)),
                Box::new(self.expr(b)),
                Box::new(self.expr(c)),
            ),
            ast::Expr::Foreign(at) => Expr::Foreign(*at),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rst(pub I, pub Vec<Decl>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DefTyp(pub Span, pub Vec<Typ>, pub Span, pub Vec<Typ>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Decl {
    DefExpr(Export, Option<DefTyp>, Span, I, Expr),
    Typ(Export, Span, I, Vec<I>, Typ),
    Dat(Export, Span, I, Vec<I>, Vec<(I, Option<Typ>)>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Chain(Vec<Expr>),
    Group(Span, Box<Expr>, Span),
    Ident(I),
    Int(i64),
    If(Span, Box<Expr>, Box<Expr>, Box<Expr>),
    Foreign(Span),
}
impl Expr {
    pub fn is_foreign(&self) -> bool {
        matches!(self, Expr::Foreign(_))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Typ {
    Fn(Vec<Typ>, Span, Vec<Typ>),
    Known(I),
    Var(I),
    Foreign(Span),
}

pub fn resolve<'s>(ast: &ast::Ast<'s>) -> (Rst, Vec<(&'s str, Span)>, Vec<Rerror>) {
    let mut res = Resolver::new();
    res.push(ast.0 .0, ast.0 .1);
    let i = res.p(&ast.0);
    let mut decls = Vec::new();

    let pre = res.at();
    for dec in ast.1.iter() {
        res.decl_pre(dec)
    }

    for dec in ast.1.iter() {
        let pre = res.at();
        if let Some(d) = res.decl(dec) {
            decls.push(d);
        }
        res.pop(pre);
    }

    res.pop(pre);

    (Rst(i, decls), res.names, res.errors)
}
