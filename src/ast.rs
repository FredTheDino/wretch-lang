#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FileId(pub usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span(pub FileId, pub usize, pub usize);

impl Span {
    pub fn merge(self, b: Span) -> Span {
        let a = self;
        if a.0 == b.0 {
            Span(a.0, a.1.min(b.1), a.2.max(b.2))
        } else {
            a
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct N<'s>(pub &'s str, pub Span);

#[rustfmt::skip]
impl<'s> N<'s> { 
    fn span(&self) -> Span { self.1 }

    fn show(&self) -> String {
        format!("{}", self.0)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct P<'s>(pub &'s str, pub Span);

#[rustfmt::skip]
impl<'s> P<'s> {
    fn span(&self) -> Span { self.1 }

    fn show(&self) -> String {
        format!("{}", self.0)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ast<'s>(pub P<'s>, pub Vec<Decl<'s>>);

impl<'s> Ast<'s> {
    #[rustfmt::skip]
    fn span(&self) -> Span { self.0.span().merge(self.1.last().map_or(self.0.span(), |x| x.span())) }
    #[rustfmt::skip]
    fn name(&self) -> P<'s> { self.0 }

    pub fn show(&self) -> String {
        format!(
            "mod {}\n\n{}",
            self.0.show(),
            self.1
                .iter()
                .map(|x| x.show())
                .collect::<Vec<_>>()
                .join("\n\n")
                .trim()
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DefTyp<'s>(pub Span, pub Vec<Typ<'s>>, pub Span, pub Vec<Typ<'s>>);

impl<'s> DefTyp<'s> {
    fn span(&self) -> Span {
        self.0.merge(self.3.last().map_or(self.2, |x| x.span()))
    }

    fn show(&self) -> String {
        format!(
            "def : {} -> {}",
            self.1
                .iter()
                .map(|x| x.show())
                .collect::<Vec<_>>()
                .join(", ")
                .trim(),
            self.3
                .iter()
                .map(|x| x.show())
                .collect::<Vec<_>>()
                .join(", ")
                .trim(),
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Decl<'s> {
    Sep(Span),
    DefExpr(Option<DefTyp<'s>>, Span, N<'s>, Expr<'s>),
    Typ(Span, P<'s>, Vec<N<'s>>, Typ<'s>),
    Dat(Span, P<'s>, Vec<N<'s>>, Vec<(P<'s>, Option<Typ<'s>>)>),
}

impl<'s> Decl<'s> {
    fn span(&self) -> Span {
        match self {
            Decl::DefExpr(d, s, _, _) => d.as_ref().map_or(*s, |x| x.span()),
            Decl::Sep(s) | Decl::Typ(s, _, _, _) | Decl::Dat(s, _, _, _) => *s,
        }
        .merge(match self {
            Decl::Sep(s) => *s,
            Decl::DefExpr(_, _, _, v) => v.span(),
            Decl::Typ(_, _, _, v) => v.span(),
            Decl::Dat(_, p, e, v) => v
                .last()
                .map_or_else(|| e.last().map_or(p.span(), |x| x.span()), |x| x.0.span()),
        })
    }

    fn show(&self) -> String {
        match self {
            Decl::Sep(_) => "-----".into(),
            Decl::DefExpr(Some(ty), _, n, expr) => {
                format!("{}\ndef {} = {}", ty.show(), n.show(), expr.show())
            }
            Decl::DefExpr(None, _, n, expr) => {
                format!("def {} = {}", n.show(), expr.show())
            }
            Decl::Typ(_, n, xs, t) => {
                format!(
                    "typ {}{} = {}",
                    n.show(),
                    xs.iter()
                        .map(|x| x.show())
                        .collect::<Vec<_>>()
                        .join(" ")
                        .trim(),
                    t.show()
                )
            }
            Decl::Dat(_, _, _, _) => todo!(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr<'s> {
    Chain(Vec<Expr<'s>>),
    Group(Span, Box<Expr<'s>>, Span),
    Ident(N<'s>),
    Int(&'s str, Span),
    If(Span, Box<Expr<'s>>, Box<Expr<'s>>),
    Foreign(Span),
}

impl<'s> Expr<'s> {
    fn span(&self) -> Span {
        match self {
            Expr::Chain(x) => match x.first().map(|x| x.span()).zip(x.last().map(|x| x.span())) {
                Some((lo, hi)) => lo.merge(hi),
                None => unreachable!("Broken invariant - there is no empty expression"),
            },
            Expr::Ident(a) => a.span(),
            Expr::If(a, _, b) => a.merge(b.span()),
            Expr::Group(a, _, b) => a.merge(*b),
            Expr::Int(_, a) => *a,
            Expr::Foreign(s) => *s,
        }
    }

    fn show(&self) -> String {
        match self {
            Expr::Chain(a) => a
                .iter()
                .map(|x| x.show())
                .collect::<Vec<_>>()
                .join(" ")
                .trim()
                .into(),
            Expr::Group(_, x, _) => format!("({})", x.show()),
            Expr::Ident(n) => n.show(),
            Expr::Int(i, _) => format!("{}", i),
            Expr::If(_, tru, fals) => format!(
                "if {} else {}",
                tru.show(),
                fals.show()
            ),
            Expr::Foreign(_) => "foreign".into(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Typ<'s> {
    Fn(Vec<Typ<'s>>, Span, Vec<Typ<'s>>),
    Known(P<'s>),
    Var(N<'s>),
    Foreign(Span),
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
            Typ::Foreign(s) => *s,
        }
    }

    fn show(&self) -> String {
        match self {
            Typ::Fn(a, _, b) => format!(
                "{} -> {}",
                a.iter()
                    .map(|x| x.show())
                    .collect::<Vec<_>>()
                    .join(" ")
                    .trim(),
                b.iter()
                    .map(|x| x.show())
                    .collect::<Vec<_>>()
                    .join(" ")
                    .trim(),
            ),
            Typ::Known(p) => p.show(),
            Typ::Var(v) => v.show(),
            Typ::Foreign(_) => "foreign".into(),
        }
    }
}
