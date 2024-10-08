use crate::{ast::Span, rst};

#[derive(Debug, Clone)]
pub enum Terror {
    ForeignDefWithoutType(rst::I, Span),
    Generic(&'static str),
    MeetError(Variant, Variant),
    StackSizeMissmatch(usize, usize),
}

#[derive(Debug, Clone, Copy)]
enum Variant {
    Unknown,
    Newtype(usize),
    Function(usize, usize),
    // Kind checking?
    Type,
}

impl Variant {
    fn arity(&self) -> Option<usize> {
        match self {
            Variant::Unknown => None,
            Variant::Newtype(_) => Some(0),
            Variant::Function(a, b) => Some(a + b),
            Variant::Type => Some(0),
        }
    }

    fn meet(self, variant: Variant) -> Result<Variant, Terror> {
        Ok(match (self, variant) {
            (Variant::Unknown, x) | (x, Variant::Unknown) => x,
            (Variant::Newtype(a), Variant::Newtype(b)) if a == b => Variant::Newtype(a),
            (Variant::Function(a1, a2), Variant::Function(b1, b2)) if a1 == b1 && a2 == b2 => {
                Variant::Function(a1, a2)
            }
            (Variant::Type, Variant::Type) => Variant::Type,
            (a, b) => return Err(Terror::MeetError(a, b)),
        })
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct Key(usize);

impl Key {
    fn i(rst::I(i, _): rst::I) -> Key {
        Key(i)
    }
}

#[derive(Debug, Clone, Copy)]
enum Poi {
    Unify(Key, Key, Span),
    Define(Key, Span),
    Explicit(Key, Variant, Span),
}

#[derive(Debug, Clone)]
struct FullVertex {
    variant: Variant,
    poi: Vec<Poi>,
    children: Vec<Option<Key>>,
    bounds: Vec<Key>,
}

impl FullVertex {
    fn unknown(i: usize, s: Span) -> Self {
        Self {
            variant: Variant::Unknown,
            poi: vec![Poi::Define(Key(i), s)],
            children: Vec::new(),
            bounds: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum Vertex {
    Full(FullVertex),
    Fwd(Key),
    Fail,
}

#[derive(Debug, Clone)]
pub struct Graph<'s>(pub Vec<Vertex>, pub Vec<Terror>, pub &'s [(&'s str, Span)]);

impl<'s> Graph<'s> {
    fn new(mapping: &'s [(&'s str, Span)]) -> Self {
        Self(
            mapping
                .iter()
                .enumerate()
                .map(|(i, (_, s))| Vertex::Full(FullVertex::unknown(i, *s)))
                .collect(),
            Vec::new(),
            mapping,
        )
    }

    fn rst(&mut self, rst: &rst::Rst) {
        for decl in rst.1.iter() {
            self.decl_ty(decl)
        }
        for decl in rst.1.iter() {
            self.decl(decl)
        }
    }

    fn decl_ty(&mut self, decl: &rst::Decl) {
        match decl {
            rst::Decl::DefExpr(_, m_typ, at, name, rst::Expr::Foreign(_)) => {
                if let Some(rst::DefTyp(_, a, at, b)) = m_typ {
                    // TODO: Remove this clone by inlining the expression
                    let typ_t = self.typ(&rst::Typ::Fn(a.clone(), *at, b.clone()));
                    self.unify(Key::i(*name), typ_t, *at);
                } else {
                    self.error(Terror::ForeignDefWithoutType(*name, *at));
                }
            }
            rst::Decl::DefExpr(_, m_typ, _, name, _) => {
                if let Some(rst::DefTyp(_, a, at_def, b)) = m_typ {
                    self.unify_explicit(
                        Key::i(*name),
                        Variant::Function(a.len(), b.len()),
                        *at_def,
                    );
                    let stack_in: Vec<_> = a.iter().map(|t| self.typ(t)).collect();
                    let stack_out: Vec<_> = b.iter().map(|t| self.typ(t)).collect();

                    for (i, x_t) in stack_in.into_iter().enumerate() {
                        let c_t = self.child(Key::i(*name), i);
                        self.unify(x_t, c_t, *at_def);
                    }
                    for (i, x_t) in stack_out.into_iter().enumerate() {
                        let c_t = self.child(Key::i(*name), a.len() + i - 1);
                        self.unify(x_t, c_t, *at_def);
                    }
                } else {
                    self.error(Terror::Generic("YOU MUST SUPPLY AT LEAST AN ARRITY!"))
                }
            }
            rst::Decl::Typ(_, _, _, args, rst::Typ::Foreign(_)) => {
                assert!(args.is_empty());
            }
            rst::Decl::Typ(_, _, _, _, _) => todo!(),
            rst::Decl::Dat(_, _, _, _, _) => todo!(),
        }
    }

    fn decl(&mut self, decl: &rst::Decl) {
        match decl {
            rst::Decl::DefExpr(_, _, _, _, rst::Expr::Foreign(_)) => { /* Handled earlier */ }
            rst::Decl::DefExpr(_, m_typ, at, name, expr) => {
                if let Some(rst::DefTyp(_, a, _, b)) = m_typ {
                    let stack_in: Vec<_> = a.iter().map(|t| self.typ(t)).collect();
                    let stack_out_size = b.len();

                    for (i, x_t) in stack_in.iter().enumerate() {
                        let c_t = self.child(Key::i(*name), i);
                        self.unify(*x_t, c_t, *at);
                    }

                    let stack_out = self.expr(expr, stack_in.clone());
                    if stack_out_size != stack_out.len() {
                        self.error(Terror::StackSizeMissmatch(stack_out_size, stack_out.len()));
                        for (i, x_t) in stack_out.iter().enumerate() {
                            println!("{}: {}", i, self.show(*x_t));
                        }
                    }
                    for (i, x_t) in stack_out.into_iter().enumerate().take(stack_out_size) {
                        let c_t = self.child(Key::i(*name), a.len() + i);
                        println!("OUT: {} {}", i, self.show(x_t));
                        self.unify(x_t, c_t, *at);
                    }
                    println!("DONE {}", self.name(*name));
                    println!("TY: {}", self.show(Key::i(*name)));
                } else {
                    self.error(Terror::Generic("YOU MUST SUPPLY AT LEAST AN ARRITY!"));
                }
            }
            rst::Decl::Typ(_, _, _, args, rst::Typ::Foreign(_)) => {
                assert!(args.is_empty());
            }
            rst::Decl::Typ(_, _, _, _, _) => todo!(),
            rst::Decl::Dat(_, _, _, _, _) => todo!(),
        }
    }

    fn expr(&mut self, expr: &rst::Expr, mut stack: Vec<Key>) -> Vec<Key> {
        match expr {
            rst::Expr::Chain(xs) => {
                for x in xs.iter() {
                    stack = self.expr(x, stack);
                }
                stack
            }
            rst::Expr::Group(_, e, _) => self.expr(e, stack),
            rst::Expr::Ident(i) => {
                let (variant, children) = if let Some(FullVertex {
                    variant, children, ..
                }) = self.resolve(Key::i(*i))
                {
                    (variant.clone(), children.clone())
                } else {
                    self.error(Terror::Generic("Cannot call an error"));
                    return stack;
                };
                // TODO: Add call POI?
                let (a, b) = if let Variant::Function(a, b) = variant {
                    (a, b)
                } else {
                    self.error(Terror::Generic("Calling a non-function"));
                    return stack;
                };
                assert_eq!(a + b, children.len());

                for n in 0..a {
                    match (children[n], stack.pop()) {
                        (Some(kiddo), Some(s)) => {
                            self.unify(kiddo, s, i.1);
                        }
                        (Some(_), None) => self.error(Terror::Generic("Stack is empty")),
                        (None, Some(s)) => {
                            self.resolve_mut(Key::i(*i)).unwrap().children[n] = Some(s);
                        }
                        (None, None) => self.error(Terror::Generic("Stack is empty and no child")),
                    };
                }
                for n in a..a + b {
                    match children[n] {
                        None => {
                            let t = self.gen(i.1);
                            self.resolve_mut(Key::i(*i)).unwrap().children[n] = Some(t);
                            stack.push(t)
                        }
                        Some(t) => stack.push(t),
                    }
                }
                stack
            }
            rst::Expr::Int(_) => {
                stack.push(Key(self
                    .2
                    .iter()
                    .enumerate()
                    .find(|(_, (n, _))| *n == "Int")
                    .expect("No 'Int' type assigned")
                    .0));
                stack
            }
            rst::Expr::If(at, tru, fal) => {
                let cond = stack.pop().expect("Stack is empty");
                let bool = Key(self
                    .2
                    .iter()
                    .enumerate()
                    .find(|(_, (n, _))| *n == "Bool")
                    .expect("No 'Bool' type assigned")
                    .0);
                self.unify(cond, bool, *at);
                let tru_stack = self.expr(tru, stack.clone());
                let fal_stack = self.expr(fal, stack);
                self.unifys(tru_stack, fal_stack, *at)
            }
            rst::Expr::Foreign(_) => unreachable!(),
        }
    }

    fn error(&mut self, err: Terror) {
        self.1.push(err)
    }

    fn typ(&mut self, typ: &rst::Typ) -> Key {
        match typ {
            rst::Typ::Fn(a, at, b) => {
                let t = self.gen(*at);
                self.unify_explicit(t, Variant::Function(a.len(), b.len()), *at);
                for (i, x) in a.iter().enumerate() {
                    let x_t = self.typ(x);
                    let c_t = self.child(t, i);
                    self.unify(x_t, c_t, x.at());
                }
                for (i, x) in b.iter().enumerate() {
                    let x_t = self.typ(x);
                    let c_t = self.child(t, a.len() + i);
                    self.unify(x_t, c_t, x.at());
                }
                t
            }
            rst::Typ::Known(i) => {
                self.unify_explicit(Key::i(*i), Variant::Newtype(i.0), i.1);
                Key::i(*i)
            }
            rst::Typ::Var(i) => Key::i(*i),
            rst::Typ::Foreign(_) => unreachable!(),
        }
    }

    fn unifys(&mut self, tru_stack: Vec<Key>, fal_stack: Vec<Key>, at: Span) -> Vec<Key> {
        assert_eq!(tru_stack.len(), fal_stack.len());
        tru_stack
            .into_iter()
            .zip(fal_stack.into_iter())
            .map(|(a, b)| self.unify(a, b, at))
            .collect()
    }

    fn unify(&mut self, a: Key, b: Key, at: Span) -> Key {
        let a = self.deref(a);
        let b = self.deref(b);
        if a == b {
            return a;
        }

        let mut a_v = if let Vertex::Full(x) = self.replace(a, Vertex::Fail) {
            x
        } else {
            return b;
        };
        let mut b_v = if let Vertex::Full(x) = self.replace(b, Vertex::Fail) {
            x
        } else {
            return a;
        };

        let variant = match a_v.variant.clone().meet(b_v.variant) {
            Ok(v) => v,
            Err(e) => {
                self.error(e);
                a_v.variant
            }
        };
        let poi = a_v
            .poi
            .into_iter()
            .chain(b_v.poi.into_iter())
            .chain([Poi::Unify(a, b, at)])
            .collect();
        if let Some(arity) = variant.arity() {
            assert!(a_v.children.len() <= arity);
            assert!(b_v.children.len() <= arity);
            a_v.children.resize(arity, None);
            b_v.children.resize(arity, None);
        };

        let l = a_v.children.len().max(b_v.children.len());
        let children: Vec<_> = a_v
            .children
            .into_iter()
            .chain([None].repeat(l))
            .take(l)
            .zip(b_v.children.into_iter().chain([None].repeat(l)).take(l))
            .map(|(x, y)| x.zip(y).map(|(x, y)| self.unify(x, y, at)))
            .collect();
        let bounds: Vec<_> = a_v
            .bounds
            .into_iter()
            .chain(b_v.bounds.into_iter())
            .collect();

        self.replace(b, Vertex::Fwd(a));
        self.replace(
            a,
            Vertex::Full(FullVertex {
                variant,
                poi,
                children,
                bounds,
            }),
        );
        a
    }

    fn unify_explicit(&mut self, a: Key, v: Variant, at: Span) {
        let a = self.deref(a);

        let mut a_v = if let Vertex::Full(x) = self.replace(a, Vertex::Fail) {
            x
        } else {
            return;
        };

        a_v.variant = match a_v.variant.clone().meet(v) {
            Ok(v) => v,
            Err(e) => {
                self.error(e);
                a_v.variant
            }
        };
        a_v.poi.push(Poi::Explicit(a, v, at));
        if let Some(arity) = a_v.variant.arity() {
            assert!(a_v.children.len() <= arity);
            a_v.children.resize(arity, None);
        };

        self.replace(a, Vertex::Full(a_v));
    }

    fn gen(&mut self, at: Span) -> Key {
        let k = Key(self.0.len());
        self.0.push(Vertex::Full(FullVertex::unknown(k.0, at)));
        k
    }

    fn child(&mut self, t: Key, i: usize) -> Key {
        let full = if let Some(full) = self.resolve(t) {
            full
        } else {
            return t;
        };
        let arity = if let Some(arity) = full.variant.arity() {
            arity
        } else {
            self.error(Terror::Generic("No arrity when getting child"));
            return t;
        };
        assert!(i < arity, "{} < {}: {}", i, arity, self.show(t));
        assert!(full.children.len() <= arity);
        self.resolve_mut(t).unwrap().children.resize(arity, None);
        let full = self.resolve(t).unwrap();
        match full.children[i] {
            Some(k) => k,
            None => {
                let k = self.gen(Span::zero());
                self.resolve_mut(t).unwrap().children[i] = Some(k);
                k
            }
        }
    }

    fn deref(&self, t: Key) -> Key {
        match &self.0[t.0] {
            Vertex::Full(_) => t,
            Vertex::Fail => t,
            Vertex::Fwd(k) => self.deref(*k),
        }
    }

    fn resolve_mut(&mut self, t: Key) -> Option<&mut FullVertex> {
        let t = self.deref(t);
        match &mut self.0[t.0] {
            Vertex::Full(f) => Some(f),
            Vertex::Fail => None,
            Vertex::Fwd(_) => unreachable!(),
        }
    }

    fn resolve(&self, t: Key) -> Option<&FullVertex> {
        let t = self.deref(t);
        match &self.0[t.0] {
            Vertex::Full(f) => Some(f),
            Vertex::Fail => None,
            Vertex::Fwd(_) => unreachable!(),
        }
    }

    fn replace(&mut self, a: Key, replace: Vertex) -> Vertex {
        std::mem::replace(&mut self.0[a.0], replace)
    }

    fn show(&self, a: Key) -> String {
        let a = self.deref(a);
        match self.resolve(a) {
            None => "Error".into(),
            Some(v) => match v.variant {
                Variant::Unknown => format!("t{}", a.0),
                Variant::Newtype(t) => format!("{}", self.2[t].0),
                Variant::Function(a, _) => {
                    let lhs = v
                        .children
                        .iter()
                        .take(a)
                        .map(|x| x.map(|x| self.show(x)).unwrap_or("?".into()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let rhs = v
                        .children
                        .iter()
                        .skip(a)
                        .map(|x| x.map(|x| self.show(x)).unwrap_or("?".into()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("({} -> {})", lhs, rhs)
                }
                Variant::Type => format!("Type"),
            },
        }
    }

    fn name(&self, name: rst::I) -> &'s str {
        self.2[name.0].0
    }
}

pub(crate) fn check<'s>(rst: &'s crate::rst::Rst, mapping: &'s [(&'s str, Span)]) -> Graph<'s> {
    let mut graph = Graph::new(mapping);
    graph.rst(rst);
    graph
}
