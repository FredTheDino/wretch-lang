use std::io::{Error, Write};

use crate::{ast::Span, rst};

struct Gen<'s>(Box<dyn Write>, Vec<(&'s str, Span)>, usize);

impl<'s> Gen<'s> {
    fn name(&self, i: &rst::I) -> &'s str {
        self.1[i.0].0
    }

    fn rst(&mut self, rst: rst::Rst) -> Result<(), Error> {
        let s = include_str!("preamble.lua");
        writeln!(self.0, "{}", s)?;
        writeln!(self.0, "")?;
        writeln!(self.0, "-- mod {}", self.name(&rst.0))?;
        self.indent();
        for decl in rst.1.iter() {
            self.decl(decl)?;
        }
        self.dedent();
        Ok(())
    }

    fn decl(&mut self, decl: &rst::Decl) -> Result<(), Error> {
        match decl {
            rst::Decl::DefExpr(_, _, _, n, expr) if expr.is_foreign() => {
                writeln!(self.0, "-- foreign {}", self.name(n))?;
                writeln!(self.0, "local _{} = {}", n.0, self.name(n))?;
            }
            rst::Decl::DefExpr(_, _, _, n, expr) => {
                writeln!(self.0, "-- def {}", self.name(n))?;
                write!(self.0, "local function _{}(s) ", n.0)?;
                self.expr(expr)?;
                writeln!(self.0, "end")?;

                if self.name(n) == "main" {
                    writeln!(self.0, "-- ENTRY POINT")?;
                    writeln!(self.0, "_{}({{}})", n.0)?;
                }
            }
            rst::Decl::Typ(_, _, _, _, _) | rst::Decl::Dat(_, _, _, _, _) => {}
        }
        Ok(())
    }

    fn expr(&mut self, expr: &rst::Expr) -> Result<(), Error> {
        match expr {
            rst::Expr::Chain(chain) => {
                writeln!(self.0, "")?;
                self.i()?;
                writeln!(self.0, "(function() ")?;
                self.indent();
                for c in chain.iter() {
                    self.i()?;
                    self.expr(c)?;
                    writeln!(self.0, "")?;
                }
                self.dedent();
                self.i()?;
                writeln!(self.0, "end)()")?;
            }
            rst::Expr::Group(_, expr, _) => self.expr(expr)?,
            rst::Expr::Ident(i) => write!(self.0, "_{}(s)", i.0)?,
            rst::Expr::Int(n) => write!(self.0, "_push(s, {})", n)?,
            rst::Expr::If(_, c, tru, fal) => {
                write!(self.0, "(function() if ")?;
                self.expr(c)?;
                write!(self.0, "then return ")?;
                self.expr(tru)?;
                write!(self.0, "else return ")?;
                self.expr(fal)?;
                write!(self.0, "end end)()")?;
            }
            rst::Expr::Foreign(_) => unreachable!(),
        };
        Ok(())
    }

    fn i(&mut self) -> Result<(), Error> {
        for _ in 0..self.2 {
            write!(self.0, "  ")?;
        }
        Ok(())
    }

    fn indent(&mut self) {
        self.2 += 1;
    }
    fn dedent(&mut self) {
        self.2 -= 1;
    }
}

pub(crate) fn gen<'s>(
    f: std::fs::File,
    rst: crate::rst::Rst,
    mapping: Vec<(&'s str, Span)>,
) -> Result<(), Error> {
    Gen(Box::new(f), mapping, 0).rst(rst)
}
