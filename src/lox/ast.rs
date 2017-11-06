use super::token::Token;

pub enum Expr {
    Binary { lh: Box<Expr>, op: Token, rh: Box<Expr> },
    Grouping(Box<Expr>),
    Literal(Primitive),
    Unary { op: Token, rh: Box<Expr> },
}

pub enum Primitive {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

pub mod printer {
    use super::{Expr, Primitive};
    use super::Expr::*;

    pub fn print_ast(expr: &Expr) -> String {
        match *expr {
            Binary { ref lh, ref op, ref rh } => parenthesize(&format!("{}", op), &[lh, rh]),
            Grouping(ref expr) => parenthesize("group", &[expr]),
            Literal(ref value) => match *value {
                Primitive::Nil => String::from("nil"),
                Primitive::Number(num) => format!("{}", num),
                Primitive::String(ref s) => s.clone(),
                Primitive::Boolean(b) => format!("{}", b),
            },
            Unary { ref op, ref rh } => parenthesize(&format!("{}", op), &[rh])
        }
    }

    fn parenthesize(name: &str, exprs: &[&Expr]) -> String {
        let mut res = format!("({}", name);

        for expr in exprs {
            res += &format!(" {}", print_ast(expr))
        }

        res += ")";
        res
    }
}
