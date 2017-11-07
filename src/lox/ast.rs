use std::fmt;

use super::token::Token;

pub enum Expr {
    Binary { lh: Box<Expr>, op: Token, rh: Box<Expr> },
    Grouping(Box<Expr>),
    Literal(Value),
    Unary { op: Token, rh: Box<Expr> },
}

#[derive(PartialEq,Debug,Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
    // Object(),
    // Function(),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::String(ref s) => write!(f, "{}", s),
            Value::Number(ref num) => write!(f, "{}", num),
            Value::Boolean(ref b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            // Value::Object() => write!(f, "object"),
        }
    }
}

pub mod printer {
    use super::{Expr, Value};
    use super::Expr::*;

    pub fn print_ast(expr: &Expr) -> String {
        match *expr {
            Binary { ref lh, ref op, ref rh } => parenthesize(&format!("{}", op), &[lh, rh]),
            Grouping(ref expr) => parenthesize("group", &[expr]),
            Literal(ref value) => match *value {
                Value::Nil => String::from("nil"),
                Value::Number(num) => format!("{}", num),
                Value::String(ref s) => s.clone(),
                Value::Boolean(b) => format!("{}", b),
                // ref obj @ Value::Object() => format!("{:?}", obj),
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
