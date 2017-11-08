use std::fmt;

pub enum Expr {
    Binary { lh: Box<Expr>, op: BiOperator, rh: Box<Expr> },
    Literal(Value),
    Unary { op: UnOperator, rh: Box<Expr> },
}

pub enum UnOperator {
    Not,
    Minus,
}

impl fmt::Display for UnOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnOperator::Not => write!(f, "!"),
            UnOperator::Minus => write!(f, "-"),
        }
    }
}

pub enum BiOperator {
    Plus,
    Minus,
    Mul,
    Div,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Eq,
    NotEq,
}

impl fmt::Display for BiOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BiOperator::Plus => write!(f, "+"),
            BiOperator::Minus => write!(f, "-"),
            BiOperator::Mul => write!(f, "*"),
            BiOperator::Div => write!(f, "/"),
            BiOperator::Gt => write!(f, ">"),
            BiOperator::GtEq => write!(f, ">="),
            BiOperator::Lt => write!(f, "<"),
            BiOperator::LtEq => write!(f, "<="),
            BiOperator::Eq => write!(f, "=="),
            BiOperator::NotEq => write!(f, "!="),
        }
    }
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
