use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub enum Expr {
    Binary { lh: Box<Expr>, op: BiOperator, rh: Box<Expr> },
    Literal(Value),
    Unary { op: UnOperator, rh: Box<Expr> },
    Variable(Rc<str>),
    Assign { name: Rc<str>, value: Box<Expr> },
    Logic { lh: Box<Expr>, op: LogicOperator, rh: Box<Expr> },
    Call { callee: Box<Expr>, args: Vec<Expr> },
}

#[derive(Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Expr),
    Function { name: Rc<str>, parameters: Vec<Rc<str>>, body: Vec<Stmt> },
    If { condition: Expr, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>> },
    Print(Expr),
    Return(Expr),
    Var { name: Rc<str>, initializer: Expr },
    While { condition: Expr, body: Box<Stmt> },
}

#[derive(Copy, Clone)]
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

#[derive(Copy, Clone)]
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

#[derive(Copy, Clone)]
pub enum LogicOperator {
    Or,
    And,
}

impl fmt::Display for LogicOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LogicOperator::Or => write!(f, "or"),
            LogicOperator::And => write!(f, "and"),
        }
    }
}

use lox::interpreter::Callable;

#[derive(Clone)]
pub enum Value {
    String(Rc<str>),
    Number(f64),
    Boolean(bool),
    Nil,
    Function(Rc<Callable>),
    // Object(Rc<RefCell<ObjectStruct>>),
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        use std::ptr;
        use std::borrow::Borrow;

        match (self, other) {
            (&Value::String(ref lh), &Value::String(ref rh)) => lh == rh,
            (&Value::Number(lh), &Value::Number(rh)) => lh == rh,
            (&Value::Boolean(lh), &Value::Boolean(rh)) => lh == rh,
            (&Value::Nil, &Value::Nil) => true,
            (&Value::Function(ref lh), &Value::Function(ref rh)) => ptr::eq::<Callable>(lh.borrow(), rh.borrow()),
            _ => false,
        }
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match *self {
            Value::String(_) => "string",
            Value::Number(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::Nil => "nil",
            Value::Function(_) => "function",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::String(ref s) => write!(f, "{}", s),
            Value::Number(ref num) => write!(f, "{}", num),
            Value::Boolean(ref b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Function(ref func) => write!(f, "<fn {}>", func.name())
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
                Value::String(ref s) => (**s).to_owned(),
                Value::Boolean(b) => format!("{}", b),
                Value::Function(ref func) => format!("func {}", func.name()),
                // ref obj @ Value::Object() => format!("{:?}", obj),
            },
            Unary { ref op, ref rh } => parenthesize(&format!("{}", op), &[rh]),
            Variable(ref s) => format!("var {}", s),
            Assign { ref name, ref value } => format!("assignment {} = {}", name, print_ast(value)),
            Logic { ref lh, ref op, ref rh } => parenthesize(&format!("{}", op), &[lh, rh]),
            Call { ref callee, ref args } => format!("call {}({})",
                                                     print_ast(callee),
                                                     args.iter().map(|it| print_ast(it))
                                                         .collect::<Vec<_>>()
                                                         .join(", "))
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
