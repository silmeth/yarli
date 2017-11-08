use std::fmt;
use std::error;

use super::ast::{Expr, Value, UnOperator, BiOperator};
use super::ast::Expr::*;
use super::Lox;

pub fn interpret(expr: Expr, lox: &mut Lox) {
    match evaluate(expr) {
        Ok(val) => println!("{}", val),
        Err(e) => lox.runtime_error(e)
    }
}

fn evaluate(expr: Expr) -> Result<Value, RuntimeError> {
    let res = match expr {
        Literal(value) => value,
        Unary { op, rh } => match op {
            UnOperator::Not => Value::Boolean(!is_truthy(&evaluate(*rh)?)),
            UnOperator::Minus => Value::Number(-expect_number(evaluate(*rh)?)?),
        },
        Binary { lh, op, rh } => {
            let lh = evaluate(*lh)?;
            let rh = evaluate(*rh)?;

            match op {
                BiOperator::Minus => Value::Number(expect_number(lh)? - expect_number(rh)?),
                BiOperator::Div => Value::Number(expect_number(lh)? / expect_number(rh)?),
                BiOperator::Mul => Value::Number(expect_number(lh)? * expect_number(rh)?),
                BiOperator::Plus => match (lh, rh) {
                    (Value::Number(lh), Value::Number(rh)) => Value::Number(lh + rh),
                    (Value::String(lh), Value::String(rh)) => Value::String(lh + &rh),
                    (Value::Number(_), rh) => return Err(RuntimeError::TypeError { expected: "number", got: rh }),
                    (Value::String(_), rh) => return Err(RuntimeError::TypeError { expected: "string", got: rh }),
                    (lh, _) => return Err(RuntimeError::TypeError { expected: "number or string", got: lh }),
                },
                BiOperator::Gt => Value::Boolean(expect_number(lh)? > expect_number(rh)?),
                BiOperator::GtEq => Value::Boolean(expect_number(lh)? >= expect_number(rh)?),
                BiOperator::Lt => Value::Boolean(expect_number(lh)? < expect_number(rh)?),
                BiOperator::LtEq => Value::Boolean(expect_number(lh)? <= expect_number(rh)?),
                BiOperator::Eq => Value::Boolean(lh == rh),
                BiOperator::NotEq => Value::Boolean(lh != rh),
            }
        }
    };

    Ok(res)
}

#[inline]
fn is_truthy(val: &Value) -> bool {
    match *val {
        Value::Boolean(false) | Value::Nil => false,
        _ => true
    }
}

#[inline]
fn expect_number(val: Value) -> Result<f64, RuntimeError> {
    match val {
        Value::Number(num) => Ok(num),
        _ => Err(RuntimeError::TypeError { expected: "number", got: val }),
    }
}

// TODO: use `failure` crate for errors
#[derive(Debug, Clone)]
pub enum RuntimeError {
    TypeError { expected: &'static str, got: Value }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RuntimeError::TypeError{ expected, ref got } =>
                write!(f, "Type error: expected {}, got {:?}.", expected, got),
        }
    }
}

impl error::Error for RuntimeError {
    fn description(&self) -> &str {
        "Runtime error."
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
