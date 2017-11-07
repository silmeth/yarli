use std::fmt;
use std::error;

use super::token::Token;
use super::ast::Expr;
use super::ast::Expr::*;
use super::ast::Value;
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
        Grouping(e) => evaluate(*e)?,
        Unary { op, rh } => match op {
            Token::Bang => Value::Boolean(!is_truthy(&evaluate(*rh)?)),
            Token::Minus => Value::Number(-expect_number(evaluate(*rh)?)?),
            _ => unreachable!() // TODO: move op to separate enum, so that this is unneeded and statically enforced
        },
        Binary { lh, op, rh } => {
            let lh = evaluate(*lh)?;
            let rh = evaluate(*rh)?;

            match op {
                Token::Minus => Value::Number(expect_number(lh)? - expect_number(rh)?),
                Token::Slash => Value::Number(expect_number(lh)? / expect_number(rh)?),
                Token::Star => Value::Number(expect_number(lh)? * expect_number(rh)?),
                Token::Plus => match (lh, rh) {
                    (Value::Number(lh), Value::Number(rh)) => Value::Number(lh + rh),
                    (Value::String(lh), Value::String(rh)) => Value::String(lh + &rh),
                    (Value::Number(_), rh) => return Err(RuntimeError::TypeError { expected: "number", got: rh }),
                    (Value::String(_), rh) => return Err(RuntimeError::TypeError { expected: "string", got: rh }),
                    (lh, _) => return Err(RuntimeError::TypeError { expected: "number or string", got: lh }),
                },
                Token::Greater => Value::Boolean(expect_number(lh)? > expect_number(rh)?),
                Token::GreaterEqual => Value::Boolean(expect_number(lh)? >= expect_number(rh)?),
                Token::Less => Value::Boolean(expect_number(lh)? < expect_number(rh)?),
                Token::LessEqual => Value::Boolean(expect_number(lh)? <= expect_number(rh)?),
                Token::EqualEqual => Value::Boolean(lh == rh),
                Token::BangEqual => Value::Boolean(lh != rh),
                _ => unreachable!() // TODO: separate op enum, like in Unary
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
