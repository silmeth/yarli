use super::ast::{Expr, Value, UnOperator, BiOperator, Stmt};
use super::ast::Expr::*;
use super::Lox;

type EvaluateResult = Result<Value, RuntimeError>;
type InterpretResult = Result<(), EarlyExit>;

pub fn interpret(stmts: Vec<Stmt>, lox: &mut Lox) {
    for stmt in stmts {
        if let Err(exit) = interpret_stmt(stmt) {
            if let EarlyExit::Error(err) = exit {
                lox.runtime_error(err);
            }
            break;
        }
    }
}

fn evaluate(expr: Expr) -> EvaluateResult {
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
        },
        Variable(s) => unimplemented!(),
    };

    Ok(res)
}

fn interpret_stmt(stmt: Stmt) -> InterpretResult {
    match stmt {
        Stmt::Expression(expr) => { evaluate(expr)?; },
        Stmt::Print(expr) => println!("{}", evaluate(expr)?),
        Stmt::Var { .. } => unimplemented!(),
    }

    Ok(())
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

#[derive(Debug, Fail)]
pub enum RuntimeError {
    #[fail(display = "Type error: expected {}, got {:?}.", expected, got)]
    TypeError { expected: &'static str, got: Value }
}

// Hack for later to easily return from function calls on `return` statement using `?` without explicit matching.
// Like using overridden stack-trace-less exceptions in Java.
pub enum EarlyExit {
    Error(RuntimeError),
    Return(Value),
}

impl From<RuntimeError> for EarlyExit {
    fn from(err: RuntimeError) -> Self {
        EarlyExit::Error(err)
    }
}
