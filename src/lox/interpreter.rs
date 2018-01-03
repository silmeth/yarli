use std::collections::HashMap;

use super::ast::{Expr, Value, UnOperator, BiOperator, Stmt};
use super::ast::Expr::*;
use super::Lox;

type EvaluateResult = Result<Value, RuntimeError>;
type InterpretResult = Result<(), EarlyExit>;


pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new()
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>, lox: &mut Lox) {
        for stmt in stmts {
            if let Err(exit) = self.interpret_stmt(stmt) {
                if let EarlyExit::Error(err) = exit {
                    lox.runtime_error(err);
                }
                break;
            }
        }
    }

    fn evaluate(&self, expr: Expr) -> EvaluateResult {
        let res = match expr {
            Literal(value) => value,
            Unary { op, rh } => match op {
                UnOperator::Not => Value::Boolean(!is_truthy(&self.evaluate(*rh)?)),
                UnOperator::Minus => Value::Number(-expect_number(self.evaluate(*rh)?)?),
            },
            Binary { lh, op, rh } => {
                let lh = self.evaluate(*lh)?;
                let rh = self.evaluate(*rh)?;

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
            Variable(s) => {
                self.environment.get(&s)?
            },
        };

        Ok(res)
    }

    fn interpret_stmt(&mut self, stmt: Stmt) -> InterpretResult {
        match stmt {
            Stmt::Expression(expr) => { self.evaluate(expr)?; },
            Stmt::Print(expr) => println!("{}", self.evaluate(expr)?),
            Stmt::Var { name, initializer } => {
                let value = self.evaluate(initializer)?;
                self.environment.define(name, value);
            },
        }

        Ok(())
    }
}

struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            values: HashMap::new()
        }
    }

    fn define(&mut self, name: String, value: Value) {
        let _ = self.values.insert(name, value);
    }

    // TODO: rethink this. This always copies a value.
    // * might be expensive for strings,
    // * makes it impossible to share two references to one object
    //   (is that correct behaviour for mutable objects?)
    // Perhaps making EvaluateResult = Result<Rc<Value>, RuntimeError> would be OK.
    fn get(&self, name: &str) -> EvaluateResult {
        self.values.get(name)
            .cloned()
            .ok_or_else(|| RuntimeError::UndefinedError { name: name.to_owned() })
    }
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
    TypeError { expected: &'static str, got: Value },
    #[fail(display = "Undefined variable {}.", name)]
    UndefinedError { name: String },
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
