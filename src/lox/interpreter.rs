use std::collections::HashMap;
use std::rc::Rc;

use super::ast::{Expr, Value, UnOperator, BiOperator, LogicOperator, Stmt};
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
            environment: Environment::new_global(),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>, lox: &mut Lox) {
        for stmt in stmts {
            if let Err(exit) = self.interpret_stmt(&stmt) {
                if let EarlyExit::Error(err) = exit {
                    lox.runtime_error(err);
                }
                break;
            }
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> EvaluateResult {
        let res = match *expr {
            Literal(ref value) => value.clone(),
            Unary { ref op, ref rh } => match *op {
                UnOperator::Not => Value::Boolean(!is_truthy(&self.evaluate(&*rh)?)),
                UnOperator::Minus => Value::Number(-expect_number(&self.evaluate(&*rh)?)?),
            },
            Binary { ref lh, ref op, ref rh } => {
                let lh = self.evaluate(&*lh)?;
                let rh = self.evaluate(&*rh)?;

                match *op {
                    BiOperator::Minus => Value::Number(expect_number(&lh)? - expect_number(&rh)?),
                    BiOperator::Div => Value::Number(expect_number(&lh)? / expect_number(&rh)?),
                    BiOperator::Mul => Value::Number(expect_number(&lh)? * expect_number(&rh)?),
                    BiOperator::Plus => match (lh, rh) {
                        (Value::Number(lh), Value::Number(rh)) => Value::Number(lh + rh),
                        (Value::String(lh), Value::String(rh)) => Value::String(Rc::from((*lh).to_owned() + &*rh)),
                        (Value::Number(_), rh) => return Err(RuntimeError::type_error("number", &rh)),
                        (Value::String(_), rh) => return Err(RuntimeError::type_error("string", &rh)),
                        (lh, _) => return Err(RuntimeError::type_error("number or string", &lh)),
                    },
                    BiOperator::Gt => Value::Boolean(expect_number(&lh)? > expect_number(&rh)?),
                    BiOperator::GtEq => Value::Boolean(expect_number(&lh)? >= expect_number(&rh)?),
                    BiOperator::Lt => Value::Boolean(expect_number(&lh)? < expect_number(&rh)?),
                    BiOperator::LtEq => Value::Boolean(expect_number(&lh)? <= expect_number(&rh)?),
                    BiOperator::Eq => Value::Boolean(lh == rh),
                    BiOperator::NotEq => Value::Boolean(lh != rh),
                }
            }
            Variable(ref s) => {
                self.environment.get(s)?
            }
            Assign { ref name, ref value } => {
                let value = self.evaluate(&*value)?;
                self.environment.assign(name.to_owned(), value.clone())?;
                value
            }
            Logic { ref lh, ref op, ref rh } => {
                let lh = self.evaluate(&*lh)?;

                match *op {
                    LogicOperator::Or if is_truthy(&lh) => lh,
                    LogicOperator::And if !is_truthy(&lh) => lh,
                    _ => self.evaluate(&*rh)?,
                }
            }
        };

        Ok(res)
    }

    fn interpret_stmt(&mut self, stmt: &Stmt) -> InterpretResult {
        match *stmt {
            Stmt::Expression(ref expr) => { self.evaluate(expr)?; }
            Stmt::Print(ref expr) => println!("{}", self.evaluate(expr)?),
            Stmt::Var { ref name, ref initializer } => {
                let value = self.evaluate(initializer)?;
                self.environment.define(name.to_owned(), value);
            }
            Stmt::Block(ref stmts) => self.execute_block(stmts)?,
            Stmt::If { ref condition, ref then_branch, ref else_branch } => {
                if is_truthy(&self.evaluate(condition)?) {
                    self.interpret_stmt(&*then_branch)?;
                } else if let Some(ref else_branch) = *else_branch {
                    self.interpret_stmt(&*else_branch)?;
                }
            }
            Stmt::While { ref condition, ref body } => {
                while is_truthy(&self.evaluate(condition)?) {
                    self.interpret_stmt(&*body)?;
                }
            }
        }

        Ok(())
    }

    fn execute_block(&mut self, stmts: &[Stmt]) -> InterpretResult {
        self.environment.push_new_local();

        let mut res = Ok(());
        for stmt in stmts {
            res = self.interpret_stmt(stmt);

            if res.is_err() {
                break;
            }
        }

        // We are at the end of a block, there must be a parent environment.
        self.environment.pop_local().unwrap();

        res
    }
}

struct Environment {
    values: HashMap<String, Value>,
    // Perhaps having a Option<&mut> ref would be better here,
    // then environment would need to be decoupled from interpreter and always passed by ref.
    // This solution is more comparable to jlox though.
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    fn new_global() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    fn push_new_local(&mut self) {
        use std::mem::replace;

        let parent_values = replace(&mut self.values, HashMap::new());
        let parent_enclosing = self.enclosing.take();

        self.enclosing = Some(Box::new(Environment {
            values: parent_values,
            enclosing: parent_enclosing,
        }))
    }

    fn pop_local(&mut self) -> Option<()> {
        match self.enclosing.take() {
            Some(parent) => {
                *self = *parent;
                Some(())
            }
            _ => None
        }
    }

    fn define(&mut self, name: String, value: Value) {
        let _ = self.values.insert(name, value);
    }

    fn assign(&mut self, name: String, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name) {
            let _ = self.values.insert(name, value);
            Ok(())
        } else {
            match self.enclosing {
                Some(ref mut enclosing) => {
                    enclosing.assign(name, value)?;
                    Ok(())
                }
                None => Err(RuntimeError::UndefinedError { name: name.to_owned() })
            }
        }
    }

    fn get(&self, name: &str) -> EvaluateResult {
        match self.values.get(name) {
            Some(val) => Ok(val.clone()),
            None => match self.enclosing {
                Some(ref enclosing) => Ok(enclosing.get(name)?),
                None => Err(RuntimeError::UndefinedError { name: name.to_owned() })
            },
        }
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
fn expect_number(val: &Value) -> Result<f64, RuntimeError> {
    match *val {
        Value::Number(num) => Ok(num),
        _ => Err(RuntimeError::type_error("number", val)),
    }
}

#[derive(Debug, Fail)]
pub enum RuntimeError {
    #[fail(display = "Type error: expected {} value, got {} ({}).", expected, value, type_name)]
    TypeError { expected: &'static str, type_name: &'static str, value: String },
    #[fail(display = "Undefined variable {}.", name)]
    UndefinedError { name: String },
}

impl RuntimeError {
    fn type_error(expected: &'static str, value: &Value) -> RuntimeError {
        RuntimeError::TypeError {
            expected,
            type_name: value.type_name(),
            value: value.to_string(),
        }
    }
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
