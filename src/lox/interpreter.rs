extern crate chrono;

use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use std::cell::RefCell;

use super::ast::{Expr, Value, UnOperator, BiOperator, LogicOperator, Stmt};
use super::ast::Expr::*;
use super::Lox;

type EvaluateResult = Result<Value, RuntimeError>;
type InterpretResult = Result<(), EarlyExit>;

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        use self::chrono::Utc;

        let globals = Rc::new(RefCell::new(Environment::new_global()));

        let clock = Native {
            name: Rc::from("clock"),
            callable: |_, _| {
                let time = Utc::now();
                let millis = time.timestamp() * 1000 + i64::from(time.timestamp_subsec_millis());

                Err(EarlyExit::Return(Value::Number(millis as f64 / 1000.0)))
            },
            arity: 0,
        };
        globals.borrow_mut().define((*clock.name).to_owned(), Value::Function(Rc::new(clock)));
        let environment = Rc::clone(&globals);

        Interpreter { globals, environment }
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
                self.environment.borrow().get(s)?
            }
            Assign { ref name, ref value } => {
                let value = self.evaluate(&*value)?;
                self.environment.borrow_mut().assign(name.to_owned(), value.clone())?;
                value
            }
            Logic { ref lh, ref op, ref rh } => {
                let lh = self.evaluate(&*lh)?;

                match *op {
                    LogicOperator::Or if is_truthy(&lh) => lh,
                    LogicOperator::And if !is_truthy(&lh) => lh,
                    _ => self.evaluate(&*rh)?,
                }
            },
            Call { ref callee, ref args } => {
                let callee = self.evaluate(callee)?;
                let args = args.iter().map(|arg| self.evaluate(arg)).collect::<Result<Vec<_>, _>>()?;

                let function = callee.into_callable()?;
                let arity = function.arity();

                if args.len() != (arity as usize) {
                    return Err(RuntimeError::WrongArity { expected: arity, got: args.len() })
                }

                match function.call(self, &args) {
                    Ok(()) => Value::Nil,
                    Err(EarlyExit::Return(val)) => val,
                    Err(EarlyExit::Error(err)) => return Err(err),
                }
            },
        };

        Ok(res)
    }

    fn interpret_stmt(&mut self, stmt: &Stmt) -> InterpretResult {
        match *stmt {
            Stmt::Expression(ref expr) => { self.evaluate(expr)?; }
            Stmt::Print(ref expr) => println!("{}", self.evaluate(expr)?),
            Stmt::Var { ref name, ref initializer } => {
                let value = self.evaluate(initializer)?;
                self.environment.borrow_mut().define(name.to_owned(), value);
            }
            Stmt::Block(ref stmts) => {
                let local = Rc::new(RefCell::new(Environment::new_local(Rc::clone(&self.environment))));
                self.execute_block(stmts, local)?
            },
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
            Stmt::Function { ref name, ref parameters, ref body } => {
                let function = Value::Function(Rc::new(LoxFunction {
                    name: Rc::from(name.to_owned()),
                    parameters: parameters.iter().map(|it| { it.to_owned() }).collect(),
                    stmts: body.to_vec()
                }));

                self.environment.borrow_mut().define(name.to_owned(), function);
            }
        }

        Ok(())
    }

    fn execute_block(&mut self, stmts: &[Stmt], environment: Rc<RefCell<Environment>>) -> InterpretResult {
        use std::mem;
        let previous = mem::replace(&mut self.environment, environment);

        let mut res = Ok(());
        for stmt in stmts {
            res = self.interpret_stmt(stmt);

            if res.is_err() {
                break;
            }
        }

        self.environment = previous;

        res
    }
}

struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn new_global() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    fn new_local(parent: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(parent),
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
                    enclosing.borrow_mut().assign(name, value)?;
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
                Some(ref enclosing) => Ok(enclosing.borrow().get(name)?),
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

pub trait Callable {
    fn arity(&self) -> u8;
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> InterpretResult;
    fn name(&self) -> Rc<str>;
}

impl <D, C: Callable + ?Sized> Callable for D where D: Deref<Target = C> {
    fn arity(&self) -> u8 {
        self.deref().arity()
    }

    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> InterpretResult {
        self.deref().call(interpreter, args)
    }

    fn name(&self) -> Rc<str> {
        self.deref().name()
    }
}

trait IntoCallable {
    type C: Callable;
    fn into_callable(self) -> Result<Self::C, RuntimeError>;
}

impl IntoCallable for Value {
    type C = Rc<Callable>;

    fn into_callable(self) -> Result<Self::C, RuntimeError> {
        match self {
            Value::Function(f) => Ok(f),
            _ => Err(RuntimeError::not_callable(&self))
        }
    }
}

struct Native<F> where for<'a, 'b> F: Fn(&'a mut Interpreter, &'b [Value]) -> InterpretResult {
    name: Rc<str>,
    arity: u8,
    callable: F,
}

impl <F> Callable for Native<F> where for<'a, 'b> F: Fn(&'a mut Interpreter, &'b [Value]) -> InterpretResult {
    fn arity(&self) -> u8 {
        self.arity
    }

    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> InterpretResult {
        (self.callable)(interpreter, args)
    }

    fn name(&self) -> Rc<str> {
        Rc::clone(&self.name)
    }
}

struct LoxFunction {
    name: Rc<str>,
    parameters: Vec<String>,
    stmts: Vec<Stmt>,
}

impl Callable for LoxFunction {
    fn arity(&self) -> u8 {
        self.parameters.len() as u8
    }

    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> InterpretResult {
        let mut environment = Environment::new_local(Rc::clone(&interpreter.globals));

        for (param, arg) in self.parameters.iter().zip(args) {
            environment.define(param.to_owned(), arg.clone());
        }

        interpreter.execute_block(&*self.stmts, Rc::new(RefCell::new(environment)))
    }
    fn name(&self) -> Rc<str> {
        Rc::clone(&self.name)
    }
}

#[derive(Debug, Fail)]
pub enum RuntimeError {
    #[fail(display = "Type error: expected {} value, got {} ({}).", expected, value, type_name)]
    TypeError { expected: &'static str, type_name: &'static str, value: String },
    #[fail(display = "Undefined variable {}.", name)]
    UndefinedError { name: String },
    #[fail(display = "Can only call callable objects. Got not callable: {}.", value)]
    NotCallable { value: String },
    #[fail(display = "Expected {} arguments but got {}.", expected, got)]
    WrongArity { expected: u8, got: usize },
}

impl RuntimeError {
    fn type_error(expected: &'static str, value: &Value) -> RuntimeError {
        RuntimeError::TypeError {
            expected,
            type_name: value.type_name(),
            value: value.to_string(),
        }
    }

    fn not_callable(value: &Value) -> RuntimeError {
        RuntimeError::NotCallable {
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
