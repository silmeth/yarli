use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;

use super::Lox;
use super::token::{TokenContext, Token};
use super::token::Token::*;
use super::ast::{Expr, Value, UnOperator, BiOperator, LogicOperator, Stmt};
use super::ast::Expr::*;

pub fn parse(tokens: Vec<TokenContext>, lox: &mut Lox) -> Vec<Stmt> {
    let mut state = ParserState::new(tokens, lox);
    let mut stmts = Vec::new();
    while state.peek() != Some(&Eof) && state.peek() != None {
        if let Ok(stmt) = state.declaration() {
            stmts.push(stmt)
        };
    }
    stmts
}

struct ParserState<'a> {
    tokens: Peekable<IntoIter<TokenContext>>,
    lox: &'a mut Lox,
}

impl<'a> ParserState<'a> {
    fn new(tokens: Vec<TokenContext>, lox: &mut Lox) -> ParserState {
        ParserState {
            tokens: tokens.into_iter().peekable(),
            lox,
        }
    }

    // expression → assignment
    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    // assignment → (identifier "=" assignment) | logicOr
    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.or()?;

        if self.match_next(&[Equal]).is_some() {
            let value = self.assignment()?;

            match expr {
                Variable(name) => Ok(Assign { name, value: Box::new(value) }),
                _ => Err(self.error("Invalid assignment target."))
            }
        } else {
            Ok(expr)
        }
    }

    // logicOr → logicAnd ("or" logicAnd)*
    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;

        while self.match_next(&[Or]).is_some() {
            let rh = self.and()?;
            expr = Logic { lh: Box::new(expr), op: LogicOperator::Or, rh: Box::new(rh) };
        }

        Ok(expr)
    }

    // logicAnd → equality ("and" equality)*
    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while self.match_next(&[And]).is_some() {
            let rh = self.equality()?;
            expr = Logic { lh: Box::new(expr), op: LogicOperator::And, rh: Box::new(rh) };
        }

        Ok(expr)
    }

    // equality → comparison (("==" | "!=") comparison)*
    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut res = self.comparison()?;

        while let Some(op_token) = self.match_next(&[EqualEqual, BangEqual]) {
            let rh = self.comparison()?;
            let op = if op_token == EqualEqual { BiOperator::Eq } else { BiOperator::NotEq };

            res = Binary { lh: Box::new(res), op, rh: Box::new(rh) }
        }

        Ok(res)
    }

    // comparison → addition ((">" | ">=" | "<" | "<=") addition)*
    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut res = self.addition()?;

        while let Some(op_token) = self.match_next(&[Greater, GreaterEqual, Less, LessEqual]) {
            let rh = self.addition()?;
            let op = match op_token {
                Greater => BiOperator::Gt,
                GreaterEqual => BiOperator::GtEq,
                Less => BiOperator::Lt,
                LessEqual => BiOperator::LtEq,
                _ => unreachable!(),
            };

            res = Binary { lh: Box::new(res), op, rh: Box::new(rh) }
        }

        Ok(res)
    }

    // addition → multiplication (("+" | "-") multiplication)*
    fn addition(&mut self) -> Result<Expr, ParseError> {
        let mut res = self.multiplication()?;

        while let Some(op_token) = self.match_next(&[Plus, Minus]) {
            let rh = self.multiplication()?;
            let op = if op_token == Plus { BiOperator::Plus } else { BiOperator::Minus };

            res = Binary { lh: Box::new(res), op, rh: Box::new(rh) }
        }

        Ok(res)
    }

    // multiplication → unary (("*" | "/") unary)*
    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut res = self.unary()?;

        while let Some(op_token) = self.match_next(&[Star, Slash]) {
            let rh = self.unary()?;
            let op = if op_token == Star { BiOperator::Mul } else { BiOperator::Div };

            res = Binary { lh: Box::new(res), op, rh: Box::new(rh) }
        }

        Ok(res)
    }

    // unary → (("!" | "-") unary) | call
    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op_token) = self.match_next(&[Bang, Minus]) {
            let rh = self.unary()?;
            let op = if op_token == Bang { UnOperator::Not } else { UnOperator::Minus };
            Ok(Unary { op, rh: Box::new(rh) })
        } else {
            self.call()
        }
    }

    // call → primary ("(" arguments? ")")*
    // arguments → expression ("," expression)*
    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        while self.match_next(&[LeftParen]).is_some() {
            expr = self.finish_call(expr)?;
        }

        Ok(expr)
    }

    #[inline]
    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut args = Vec::new();

        if self.match_next(&[RightParen]).is_none() {
            loop {
                if args.len() >= 8 {
                    self.error("Cannot have more than 8 arguments.");
                }

                args.push(self.expression()?);

                if self.match_next(&[Comma]).is_none() {
                    break;
                }
            }

            self.consume(&RightParen, "Expect ')' after arguments.")?;
        }

        Ok(Call { callee: Box::new(callee), args })
    }

    // primary → NUMBER | STRING | "false" | "true" | "nil" | identifier | "(" expr ")"
    fn primary(&mut self) -> Result<Expr, ParseError> {
        let res = if let Some(token) = self.advance() {
            match token {
                False => Literal(Value::Boolean(false)),
                True => Literal(Value::Boolean(true)),
                StringLit(s) => Literal(Value::String(Rc::from(s))),
                Number(num) => Literal(Value::Number(num)),
                Nil => Literal(Value::Nil),
                Identifier(s) => Variable(s),
                LeftParen => {
                    let expr = self.expression()?;
                    self.consume(&RightParen, "Expect ')' after grouped expression.")?;
                    expr
                }
                _ => return Err(self.error("Expect expression.")),
            }
        } else {
            // last index reached
            return Err(self.error("Expect expression."));
        };

        Ok(res)
    }

    // declaration → funDecl | varDecl | statement
    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        let res = match self.match_next(&[Var, Fun]) {
            Some(Fun) => self.function("function"),
            Some(Var) => self.var_declaration(),
            _ => self.statement(),
        };

        res.map_err(|e| {
            self.synchronize();
            e
        })
    }

    // funDecl → "fun" function
    // function → IDENTIFIER "(" parameters? ")" block
    fn function(&mut self, kind: &'static str) -> Result<Stmt, ParseError> {
        // "fun" already consumed
        let name = self.consume_identifier(&format!("Expect {} name.", kind))?;

        self.consume(&LeftParen, &format!("Expect '(' after {}.", kind))?;

        let mut parameters = Vec::new();
        if self.match_next(&[RightParen]).is_none() {
            loop {
                if parameters.len() >= 8 {
                    self.error("Cannot have more than 8 parameters.");
                }

                parameters.push(self.consume_identifier("Expect parameter name.")?);

                if self.match_next(&[Comma]).is_none() {
                    break;
                }
            }

            self.consume(&RightParen, "Expect ')' after parameters.")?;
        }

        self.consume(&LeftBrace, &format!("Expect '{{' before {} body.", kind))?;
        let body = self.block()?;
        Ok(Stmt::Function { name, parameters, body })
    }

    // varDecl → "var" identifier ("=" expression)? ";"
    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        // "var" already consumed
        let name = self.consume_identifier("Expect variable name.")?;

        let initializer = match self.match_next(&[Equal]) {
            Some(_) => self.expression()?,
            None => Literal(Value::Nil),
        };

        self.consume(&Semicolon, "Expect ';' after variable declaration.")?;

        Ok(Stmt::Var { name, initializer })
    }


    // statement → exprStmt | forStmt | ifStmt | printStmt | returnStmt | whileStmt | block
    fn statement(&mut self) -> Result<Stmt, ParseError> {
        match self.match_next(&[Print, For, If, LeftBrace, Return, While]) {
            Some(Print) => self.print_stmt(),
            Some(For) => self.for_stmt(),
            Some(If) => self.if_stmt(),
            Some(LeftBrace) => Ok(Stmt::Block(self.block()?)),
            Some(Return) => self.return_stmt(),
            Some(While) => self.while_stmt(),
            _ => self.expr_stmt(),
        }
    }

    // forStmt → "for" "(" (varDecl | exprStmt | ";") expression? ";" expression? ")" statement
    fn for_stmt(&mut self) -> Result<Stmt, ParseError> {
        // "for" already consumed

        self.consume(&LeftParen, "Expect '(' after 'for'.")?;
        let initializer = match self.match_next(&[Semicolon, Var]) {
            Some(Semicolon) => None,
            Some(Var) => Some(self.var_declaration()?),
            _ => Some(self.expr_stmt()?),
        };

        let condition = if let Some(&Semicolon) = self.peek() {
            Literal(Value::Boolean(true))
        } else {
            self.expression()?
        };
        self.consume(&Semicolon, "Expect ';' after for loop condition.")?;

        let increment = if let Some(&RightParen) = self.peek() {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(&RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;
        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment)]);
        }

        body = Stmt::While { condition, body: Box::new(body) };

        if let Some(initializer) = initializer { body = Stmt::Block(vec![initializer, body]) }

        Ok(body)
    }

    // ifStmt → if "(" expression ")" statement ("else" statement)?
    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        // "if" already consumed
        self.consume(&LeftParen, "Expect '(' after 'if'.")?;

        let condition = self.expression()?;

        self.consume(&RightParen, "Expect ')' after if condition.")?;

        let if_branch = Box::new(self.statement()?);

        let else_branch = if self.match_next(&[Else]).is_some() {
            Some(self.statement()?)
        } else {
            None
        }.map(Box::new);

        Ok(Stmt::If { condition, then_branch: if_branch, else_branch })
    }

    // printStmt → "print" expression ";"
    fn print_stmt(&mut self) -> Result<Stmt, ParseError> {
        // "print" is already consumed
        let expr = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Print(expr))
    }

    // returnStmt → "return" expression? ";"
    fn return_stmt(&mut self) -> Result<Stmt, ParseError> {
        // "return" is already consumed
        let val = if Some(&Semicolon) == self.peek() {
            Literal(Value::Nil)
        } else {
            self.expression()?
        };

        self.consume(&Semicolon, "Expect ';' after return value.")?;
        Ok(Stmt::Return(val))
    }

    // whileStmt → "while" "(" expression ")" stmt
    fn while_stmt(&mut self) -> Result<Stmt, ParseError> {
        // "while" already consumed

        self.consume(&LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(&RightParen, "Expect ')' after while condition.")?;
        let body = self.statement()?;

        Ok(Stmt::While { condition, body: Box::new(body) })
    }

    // block → "{" (declaration)* "}"
    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();

        while self.peek() != Some(&RightBrace) && self.peek() != None {
            stmts.push(self.declaration()?)
        }

        self.consume(&RightBrace, "Expect '}' after block.")?;
        Ok(stmts)
    }

    // exprStmt → expression ";"
    fn expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }

    #[inline]
    fn match_next(&mut self, expected_tokens: &[Token]) -> Option<Token> {
        for expected in expected_tokens {
            if self.peek() == Some(expected) {
                return self.advance();
            }
        }

        None
    }

    fn advance(&mut self) -> Option<Token> {
        match self.tokens.peek() {
            // never consume EOF
            None | Some(&TokenContext { token: Eof, .. }) => None,
            Some(_) => self.tokens.next().map(|ctx| ctx.token)
        }
    }

    #[inline]
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek().map(|ctx| &ctx.token)
    }

    #[inline]
    fn consume(&mut self, token: &Token, msg: &str) -> Result<(), ParseError> {
        if self.peek() == Some(token) {
            let _ = self.advance();
            Ok(())
        } else {
            Err(self.error(msg))
        }
    }

    #[inline]
    fn consume_identifier(&mut self, msg: &str) -> Result<String, ParseError> {
        match self.peek() {
            Some(&Identifier(_)) => {
                match self.advance().unwrap() {
                    Identifier(s) => Ok(s),
                    _ => unreachable!()
                }
            }
            _ => Err(self.error(msg)),
        }
    }

    fn error(&mut self, msg: &str) -> ParseError {
        self.lox.error_at_token(self.tokens.peek().unwrap(), msg);
        ParseError
    }

    fn synchronize(&mut self) {
        loop {
            match self.tokens.peek() {
                Some(ctx) if [Class, Fun, Var, For, If, While, Print, Return, Eof].contains(&ctx.token) => break,
                Some(_) => {}
                _ => panic!("End of iterator without encountering Eof."),
            }

            let _ = self.tokens.next();
        }
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Parse error.")]
struct ParseError;
