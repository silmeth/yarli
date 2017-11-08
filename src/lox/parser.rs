use std::fmt;
use std::error;

use super::Lox;
use super::token::{TokenContext, Token};
use super::token::Token::*;
use super::ast::{Expr, Value, UnOperator, BiOperator};
use super::ast::Expr::*;

pub fn parse(tokens: Vec<TokenContext>, lox: &mut Lox) -> Expr {
    match ParserState::new(tokens, lox).expression() {
        Ok(e) => e,
        Err(_) => Expr::Literal(Value::Nil),
    }
}

struct ParserState<'a> {
    tokens: Vec<TokenContext>,
    current: usize,
    lox: &'a mut Lox,
}

impl<'a> ParserState<'a> {
    fn new(tokens: Vec<TokenContext>, lox: &mut Lox) -> ParserState {
        ParserState {
            tokens,
            current: 0,
            lox,
        }
    }

    // expression → equality
    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
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

    // unary → (("!" | "-") unary) | primary
    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op_token) = self.match_next(&[Bang, Minus]) {
            let rh = self.unary()?;
            let op = if op_token == Bang { UnOperator::Not } else { UnOperator::Minus };
            Ok(Unary { op, rh: Box::new(rh) })
        } else {
            self.primary()
        }
    }

    // primary → NUMBER | STRING | "false" | "true" | "nil" | "(" expr ")"
    fn primary(&mut self) -> Result<Expr, ParseError> {
        let res = if let Some(token) = self.advance() {
            match token {
                False => Literal(Value::Boolean(false)),
                True => Literal(Value::Boolean(true)),
                String(s) => Literal(Value::String(s)),
                Number(num) => Literal(Value::Number(num)),
                Nil => Literal(Value::Nil),
                LeftParen => {
                    let expr = self.expression()?;
                    self.consume(&RightParen, "Expect ')' after grouped expression.")?;
                    expr
                }
                _ => {
                    let idx = self.current;
                    return Err(self.error(idx, "Expect expression."));
                }
            }
        } else {
            // last index reached
            let idx = self.tokens.len() - 1;
            return Err(self.error(idx, "Expect expression."));
        };

        Ok(res)
    }

    #[inline]
    fn match_next(&mut self, expected_tokens: &[Token]) -> Option<Token> {
        for expected in expected_tokens {
            if self.check(expected) {
                return self.advance();
            }
        }

        None
    }

    fn advance(&mut self) -> Option<Token> {
        use std::mem;

        match self.tokens.get_mut(self.current) {
            // never consume EOF
            None | Some(&mut TokenContext { token: Eof, .. }) => None,
            Some(token) => {
                self.current += 1;
                Some(mem::replace(&mut token.token, Nil))
            },
        }
    }

    #[inline]
    fn check(&self, expected: &Token) -> bool {
        self.tokens[self.current].token == *expected
    }

    fn consume(&mut self, token: &Token, msg: &str) -> Result<(), ParseError> {
        if self.check(token) {
            let _ = self.advance();
            Ok(())
        } else {
            let current_idx = self.current;
            Err(self.error(current_idx, msg))
        }
    }

    fn error(&mut self, token_idx: usize, msg: &str) -> ParseError {
        self.lox.error_at_token(&self.tokens[token_idx], msg);
        ParseError
    }

//    fn synchronize(&mut self) {
//        let mut prev = self.advance();
//
//        while prev != Some(Semicolon) && !self.is_at_end() && prev != None {
//            match self.tokens[self.current].token {
//                Class | Fun | Var | For | If | While | Print | Return => return,
//                _ => { },
//            }
//
//            prev = self.advance();
//        }
//    }

    fn synchronize(&mut self) {
        if let Some((idx, _)) = self.tokens.iter()
            .enumerate()
            .skip(self.current)
            .find(|&(_, t)|
                [Class, Fun, Var, For, If, While, Print, Return].contains(&t.token)) {
            self.current = idx;
        } else {
            self.current = self.tokens.len() - 1;
        }
    }
}

#[derive(Debug, Clone)]
struct ParseError;

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error.")
    }
}

impl error::Error for ParseError {
    fn description(&self) -> &str {
        "Parse error."
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
