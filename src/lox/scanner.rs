use super::Lox;
use super::token::TokenContext;
use super::token::Token;
use super::token::Token::*;

pub fn scan_tokens(source: &[u8], lox: &mut Lox) -> Vec<TokenContext> {
    ScannerState::new(source, lox).scan_tokens()
}

struct ScannerState<'a, 'b> {
    source: &'a [u8],
    lox: &'b mut Lox,
    tokens: Vec<TokenContext>,
    start: usize,
    current: usize,
    line: u32,
}

impl <'a, 'b> ScannerState<'a, 'b> {
    pub fn new(source: &'a [u8], lox: &'b mut super::Lox) -> ScannerState<'a, 'b> {
        ScannerState {
            source,
            lox,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Vec<TokenContext> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.add_token(Eof);

        self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            b'(' => self.add_token(LeftParen),
            b')' => self.add_token(RightParen),
            b'{' => self.add_token(LeftBrace),
            b'}' => self.add_token(RightBrace),
            b',' => self.add_token(Comma),
            b'.' => self.add_token(Dot),
            b'-' => self.add_token(Minus),
            b'+' => self.add_token(Plus),
            b';' => self.add_token(Semicolon),
            b'*' => self.add_token(Star),
            b'!' => {
                let token = if self.match_next(b'=') { BangEqual } else { Bang };
                self.add_token(token);
            },
            b'=' => {
                let token = if self.match_next(b'=') { EqualEqual } else { Equal };
                self.add_token(token);
            },
            b'<' => {
                let token = if self.match_next(b'=') { LessEqual } else { Less };
                self.add_token(token);
            },
            b'>' => {
                let token = if self.match_next(b'=') { GreaterEqual } else { Greater };
                self.add_token(token);
            },
            b'/' => {
                if self.match_next(b'/') {
                    while self.peek() != Some(b'\n') && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(Slash);
                }
            },
            b' ' | b'\t' | b'\r' => { },
            b'"' => self.string(),
            b'\n' => self.line += 1,
            c => {
                if c.is_ascii_digit() {
                    self.number();
                } else if c.is_ascii_alphabetic() {
                    self.identifier();
                } else {
                    self.lox.error_on_line(self.line, "Unknown token.");
                }
            }
        }
    }

    fn string(&mut self) {
        use std::str;

        while self.peek() != Some(b'"') && !self.is_at_end() {
            if let Some(b'\n') = self.peek() { self.line += 1; }
            self.advance();
        }

        if self.is_at_end() {
            self.lox.error_on_line(self.line, "Unterminated string.");
            return;
        }

        // consume terminating '"'
        self.advance();

        let value = match str::from_utf8(&self.source[self.start + 1 .. self.current - 1]) {
            Ok(s) => s.to_owned(),
            Err(e) => {
                self.lox.error_on_line(self.line, &format!("String literal is not a valid UTF-8. {}", e));
                return;
            }
        };
        self.add_token(StringLit(value));
    }

    fn number(&mut self) {
        use std::str;

        while self.peek().map_or(false, |b| b.is_ascii_digit()) {
            self.advance();
        }

        if self.peek() == Some(b'.') && self.peek_next().map_or(false, |b| b.is_ascii_digit()) {
            self.advance();
            while self.peek().map_or(false, |b| b.is_ascii_digit()) {
                self.advance();
            }
        }

        // this is valid ASCII as contains only digits and '.'
        let num = str::from_utf8(&self.source[self.start .. self.current])
            .unwrap()
            .parse()
            .unwrap();
        self.add_token(Number(num));
    }

    fn identifier(&mut self) {
        use std::str;

        while self.peek().map_or(false, |b| b.is_ascii_alphanumeric()) {
            self.advance();
        }

        // identifiers are always correct ASCII
        let token = match str::from_utf8(&self.source[self.start .. self.current]).unwrap() {
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "for" => For,
            "fun" => Fun,
            "if" => If,
            "nil" => Nil,
            "or" => Or,
            "print" => Print,
            "return" => Return,
            "super" => Super,
            "this" => This,
            "true" => True,
            "var" => Var,
            "while" => While,
            name => Identifier(name.to_owned())
        };

        self.add_token(token);
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> u8 {
        let res = self.source[self.current];
        self.current += 1;
        res
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(TokenContext { token, line: self.line });
    }

    fn match_next(&mut self, char: u8) -> bool {
        if self.is_at_end() || self.source[self.current] != char {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self) -> Option<u8> {
        self.source.get(self.current).cloned()
    }

    fn peek_next(&self) -> Option<u8> {
        self.source.get(self.current + 1).cloned()
    }
}
