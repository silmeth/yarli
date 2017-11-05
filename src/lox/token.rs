use std::fmt;

#[derive(Debug,PartialEq)]
pub enum Token {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;
        match *self {
            LeftParen => write!(f, "LeftParen"),
            RightParen => write!(f, "RightParen"),
            LeftBrace => write!(f, "LeftBrace"),
            RightBrace => write!(f, "RightBrace"),
            Comma => write!(f, "Comma"),
            Dot => write!(f, "Dot"),
            Minus => write!(f, "Minus"),
            Plus => write!(f, "Plus"),
            Semicolon => write!(f, "Semicolon"),
            Slash => write!(f, "Slash"),
            Star => write!(f, "Star"),
            Bang => write!(f, "Bang"),
            BangEqual => write!(f, "BangEq"),
            Equal => write!(f, "Eq"),
            EqualEqual => write!(f, "EqEq"),
            Greater => write!(f, "Greater"),
            GreaterEqual => write!(f, "GreaterEq"),
            Less => write!(f, "Less"),
            LessEqual => write!(f, "LessEq"),
            Identifier(ref s) => write!(f, "Identifier({})", s),
            String(ref s) => write!(f, "String({})", s),
            Number(ref num) => write!(f, "Number({})", num),
            And => write!(f, "And"),
            Class => write!(f, "Class"),
            Else => write!(f, "Else"),
            False => write!(f, "False"),
            Fun => write!(f, "Fun"),
            For => write!(f, "For"),
            If => write!(f, "If"),
            Nil => write!(f, "Nil"),
            Or => write!(f, "Or"),
            Print => write!(f, "Print"),
            Return => write!(f, "Return"),
            Super => write!(f, "Super"),
            This => write!(f, "This"),
            True => write!(f, "True"),
            Var => write!(f, "Var"),
            While => write!(f, "While"),
            Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug,PartialEq)]
pub struct TokenContext {
    pub token: Token,
    pub line: u32,
}

impl fmt::Display for TokenContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token)
    }
}
