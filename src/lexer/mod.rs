use crate::{throw_unexpected_char, try_next_char};
use std::{
    fmt::{Debug, Write},
    ops::Range,
    rc::Rc,
    str::Chars,
};
#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    UnexpectedChar(char, u32, u32),
    UnexpectedEof(u32, u32),
    ExpectedCharNotExisting(char, u32, u32),
}
pub type Result<T> = std::result::Result<T, LexerError>;

pub struct Lexer {
    chars: Vec<char>,
    line: u32,
    col: u32,
    pos: usize,
    token_stream: Vec<Token>,
}

impl Lexer {
    pub fn new(code: Chars<'_>) -> Lexer {
        Self {
            chars: code.collect(),
            token_stream: vec![],
            line: 1,
            col: 1,
            pos: 0,
        }
    }
    pub fn tokenize(mut self) -> Result<Vec<Token>> {
        while let (tok, is_eof) = self.next_token() && !is_eof {
            self.token_stream.push(tok?);
        }
        Ok(self.token_stream)
    }
    fn next_token(&mut self) -> (Result<Token>, bool) {
        self.skip_unneeded();
        if self.chars.len() - self.pos <= 0 {
            return (Ok(Token::new(self, TokenKind::_Eof, "".into())), true);
        }
        let tok = match try_next_char!(self) {
            '/' => match self.peek_char(0) {
                Some('/') => {
                    try_next_char!(self);
                    self.take_until("\n");
                    return self.next_token();
                }
                Some('*') => {
                    try_next_char!(self);
                    self.take_until("*/");
                    return self.next_token();
                }
                _ => Token::new(self, TokenKind::Punctuation(Punctuation::Div), "/".into()),
            },
            '&' => {
                match self.peek_char(0) {
                    Some('&') => {
                        try_next_char!(self);
                        return (
                            Ok(Token::new(
                                self,
                                TokenKind::Punctuation(Punctuation::And),
                                "&&".into(),
                            )),
                            false,
                        );
                    }
                    _ => {}
                }
                return (
                    Err(LexerError::ExpectedCharNotExisting(
                        '&', self.col, self.line,
                    )),
                    false,
                );
            }
            '|' => {
                match self.peek_char(0) {
                    Some('|') => {
                        try_next_char!(self);
                        return (
                            Ok(Token::new(
                                self,
                                TokenKind::Punctuation(Punctuation::Or),
                                "||".into(),
                            )),
                            false,
                        );
                    }
                    _ => {}
                }
                return (
                    Ok(Token::new(
                        self,
                        TokenKind::Punctuation(Punctuation::BitwiseOr),
                        "|".into(),
                    )),
                    false,
                );
            }
            '"' => {
                let s = self.take_until("\"");
                Token::new(
                    self,
                    TokenKind::String(s.clone()),
                    format!("\"{}\"", s).into(),
                )
            }
            '`' => {
                let s = self.take_until("`");
                Token::new(
                    self,
                    TokenKind::String(s.clone()),
                    format!("`{}`", s).into(),
                )
            }
            '-' => Token::new(self, TokenKind::Punctuation(Punctuation::Sub), "-".into()),
            '{' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::LeftBracket),
                "{".into(),
            ),
            '}' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::RightBracket),
                "}".into(),
            ),
            '(' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::LeftParen),
                "(".into(),
            ),
            ')' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::RightParen),
                ")".into(),
            ),
            '[' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::LeftSBracket),
                "[".into(),
            ),
            ']' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::RightSBracket),
                "]".into(),
            ),
            '.' => Token::new(self, TokenKind::Punctuation(Punctuation::Dot), ".".into()),
            ':' => Token::new(self, TokenKind::Punctuation(Punctuation::Colon), ":".into()),
            ';' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::Semicolon),
                ";".into(),
            ),
            '%' => Token::new(self, TokenKind::Punctuation(Punctuation::Mod), "%".into()),
            '*' => Token::new(self, TokenKind::Punctuation(Punctuation::Mul), "*".into()),
            ',' => Token::new(self, TokenKind::Punctuation(Punctuation::Comma), ",".into()),
            '+' => Token::new(self, TokenKind::Punctuation(Punctuation::Plus), "+".into()),
            '^' => Token::new(self, TokenKind::Punctuation(Punctuation::Exp), "^".into()),
            '!' => Token::new(self, TokenKind::Punctuation(Punctuation::Not), "!".into()),
            '<' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::LeftArrow),
                "<".into(),
            ),
            '>' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::RightArrow),
                ">".into(),
            ),
            '=' => Token::new(
                self,
                TokenKind::Punctuation(Punctuation::Equals),
                "=".into(),
            ),
            '#' => Token::new(self, TokenKind::Punctuation(Punctuation::Hash), "#".into()),
            c => {
                if c == '0' && let Some(char) = self.peek_char(0) && (char == 'x' || char == 'X') {
                    let _ = self.next_char();
                    let mut number: String = "0x".into();
                    number.push_str(&self.take_while(|c| c.is_ascii_hexdigit()));
                    return (Ok(Token::new(self, TokenKind::Number(Number::Hex(u64::from_str_radix(&number[2..], 16).unwrap())), number.into())), false);
                }
                if c.is_numeric() {
                    let mut number: String = c.into();
                    number.push_str(&self.take_while(|c| c.is_numeric() || c == '.'));
                    let mut dot_n = 0;
                    let chars = number.chars().collect::<Vec<_>>();
                    let mut i = 0;
                    let a = chars.len();
                    loop {
                        if i == chars.len() {
                            break;
                        }
                        let char = chars[i];

                        if char == '.' {
                            dot_n += 1;
                        }
                        i += 1;
                        if i < chars.len() && dot_n > 0 && !chars[i].is_numeric() {
                            dot_n -= 1;
                            i -= 1;
                            self.pos -= a - i;
                            break;
                        }
                    }
                    let number = String::from_iter(&chars[..i]);
                    if dot_n == 1 {
                        if let Ok(num) = number.parse() {
                            return (
                                Ok(Token::new(
                                    self,
                                    TokenKind::Number(Number::Float(num)),
                                    number.into(),
                                )),
                                false,
                            );
                        }
                    } else if dot_n == 0 {
                        if let Ok(num) = number.parse() {
                            return (
                                Ok(Token::new(
                                    self,
                                    TokenKind::Number(Number::Int(num)),
                                    number.into(),
                                )),
                                false,
                            );
                        }
                    }
                }
                if c.is_alphanumeric() || c == '_' {
                    let mut id: String = c.into();
                    id.push_str(&self.take_while(|c| c.is_alphanumeric() || c == '_'));
                    let id: Rc<str> = id.into();
                    return (Ok(Token::new(self, TokenKind::ID(id.clone()), id)), false);
                }
                throw_unexpected_char!(c, self)
            }
        };
        (Ok(tok), false)
    }
    fn take_until(&mut self, pattern: &str) -> Rc<str> {
        let pattern: Vec<char> = pattern.chars().collect();
        let mut pos = 0;
        let mut out = String::new();
        let mut buf = String::new();
        loop {
            if pattern.len() == pos {
                break;
            }
            if let Ok(char) = self.next_char_unchecked() {
                if char == pattern[pos] {
                    buf.push(char);
                    pos += 1;
                } else {
                    out.push_str(&buf);
                    pos = 0;
                    out.push(char);
                    buf.clear();
                }
            } else {
                break;
            }
        }
        out.into()
    }
    fn take_while(&mut self, condition: impl Fn(char) -> bool) -> String {
        let mut out = String::new();
        while let Some(char) = self.peek_char(0) && condition(char) {
            out.push(self.next_char_unchecked().unwrap());
        }
        out
    }
    fn skip_unneeded(&mut self) {
        while let Some(char) = self.peek_char(0) {
            if char == ' ' || char == '\t' || char == '\n' {
                let _ = self.next_char_unchecked();
            } else {
                break;
            }
        }
    }
    fn next_char_unchecked(&mut self) -> Result<char> {
        if self.chars.len() - self.pos <= 0 {
            return Err(LexerError::UnexpectedEof(self.line, self.col - 1));
        }
        let char = self.chars[self.pos];
        self.pos += 1;
        self.col += 1;
        if char == '\n' {
            self.col = 1;
            self.line += 1;
        }
        Ok(char)
    }
    fn next_char(&mut self) -> Result<char> {
        let char = self.next_char_unchecked();
        if let Ok(char) = char {
            if char == '\n' {
                return self.next_char();
            }
        }
        char
    }
    fn peek_char(&self, offset: i32) -> Option<char> {
        if (self.chars.len() - self.pos) as i32 - offset <= 0 {
            return None;
        }
        if self.pos as i32 + offset < 0 {
            return None;
        }
        Some(self.chars[(self.pos as i32 + offset) as usize])
    }
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub token: TokenKind,
    pub contents: Rc<str>,
    pub pos: Range<usize>,
    pub line: u32,
    pub col: u32,
}
impl Token {
    pub fn new(lexer: &Lexer, token: TokenKind, contents: Rc<str>) -> Token {
        Token {
            token,
            pos: lexer.pos..lexer.pos + contents.len(),
            contents,
            line: lexer.line,
            col: lexer.col,
        }
    }
}
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.contents)
    }
}
pub(crate) struct TokenStream(Vec<Token>);
pub(crate) trait ToTokenStream {
    fn to_token_stream(self) -> TokenStream;
}
impl<T: Iterator<Item = Token>> ToTokenStream for T {
    fn to_token_stream(self) -> TokenStream {
        TokenStream(self.collect())
    }
}
impl Debug for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('"')?;
        f.write_char(' ')?;
        for t in self.0.iter() {
            t.fmt(f)?;
            f.write_char(' ')?;
        }
        f.write_char('"')?;
        Ok(())
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    ID(Rc<str>),
    String(Rc<str>),
    Punctuation(Punctuation),
    Number(Number),
    _Eof,
}
impl TokenKind {
    pub fn kind(&self) -> TokenKindDesc {
        match self {
            TokenKind::ID(_) => TokenKindDesc::ID,
            TokenKind::String(_) => TokenKindDesc::String,
            TokenKind::Punctuation(_) => TokenKindDesc::Punctuation,
            TokenKind::Number(_) => TokenKindDesc::Number,
            TokenKind::_Eof => unreachable!(),
        }
    }
}
impl PartialEq<TokenKind> for TokenKindDesc {
    fn eq(&self, other: &TokenKind) -> bool {
        match other {
            TokenKind::ID(_) => *self == TokenKindDesc::ID,
            TokenKind::String(_) => *self == TokenKindDesc::String,
            TokenKind::Punctuation(_) => *self == TokenKindDesc::Punctuation,
            TokenKind::Number(_) => *self == TokenKindDesc::Number,
            TokenKind::_Eof => false,
        }
    }
}
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TokenKindDesc {
    ID,
    String,
    Punctuation,
    Number,
}
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Punctuation {
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    LeftSBracket,
    RightSBracket,
    Dot,
    Colon,
    Semicolon,
    Mod,
    Mul,
    And,
    Or,
    Comma,
    Sub,
    Div,
    Plus,
    Exp,
    Not,
    LeftArrow,
    RightArrow,
    Equals,
    Hash,
    BitwiseOr,
}
impl Punctuation {
    pub fn is_operation(&self) -> bool {
        match self {
            Punctuation::LeftBracket => false,
            Punctuation::RightBracket => false,
            Punctuation::LeftParen => false,
            Punctuation::RightParen => false,
            Punctuation::LeftSBracket => false,
            Punctuation::RightSBracket => false,
            Punctuation::Colon => false,
            Punctuation::Semicolon => false,
            _ => true,
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Float(f64),
    Int(u64),
    Hex(u64),
}
#[macro_export]
macro_rules! try_next_char {
    ($self:expr) => {{
        let next_char = $self.next_char();
        if let Ok(char) = next_char {
            char
        } else {
            return (Err(next_char.err().unwrap()), false);
        }
    }};
}
#[macro_export]
macro_rules! throw_unexpected_char {
    ($char:expr, $lex:expr) => {
        return (
            Err(LexerError::UnexpectedChar($char, $lex.line, $lex.col - 1)),
            false,
        )
    };
}
