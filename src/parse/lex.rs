use std::{collections::HashMap, fmt::Display};

use crate::utils::string_interner::{IStr, intern};

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Position {
    line: usize,
    col: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Token {
    LPAR,
    RPAR,
    COMMA,
    PLUS,
    MINUS,
    STAR,
    BANG,
    TILDE,
    CARET,
    AND,
    PIPE,
    SEMI,
    EQ,
    EQEQ,
    LT,
    LTEQ,
    LTMINUS,
    FUN,
    WITHLOCAL,
    NULLPTR,
    IF,
    THEN,
    ELSE,
    WHILE,
    DO,
    DONE,
    RETURN,
    SET,
    ALLOC,
    FREE,
    IDENT(IStr),
    NUM(i32),
}

pub struct Lexer {
    data: Vec<char>,
    index: usize,
    idmap: HashMap<IStr, Token>,
    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new(data: Vec<char>) -> Self {
        let mut idmap = HashMap::new();
        idmap.insert(intern("fun"), Token::FUN);
        idmap.insert(intern("locals"), Token::WITHLOCAL);
        idmap.insert(intern("nullptr"), Token::NULLPTR);
        idmap.insert(intern("if"), Token::IF);
        idmap.insert(intern("then"), Token::THEN);
        idmap.insert(intern("else"), Token::ELSE);
        idmap.insert(intern("do"), Token::DO);
        idmap.insert(intern("done"), Token::DONE);
        idmap.insert(intern("while"), Token::WHILE);
        idmap.insert(intern("return"), Token::RETURN);
        idmap.insert(intern("set"), Token::SET);
        idmap.insert(intern("alloc"), Token::ALLOC);
        idmap.insert(intern("free"), Token::FREE);
        Self {
            data,
            idmap,
            index: 0,
            line: 1,
            col: 1,
        }
    }

    fn next_char(&mut self) -> char {
        let res = self.current_char();
        self.index += 1;
        if res == '\n' {
            self.col = 1;
            self.line += 1;
        } else {
            self.col += 1;
        }
        res
    }

    fn current_char(&mut self) -> char {
        self.data[self.index]
    }

    fn eof(&self) -> bool {
        self.index >= self.data.len()
    }

    fn lexnum(&mut self, firstchar: char) -> i32 {
        let mut res: i32 = firstchar.to_digit(10).unwrap().try_into().unwrap();
        while !self.eof() {
            let cc = self.current_char();
            if let '0'..='9' = cc {
                let cc = cc.to_digit(10).unwrap().try_into().unwrap();
                res = res.checked_mul(10).unwrap().checked_add(cc).unwrap();
                self.next_char();
            } else {
                break;
            }
        }
        res
    }

    fn lexident(&mut self, firstchar: char) -> IStr {
        let mut res: String = String::new();
        res.push(firstchar);
        while !self.eof() {
            let cc = self.current_char();
            if cc.is_alphanumeric() || cc == '_' {
                res.push(cc);
                self.next_char();
            } else {
                break;
            }
        }
        intern(res)
    }

    fn pos(&self) -> Position {
        Position {
            line: self.line,
            col: self.col,
        }
    }

    pub fn next_token(&mut self) -> (Position, Option<Token>) {
        while !self.eof() {
            return (
                self.pos(),
                Some(match self.next_char() {
                    ' ' | '\n' | '\t' | '\r' => continue,
                    '(' => Token::LPAR,
                    ')' => Token::RPAR,
                    ',' => Token::COMMA,
                    '+' => Token::PLUS,
                    '-' => Token::MINUS,
                    '*' => Token::STAR,
                    '!' => Token::BANG,
                    '~' => Token::TILDE,
                    '^' => Token::CARET,
                    '&' => Token::AND,
                    '|' => Token::PIPE,
                    ';' => Token::SEMI,
                    '=' if !self.eof() && self.current_char() == '=' => {
                        self.next_char();
                        Token::EQEQ
                    }
                    '=' => Token::EQ,
                    '<' if !self.eof() && self.current_char() == '=' => {
                        self.next_char();
                        Token::LTEQ
                    }
                    '<' if !self.eof() && self.current_char() == '-' => {
                        self.next_char();
                        Token::LTMINUS
                    }
                    '<' => Token::LT,
                    c @ '0'..='9' => Token::NUM(self.lexnum(c)),
                    c if c.is_alphabetic() || c == '_' => {
                        let str = self.lexident(c);
                        self.idmap.get(&str).copied().unwrap_or(Token::IDENT(str))
                    }
                    c => panic!("Unexpected char {c} at input {}", self.index),
                }),
            );
        }
        (self.pos(), None)
    }

    #[cfg(test)]
    pub fn tokenize(mut self) -> Vec<Token> {
        let mut res = Vec::new();
        while let (_, Some(x)) = self.next_token() {
            res.push(x);
        }
        res
    }
}

#[cfg(test)]
mod test {
    use crate::{parse::lex::Token, utils::string_interner::intern};

    use super::Lexer;

    #[test]
    fn test_all_tokens() {
        let data = "(),+-*!~^&|;= == < <= <- fun locals nullptr if then else do done while return set alloc free 0 1 2 42 999 foo bar baz FOO_BAR_42_baz\t\n";
        let res = Lexer::new(data.chars().collect()).tokenize();
        assert_eq!(res, [
            Token::LPAR,
            Token::RPAR,
            Token::COMMA,
            Token::PLUS,
            Token::MINUS,
            Token::STAR,
            Token::BANG,
            Token::TILDE,
            Token::CARET,
            Token::AND,
            Token::PIPE,
            Token::SEMI,
            Token::EQ,
            Token::EQEQ,
            Token::LT,
            Token::LTEQ,
            Token::LTMINUS,
            Token::FUN,
            Token::WITHLOCAL,
            Token::NULLPTR,
            Token::IF,
            Token::THEN,
            Token::ELSE,
            Token::DO,
            Token::DONE,
            Token::WHILE,
            Token::RETURN,
            Token::SET,
            Token::ALLOC,
            Token::FREE,
            Token::NUM(0),
            Token::NUM(1),
            Token::NUM(2),
            Token::NUM(42),
            Token::NUM(999),
            Token::IDENT(intern("foo")),
            Token::IDENT(intern("bar")),
            Token::IDENT(intern("baz")),
            Token::IDENT(intern("FOO_BAR_42_baz")),
        ])
    }
}
