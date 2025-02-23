use std::{collections::HashMap, fmt::Display};

use crate::utils::string_interner::{IStr, intern};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Position {
    file: IStr,
    line: usize,
    col: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            file: intern("internal.sl"),
            line: Default::default(),
            col: Default::default(),
        }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
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
    GT,
    MINUSGT,
    EQGT,
    COLONCOLON,
    COLON,
    UNDERSCORE,
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
    MATCH,
    WITH,
    END,
    INDUCTIVE,
    DEF,
    REC,
    LET,
    IN,
    INT,
    PTR,
    FORALL,
    IDENT(IStr),
    NUM(i32),
}

pub struct Lexer {
    filename: IStr,
    data: Vec<char>,
    index: usize,
    idmap: HashMap<IStr, Token>,
    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new(filename: IStr, data: Vec<char>) -> Self {
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
        idmap.insert(intern("match"), Token::MATCH);
        idmap.insert(intern("with"), Token::WITH);
        idmap.insert(intern("end"), Token::END);
        idmap.insert(intern("inductive"), Token::INDUCTIVE);
        idmap.insert(intern("def"), Token::DEF);
        idmap.insert(intern("rec"), Token::REC);
        idmap.insert(intern("let"), Token::LET);
        idmap.insert(intern("in"), Token::IN);
        idmap.insert(intern("int"), Token::INT);
        idmap.insert(intern("ptr"), Token::PTR);
        idmap.insert(intern("forall"), Token::FORALL);
        Self {
            filename,
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
            if cc.is_alphanumeric() || cc == '_' || cc == '\'' {
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
            file: self.filename,
            line: self.line,
            col: self.col,
        }
    }

    pub fn next_token(&mut self) -> (Position, Option<Token>, Position) {
        while !self.eof() {
            return (
                self.pos(),
                Some(match self.next_char() {
                    ' ' | '\n' | '\t' | '\r' => continue,
                    '/' if !self.eof() && self.current_char() == '/' => {
                        self.next_char();
                        while !self.eof() && self.next_char() != '\n' {}
                        continue;
                    }
                    '/' if !self.eof() && self.current_char() == '*' => {
                        self.next_char();
                        loop {
                            while !self.eof() && self.next_char() != '*' {}
                            if self.eof() {
                                panic!("Unterminated comment at EOF at {}", self.index)
                            }
                            if self.next_char() == '/' {
                                break;
                            }
                        }
                        continue;
                    }
                    '(' => Token::LPAR,
                    ')' => Token::RPAR,
                    ',' => Token::COMMA,
                    '+' => Token::PLUS,
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
                    '=' if !self.eof() && self.current_char() == '>' => {
                        self.next_char();
                        Token::EQGT
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
                    '>' => Token::GT,
                    '-' if !self.eof() && self.current_char() == '>' => {
                        self.next_char();
                        Token::MINUSGT
                    }
                    '-' => Token::MINUS,
                    ':' if !self.eof() && self.current_char() == ':' => {
                        self.next_char();
                        Token::COLONCOLON
                    }
                    ':' => Token::COLON,
                    '_' => Token::UNDERSCORE,
                    c @ '0'..='9' => Token::NUM(self.lexnum(c)),
                    c if c.is_alphabetic() || c == '_' => {
                        let str = self.lexident(c);
                        self.idmap.get(&str).copied().unwrap_or(Token::IDENT(str))
                    }
                    c => panic!("Unexpected char {c} at input {}", self.index),
                }),
                self.pos(),
            );
        }
        (self.pos(), None, self.pos())
    }

    #[cfg(test)]
    pub fn tokenize(mut self) -> Vec<Token> {
        let mut res = Vec::new();
        while let (_, Some(x), _) = self.next_token() {
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
        let data = "(),+-*!~^&|;= == < > <= <- -> => :: : _ fun locals nullptr if then else do done while return set alloc free match with end inductive def rec let in int ptr 0 1 2 42 999 foo bar baz FOO_BAR_42_baz\t\n";
        let res = Lexer::new(intern("foo"), data.chars().collect()).tokenize();
        assert_eq!(
            res,
            [
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
                Token::GT,
                Token::LTEQ,
                Token::LTMINUS,
                Token::MINUSGT,
                Token::EQGT,
                Token::COLONCOLON,
                Token::COLON,
                Token::UNDERSCORE,
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
                Token::MATCH,
                Token::WITH,
                Token::END,
                Token::INDUCTIVE,
                Token::DEF,
                Token::REC,
                Token::LET,
                Token::IN,
                Token::INT,
                Token::PTR,
                Token::NUM(0),
                Token::NUM(1),
                Token::NUM(2),
                Token::NUM(42),
                Token::NUM(999),
                Token::IDENT(intern("foo")),
                Token::IDENT(intern("bar")),
                Token::IDENT(intern("baz")),
                Token::IDENT(intern("FOO_BAR_42_baz")),
            ]
        )
    }

    #[test]
    fn test_comments() {
        let data = "foo //bar baz 42 /* aa */ bb \n foo/*  */bar baz";
        let res = Lexer::new(intern("foo"), data.chars().collect()).tokenize();
        assert_eq!(
            res,
            [
                Token::IDENT(intern("foo")),
                Token::IDENT(intern("foo")),
                Token::IDENT(intern("bar")),
                Token::IDENT(intern("baz"))
            ]
        )
    }
}
