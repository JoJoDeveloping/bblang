use std::collections::HashMap;

use crate::{
    compile::highast::{BoxExpr, Expr, Function, Program},
    progs::ast::{BinOp, Value},
    utils::string_interner::IStr,
};

use super::lex::{Lexer, Position, Token};

pub struct Parser {
    lexer: Lexer,
    saved_next_tok: Option<Token>,
    saved_last_pos: Position,
    current_last_pos: Position,
    pos: Position,
}

impl Token {
    fn lprec(self) -> u8 {
        use Token::*;
        match self {
            LTMINUS => 1,
            PIPE => 1,
            CARET => 2,
            AND => 3,
            EQEQ => 4,
            LTEQ | LT => 5,
            STAR => 6,
            PLUS | MINUS => 7,
            _ => unreachable!(),
        }
    }
    fn rprec(self) -> u8 {
        use Token::*;
        match self {
            LTMINUS => 0,
            PIPE => 2,
            CARET => 3,
            AND => 4,
            EQEQ => 5,
            LTEQ | LT => 6,
            STAR => 7,
            PLUS | MINUS => 8,
            _ => unreachable!(),
        }
    }
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let (pos, saved_next_tok, current_last_pos) = lexer.next_token();
        Self {
            lexer,
            saved_next_tok,
            saved_last_pos: pos,
            current_last_pos,
            pos,
        }
    }

    fn error(&self, err: &str) -> ! {
        panic!("Parse error at {}: {err}!", self.pos)
    }

    fn tok_error(&self, err: &str, got: Token) -> ! {
        self.error(&format!("{err}, got {got:?}"))
    }

    fn otok_error(&self, err: &str, got: Option<Token>) -> ! {
        match got {
            Some(x) => self.tok_error(err, x),
            None => self.error(&format!("{err}, got EOF")),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        let res = self.saved_next_tok;
        let (pos, nt, end_pos) = self.lexer.next_token();
        self.saved_next_tok = nt;
        self.pos = pos;
        self.current_last_pos = self.saved_last_pos;
        self.saved_last_pos = end_pos;
        res
    }

    fn peek_token(&mut self) -> Option<Token> {
        self.saved_next_tok
    }

    fn pos(&self) -> Position {
        self.pos
    }

    fn endpos(&self) -> Position {
        self.current_last_pos
    }

    fn expect(&mut self, e: Token) {
        match self.next_token() {
            Some(k) if k == e => (),
            x => self.otok_error(&format!("expected {e:?}"), x),
        }
    }

    fn expect_ident(&mut self) -> IStr {
        match self.next_token() {
            Some(Token::IDENT(x)) => x,
            x => self.otok_error("expected identifier", x),
        }
    }

    fn parse_comma_separated_idents(&mut self, end: Token) -> Vec<IStr> {
        self.parse_comma_separated(end, Parser::expect_ident)
    }

    fn parse_comma_separated<T, F: for<'a> Fn(&'a mut Self) -> T>(
        &mut self,
        end: Token,
        f: F,
    ) -> Vec<T> {
        let mut args = Vec::new();
        loop {
            if self.peek_token() == Some(end) {
                self.next_token();
                break;
            }
            args.push(f(self));
            match self.next_token() {
                Some(Token::COMMA) => continue,
                Some(x) if x == end => break,
                x => self.otok_error(&format!("expected comma or {end:?}"), x),
            }
        }

        args
    }

    fn parse_function(&mut self) -> Function {
        let start = self.pos();
        self.expect(Token::FUN);
        let name = self.expect_ident();
        self.expect(Token::LPAR);
        let args = self.parse_comma_separated_idents(Token::RPAR);
        let locals = match self.next_token() {
            Some(Token::WITHLOCAL) => self.parse_comma_separated_idents(Token::EQ),
            Some(Token::EQ) => Vec::new(),
            x => self.otok_error("expected locals or body", x),
        };
        let code = self.parse_expr();
        Function {
            name,
            args,
            locals: locals.into_iter().collect(),
            code,
            span: (start, self.endpos()),
        }
    }

    fn parse_sexpr(&mut self) -> BoxExpr {
        let start = self.pos();
        match self.peek_token() {
            Some(Token::IF) => {
                self.next_token();
                let expr1 = self.parse_expr();
                self.expect(Token::THEN);
                let expr2 = self.parse_expr();
                self.expect(Token::ELSE);
                let expr3 = self.parse_expr();
                Box::new((Expr::If(expr1, expr2, expr3), (start, self.endpos())))
            }
            Some(Token::WHILE) => {
                self.next_token();
                let expr1 = self.parse_expr();
                self.expect(Token::DO);
                let expr2 = self.parse_expr();
                self.expect(Token::DONE);
                Box::new((Expr::While(expr1, expr2), (start, self.endpos())))
            }
            Some(Token::RETURN) => {
                self.next_token();
                Box::new((Expr::Return(self.parse_sexpr()), (start, self.endpos())))
            }
            Some(Token::FREE) => {
                self.next_token();
                Box::new((Expr::Free(self.parse_sexpr()), (start, self.endpos())))
            }
            Some(Token::SET) => {
                self.next_token();
                let ident = self.expect_ident();
                self.expect(Token::EQ);
                let expr2 = self.parse_sexpr();
                Box::new((Expr::SetLocal(ident, expr2), (start, self.endpos())))
            }
            _ => self.parse_bin_expr(),
        }
    }

    fn parse_expr(&mut self) -> BoxExpr {
        let start = self.pos();
        let prim = self.parse_sexpr();
        if let Some(Token::SEMI) = self.peek_token() {
            self.next_token();
            let other = self.parse_expr();
            Box::new((Expr::Seq(prim, other), (start, self.endpos())))
        } else {
            prim
        }
    }

    fn parse_bin_expr(&mut self) -> BoxExpr {
        self.parse_bin_expr_prec(0)
    }

    fn peek_binop(&mut self) -> Option<Token> {
        use Token::*;
        match self.peek_token() {
            Some(x @ (PLUS | MINUS | STAR | EQEQ | LTEQ | LT | AND | PIPE | CARET | LTMINUS)) => {
                Some(x)
            }
            _ => None,
        }
    }

    fn parse_bin_expr_prec(&mut self, cur_prec: u8) -> BoxExpr {
        use Token::*;
        let start = self.pos();
        let mut lhs = self.parse_simple_expr();
        while let Some(opr) = self.peek_binop() {
            if opr.lprec() < cur_prec {
                break;
            }
            self.next_token();
            let rhs = self.parse_bin_expr_prec(opr.rprec());
            lhs = Box::new((
                match opr {
                    LTMINUS => Expr::Store(lhs, rhs),
                    PLUS => Expr::BinOp(lhs, BinOp::Add, rhs),
                    MINUS => Expr::BinOp(lhs, BinOp::Sub, rhs),
                    STAR => Expr::BinOp(lhs, BinOp::Mul, rhs),
                    LT => Expr::BinOp(lhs, BinOp::Lt, rhs),
                    EQEQ => Expr::BinOp(lhs, BinOp::Eq, rhs),
                    LTEQ => Expr::BinOp(lhs, BinOp::Le, rhs),
                    AND => Expr::BinOp(lhs, BinOp::BitwiseAnd, rhs),
                    PIPE => Expr::BinOp(lhs, BinOp::BitwiseOr, rhs),
                    CARET => Expr::BinOp(lhs, BinOp::BitwiseXor, rhs),
                    _ => unreachable!(),
                },
                (start, self.endpos()),
            ))
        }
        lhs
    }

    fn parse_simple_expr(&mut self) -> BoxExpr {
        let start = self.pos();
        Box::new((
            match self.next_token() {
                Some(Token::IDENT(x)) => {
                    if let Some(Token::LPAR) = self.peek_token() {
                        let fnname = x;
                        self.expect(Token::LPAR);
                        let args = self.parse_comma_separated(Token::RPAR, Parser::parse_expr);
                        Expr::Call(fnname, args)
                    } else {
                        Expr::ReadLocal(x)
                    }
                }
                Some(Token::NUM(x)) => Expr::Const(Value::Int(x)),
                Some(Token::NULLPTR) => Expr::Const(Value::NullPointer),
                Some(Token::LPAR) => {
                    let res = self.parse_expr();
                    self.expect(Token::RPAR);
                    return res;
                }
                Some(Token::TILDE) => Expr::BitNot(self.parse_simple_expr()),
                Some(Token::BANG) => Expr::If(
                    self.parse_simple_expr(),
                    Box::new((Expr::Const(Value::Int(0)), (start, start))),
                    Box::new((Expr::Const(Value::Int(1)), (start, start))),
                ),
                Some(Token::MINUS) => Expr::Negate(self.parse_simple_expr()),
                Some(Token::STAR) => Expr::Load(self.parse_simple_expr()),
                Some(Token::ALLOC) => Expr::Alloc(self.parse_simple_expr()),
                got => self.otok_error("expected expression", got),
            },
            (start, self.endpos()),
        ))
    }

    pub fn parse_program(&mut self) -> Program {
        let mut functions = HashMap::new();
        while self.peek_token().is_some() {
            let func = self.parse_function();
            let name = func.name;
            if functions.insert(func.name, func).is_some() {
                self.error(&format!("function {name} defined twice"))
            }
        }
        Program { functions }
    }
}
