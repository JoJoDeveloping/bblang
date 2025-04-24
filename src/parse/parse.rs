use std::{collections::HashMap, vec};

use crate::{
    compile::highast::{BoxExpr, Expr, Function, Program},
    progs::ast::{BinOp, Value},
    specs::source_ast::{
        FunctionSpec, SourceConstDef, SourceConstructor, SourceDef, SourceExpr, SourceExprBox,
        SourceGenerics, SourceInductive, SourceInductives, SourceMatchArm, SourcePolyType,
        SourceType, SourceTypeBox,
    },
    utils::string_interner::{IStr, intern},
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
        let spec = if let Some(Token::SPEC) = self.peek_token() {
            self.next_token();
            let mut res = HashMap::new();
            loop {
                let name = self.expect_ident();
                self.expect(Token::COLON);
                let prog = self.parse_spec_const_defs();
                if res.insert(name, prog).is_some() {
                    self.error(&format!("Duplicate spec kind {name}"))
                }
                if let Some(Token::SEMI) = self.peek_token() {
                    self.next_token();
                } else {
                    break;
                }
            }
            self.expect(Token::END);
            let rres = FunctionSpec {
                pre: res.remove(&intern("pre")).unwrap_or_default(),
                post: res.remove(&intern("post")).unwrap_or_default(),
                arg_names: args.clone(),
            };
            if !res.is_empty() {
                self.error(&format!("unused spec kind {name}"))
            }
            Some(rres)
        } else {
            None
        };
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
            spec,
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
        let mut spec_stuff = Vec::new();
        while self.peek_token().is_some() {
            match self.peek_token() {
                Some(Token::FUN) => {
                    let func = self.parse_function();
                    let name = func.name;
                    if functions.insert(func.name, func).is_some() {
                        self.error(&format!("function {name} defined twice"))
                    }
                }
                Some(Token::SPEC) => {
                    self.next_token();
                    spec_stuff.extend(self.parse_spec_program());
                    self.expect(Token::END);
                }
                got => self.otok_error("Expected function or spec block", got),
            }
        }
        Program {
            functions,
            spec_stuff,
        }
    }

    fn parse_spec_const_defs(&mut self) -> Vec<SourceConstDef> {
        let mut res = Vec::new();
        while let Some(Token::DEF) = self.peek_token() {
            res.push(self.parse_const_item());
        }
        res
    }

    fn parse_spec_program(&mut self) -> Vec<SourceDef> {
        let mut res = Vec::new();
        while let Some(Token::DEF | Token::INDUCTIVE) = self.peek_token() {
            res.push(self.parse_spec_item());
        }
        res
    }

    pub fn parse_spec_program_eager(&mut self) -> Vec<SourceDef> {
        let mut res = Vec::new();
        while let Some(_) = self.peek_token() {
            res.push(self.parse_spec_item());
        }
        res
    }

    fn parse_spec_item(&mut self) -> SourceDef {
        match self.peek_token() {
            Some(Token::INDUCTIVE) => SourceDef::Inductives(self.parse_inductives()),
            Some(Token::DEF) => SourceDef::Const(self.parse_const_item()),
            got => self.otok_error("expected inductives or const def", got),
        }
    }

    fn parse_inductive(&mut self) -> SourceInductive {
        let begin = self.pos();
        let name = self.expect_ident();
        let generics = self.parse_generics();
        self.expect(Token::EQ);
        match self.peek_token() {
            Some(Token::BANG) => {
                self.next_token();
                return SourceInductive {
                    generics,
                    name,
                    constrs: HashMap::new(),
                    pos: (begin, self.endpos()),
                };
            }
            Some(Token::PIPE) => {
                self.next_token();
            }
            _ => {}
        }
        let mut constrs = HashMap::new();
        loop {
            let cname = self.expect_ident();
            let args = if let Some(Token::LPAR) = self.peek_token() {
                self.next_token();
                self.parse_comma_separated(Token::RPAR, Self::parse_source_type)
            } else {
                vec![]
            };
            if let Some(_) = constrs.insert(cname, SourceConstructor { args }) {
                self.error(&format!("Constructor {cname} appears twice in {name}"))
            }
            if let Some(Token::PIPE) = self.peek_token() {
                self.next_token();
                continue;
            } else {
                break;
            }
        }
        SourceInductive {
            generics,
            name,
            constrs,
            pos: (begin, self.endpos()),
        }
    }

    fn parse_inductives(&mut self) -> SourceInductives {
        self.expect(Token::INDUCTIVE);
        let mut res = Vec::new();
        loop {
            res.push(self.parse_inductive());
            if let Some(Token::WITH) = self.peek_token() {
                self.next_token();
                continue;
            } else {
                break;
            }
        }
        SourceInductives(res)
    }

    fn parse_generics(&mut self) -> SourceGenerics {
        let Some(Token::LT) = self.peek_token() else {
            return SourceGenerics { names: vec![] };
        };
        self.next_token();
        let names = self.parse_comma_separated_idents(Token::GT);
        SourceGenerics { names }
    }

    fn parse_source_type(&mut self) -> SourceTypeBox {
        let begin = self.pos();
        let bty = self.parse_source_base_type();
        if let Some(Token::MINUSGT) = self.peek_token() {
            self.next_token();
            let res = self.parse_source_type();
            Box::new((SourceType::Arrow(bty, res), (begin, self.endpos())))
        } else {
            bty
        }
    }

    fn parse_source_base_type(&mut self) -> SourceTypeBox {
        let begin = self.pos();
        match self.next_token() {
            Some(Token::LPAR) => {
                let res = self.parse_source_type();
                self.expect(Token::RPAR);
                res
            }
            Some(Token::IDENT(name)) => {
                let Some(Token::LT) = self.peek_token() else {
                    return Box::new((SourceType::BoundVar(name), (begin, self.endpos())));
                };
                self.next_token();
                let spec = self.parse_comma_separated(Token::GT, Self::parse_source_type);
                Box::new((SourceType::Inductive(name, spec), (begin, self.endpos())))
            }
            Some(Token::INT) => Box::new((SourceType::BuiltinInt, (begin, self.endpos()))),
            Some(Token::PTR) => Box::new((SourceType::BuiltinPtr, (begin, self.endpos()))),
            got => self.otok_error("expected (spec) type", got),
        }
    }

    fn parse_polyty(&mut self) -> SourcePolyType {
        let names = if let Some(Token::FORALL) = self.peek_token() {
            self.next_token();
            let mut names = Vec::new();
            loop {
                match self.next_token() {
                    Some(Token::COMMA) => break,
                    Some(Token::IDENT(i)) => names.push(i),
                    got => self.otok_error("expected ident or quantifier ending comma", got),
                }
            }
            names
        } else {
            vec![]
        };
        SourcePolyType {
            binders: SourceGenerics { names },
            ty: self.parse_source_type(),
        }
    }

    fn parse_const_item(&mut self) -> SourceConstDef {
        let begin = self.pos();
        self.expect(Token::DEF);
        let name = self.expect_ident();
        self.expect(Token::COLON);
        let ty = self.parse_polyty();
        self.expect(Token::EQ);
        let value = self.parse_spec_expr();
        SourceConstDef {
            name,
            ty,
            value,
            pos: (begin, self.endpos()),
        }
    }

    fn parse_spec_expr(&mut self) -> SourceExprBox {
        let begin = self.pos();
        let mut res = self.parse_spec_base_expr();
        while let Some(
            Token::MATCH
            | Token::LET
            | Token::FUN
            | Token::REC
            | Token::PREDICATE
            | Token::LPAR
            | Token::IDENT(_)
            | Token::NUM(_),
        ) = self.peek_token()
        {
            res = Box::new((
                SourceExpr::App(res, self.parse_spec_base_expr()),
                (begin, self.endpos()),
            ));
        }
        res
    }

    fn parse_spec_base_expr(&mut self) -> SourceExprBox {
        let begin = self.pos();
        Box::new((
            match self.next_token() {
                Some(Token::MATCH) => {
                    let discriminee = self.parse_spec_expr();
                    let ty = if let Some(Token::COLON) = self.peek_token() {
                        self.next_token();
                        Some(self.expect_ident())
                    } else {
                        None
                    };
                    self.expect(Token::WITH);
                    let mut arms = HashMap::new();
                    match self.peek_token() {
                        Some(Token::END) => {
                            self.next_token();
                            return Box::new((
                                SourceExpr::IndMatch(discriminee, ty, arms),
                                (begin, self.endpos()),
                            ));
                        }
                        Some(Token::PIPE) => {
                            self.next_token();
                        }
                        _ => {}
                    }
                    loop {
                        let constr = self.expect_ident();
                        if arms.contains_key(&constr) {
                            self.error(&format!("Match has arm {constr} twice!"));
                        }
                        let vars = if let Some(Token::LPAR) = self.peek_token() {
                            self.next_token();
                            self.parse_comma_separated_idents(Token::RPAR)
                        } else {
                            vec![]
                        };
                        self.expect(Token::EQGT);
                        let expr = self.parse_spec_expr();
                        arms.insert(constr, SourceMatchArm { constr, vars, expr });
                        match self.next_token() {
                            Some(Token::PIPE) => continue,
                            Some(Token::END) => break,
                            got => self.otok_error("expected next match case or match to end", got),
                        }
                    }
                    SourceExpr::IndMatch(discriminee, ty, arms)
                }
                Some(Token::LET) => {
                    let res = self.parse_let();
                    self.expect(Token::END);
                    res
                }
                Some(Token::PREDICATE) => {
                    let name = self.expect_ident();
                    self.expect(Token::LPAR);
                    let binds = self.parse_comma_separated_idents(Token::RPAR);
                    self.expect(Token::EQGT);
                    let expr = self.parse_spec_expr();
                    SourceExpr::PredicateBox(name, binds, expr)
                }
                Some(Token::FUN) => self.parse_lambda(None),
                Some(Token::REC) => {
                    let rbinder = self.expect_ident();
                    self.parse_lambda(Some(rbinder))
                }
                Some(Token::LPAR) => {
                    let res = self.parse_spec_expr();
                    self.expect(Token::RPAR);
                    return res;
                }
                Some(Token::IDENT(x)) => {
                    if let Some(Token::COLONCOLON) = self.peek_token() {
                        self.next_token();
                        let ident2 = self.expect_ident();
                        let spec = if let Some(Token::LT) = self.peek_token() {
                            self.next_token();
                            let r = self
                                .parse_comma_separated(Token::GT, Self::parse_optional_type_spec);
                            Some(r)
                        } else {
                            None
                        };
                        let args = if let Some(Token::LPAR) = self.peek_token() {
                            self.next_token();
                            self.parse_comma_separated(Token::RPAR, Self::parse_spec_expr)
                        } else {
                            vec![]
                        };
                        SourceExpr::IndConst(x, ident2, spec, args)
                    } else {
                        SourceExpr::Var(x)
                    }
                }
                Some(Token::NUM(x)) => SourceExpr::NumLiteral(x.into()),
                got => self.otok_error("expected (spec) expression", got),
            },
            (begin, self.endpos()),
        ))
    }

    fn parse_optional_type_annot(&mut self) -> Option<SourceTypeBox> {
        if let Some(Token::COLON) = self.peek_token() {
            self.next_token();
            Some(self.parse_source_type())
        } else {
            None
        }
    }

    fn parse_optional_type_spec(&mut self) -> Option<SourceTypeBox> {
        if let Some(Token::UNDERSCORE) = self.peek_token() {
            None
        } else {
            Some(self.parse_source_type())
        }
    }

    fn parse_lambda(&mut self, rbinder: Option<IStr>) -> SourceExpr {
        let abinder = self.expect_ident();
        let ty = self.parse_optional_type_annot();
        self.expect(Token::EQGT);
        let body = self.parse_spec_expr();
        SourceExpr::Lambda(rbinder, abinder, ty, body)
    }

    fn parse_let(&mut self) -> SourceExpr {
        let binder = self.expect_ident();
        let polyty = if let Some(Token::COLON) = self.peek_token() {
            self.next_token();
            Some(Box::new(self.parse_polyty()))
        } else {
            None
        };
        self.expect(Token::EQ);
        let expr1 = self.parse_spec_expr();
        self.expect(Token::IN);
        let expr2 = if let Some(Token::LET) = self.peek_token() {
            let begin = self.pos();
            self.next_token();
            Box::new((self.parse_let(), (begin, self.endpos())))
        } else {
            self.parse_spec_expr()
        };
        SourceExpr::Let(binder, polyty, expr1, expr2)
    }
}
