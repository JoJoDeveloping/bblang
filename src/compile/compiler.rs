use std::collections::{BTreeSet, HashMap};

use crate::{
    progs::ast::{self, BasicBlock, DebugInfo, Operand, Statement, Terminator},
    utils::string_interner::IStr,
};

use super::highast::{Expr, Function, Program, Span};

struct IncompleteBB {
    insns: Vec<ast::Statement>,
    term: Option<ast::Terminator>,
}

struct Temporaries {
    baseline: u32,
    max_alloc: u32,
    available: BTreeSet<u32>,
}

impl Temporaries {
    pub fn new(baseline: u32) -> Self {
        Self {
            baseline,
            max_alloc: 0,
            available: BTreeSet::new(),
        }
    }

    pub fn alloc(&mut self) -> u32 {
        self.baseline
            .checked_add(if let Some(x) = self.available.pop_first() {
                x
            } else {
                let res = self.max_alloc;
                self.max_alloc += 1;
                res
            })
            .unwrap()
    }

    pub fn dealloc(&mut self, temp: u32) {
        let temp = temp.checked_sub(self.baseline).unwrap();
        if !self.available.insert(temp) {
            panic!("temporary removed twice")
        }
    }

    pub fn is_empty(&self) -> bool {
        self.max_alloc as usize == self.available.len()
    }
}

struct CompilationContext {
    fun_name: IStr,
    locals: HashMap<IStr, u32>,
    temps: Temporaries,
    current_block: usize,
    blocks: Vec<IncompleteBB>,
    current_span: Span,
    debug_info: DebugInfo,
}

impl CompilationContext {
    pub fn new(fun: &Function) -> CompilationContext {
        let mut locals = HashMap::new();
        let mut debug_info = DebugInfo::default();
        for i in fun.args.iter().chain(fun.locals.iter()) {
            let num = locals.len().try_into().unwrap();
            if locals.insert(*i, num).is_some() {
                panic!("Local {i} appears twice!")
            }
            debug_info.local_names.insert(num, *i);
        }
        let num_locals = locals.len().try_into().unwrap();
        Self {
            fun_name: fun.name,
            locals,
            temps: Temporaries::new(num_locals),
            current_block: 0,
            blocks: vec![IncompleteBB::new()],
            current_span: fun.span,
            debug_info: debug_info,
        }
    }

    pub fn new_temp(&mut self) -> u32 {
        self.temps.alloc()
    }

    pub fn finish_temp(&mut self, x: u32) {
        self.temps.dealloc(x);
    }

    pub fn index_of_local(&self, local: IStr) -> u32 {
        *self
            .locals
            .get(&local)
            .expect(&format!("local {local} to exist!"))
    }

    pub fn new_block(&mut self) -> usize {
        let idx = self.blocks.len();
        self.blocks.push(IncompleteBB::new());
        idx
    }

    pub fn push_stmt(&mut self, s: Statement) {
        assert!(self.blocks[self.current_block].term.is_none());
        let offset = self.blocks[self.current_block].insns.len();
        self.blocks[self.current_block].insns.push(s);
        self.debug_info
            .data
            .insert((self.current_block, offset), self.current_span);
    }

    pub fn finish_block(&mut self, t: Terminator) {
        assert!(self.blocks[self.current_block].term.is_none());
        let offset = self.blocks[self.current_block].insns.len();
        self.blocks[self.current_block].term = Some(t);
        self.debug_info
            .data
            .insert((self.current_block, offset), self.current_span);
    }

    pub fn switch_block(&mut self, new_block: usize) {
        assert!(new_block < self.blocks.len());
        self.current_block = new_block;
    }

    pub fn build(self) -> ast::Function {
        assert!(self.temps.is_empty());
        ast::Function {
            name: self.fun_name,
            blocks: self.blocks.into_iter().map(IncompleteBB::build).collect(),
            debug_info: self.debug_info,
        }
    }

    pub fn set_current_span(&mut self, span: Span) -> Span {
        let old = self.current_span;
        self.current_span = span;
        old
    }
}

impl Function {
    pub fn compile(self) -> ast::Function {
        let mut cc = CompilationContext::new(&self);
        let temp = cc.new_temp();
        self.code.compile(&mut cc, temp);
        cc.finish_block(Terminator::Return(Operand::Local(temp)));
        cc.finish_temp(temp);
        cc.build()
    }
}

impl IncompleteBB {
    pub fn new() -> Self {
        IncompleteBB {
            insns: Vec::new(),
            term: None,
        }
    }

    pub fn build(self) -> BasicBlock {
        BasicBlock {
            insns: self.insns,
            term: self.term.unwrap(),
        }
    }
}

trait Compileable {
    fn compile(&self, cc: &mut CompilationContext, target: u32);
}

impl Compileable for (Expr, Span) {
    fn compile(&self, cc: &mut CompilationContext, target: u32) {
        let cs = cc.set_current_span(self.1);
        self.0.compile(cc, target);
        cc.set_current_span(cs);
    }
}

impl Expr {
    fn compile(&self, cc: &mut CompilationContext, target: u32) {
        match self {
            Expr::Const(value) => cc.push_stmt(Statement::UnOp(
                target,
                ast::UnOp::Move,
                Operand::Const(*value),
            )),
            Expr::ReadLocal(istr) => cc.push_stmt(Statement::UnOp(
                target,
                ast::UnOp::Move,
                Operand::Local(cc.index_of_local(*istr)),
            )),
            Expr::SetLocal(local, expr) => {
                expr.compile(cc, cc.index_of_local(*local));
            }
            Expr::Negate(expr) => {
                expr.compile(cc, target);
                cc.push_stmt(Statement::UnOp(
                    target,
                    ast::UnOp::Negate,
                    Operand::Local(target),
                ));
            }
            Expr::BitNot(expr) => {
                expr.compile(cc, target);
                cc.push_stmt(Statement::UnOp(
                    target,
                    ast::UnOp::Negate,
                    Operand::Local(target),
                ));
            }
            Expr::Load(expr) => {
                expr.compile(cc, target);
                cc.push_stmt(Statement::Load(target, Operand::Local(target)));
            }
            Expr::Alloc(expr) => {
                expr.compile(cc, target);
                cc.push_stmt(Statement::Alloc(target, Operand::Local(target)));
            }
            Expr::Free(expr) => {
                expr.compile(cc, target);
                cc.push_stmt(Statement::Free(Operand::Local(target)));
            }
            Expr::BinOp(expr, bin_op, expr1) => {
                let res1 = cc.new_temp();
                expr.compile(cc, res1);
                expr1.compile(cc, target);
                cc.push_stmt(Statement::BinOp(
                    target,
                    *bin_op,
                    Operand::Local(res1),
                    Operand::Local(target),
                ));
                cc.finish_temp(res1);
            }
            Expr::Store(expr, expr1) => {
                let res1 = cc.new_temp();
                expr.compile(cc, res1);
                expr1.compile(cc, target);
                cc.push_stmt(Statement::Store(
                    Operand::Local(res1),
                    Operand::Local(target),
                ));
                cc.finish_temp(res1);
            }
            Expr::Call(fnname, args) => {
                let mut temps = vec![];
                while temps.len() < args.len() {
                    temps.push(cc.new_temp());
                }
                for (arg, target) in args.iter().zip(temps.iter()) {
                    arg.compile(cc, *target);
                }
                cc.push_stmt(Statement::FunCall(
                    Some(target),
                    *fnname,
                    temps.iter().copied().map(Operand::Local).collect(),
                ));
                for x in temps.into_iter() {
                    cc.finish_temp(x);
                }
            }
            Expr::If(expr, expr1, expr2) => {
                expr.compile(cc, target);
                let then_blk = cc.new_block();
                let else_blk = cc.new_block();
                let join_block = cc.new_block();
                cc.finish_block(Terminator::CondJump(
                    Operand::Local(target),
                    then_blk,
                    else_blk,
                ));
                cc.switch_block(then_blk);
                expr1.compile(cc, target);
                cc.finish_block(Terminator::Jump(join_block));
                cc.switch_block(else_blk);
                expr2.compile(cc, target);
                cc.finish_block(Terminator::Jump(join_block));
                cc.switch_block(join_block);
            }
            Expr::While(expr, expr1) => {
                let loop_head_block = cc.new_block();
                let loop_end_block = cc.new_block();
                let loop_continue_block = cc.new_block();
                cc.finish_block(Terminator::Jump(loop_head_block));
                cc.switch_block(loop_head_block);
                expr.compile(cc, target);
                cc.finish_block(Terminator::CondJump(
                    Operand::Local(target),
                    loop_continue_block,
                    loop_end_block,
                ));
                cc.switch_block(loop_continue_block);
                expr1.compile(cc, target);
                cc.finish_block(Terminator::Jump(loop_head_block));
                cc.switch_block(loop_end_block);
            }
            Expr::Seq(expr, expr1) => {
                expr.compile(cc, target);
                expr1.compile(cc, target);
            }
            Expr::Return(expr) => {
                expr.compile(cc, target);
                cc.finish_block(Terminator::Return(Operand::Local(target)));
                let nb = cc.new_block();
                cc.switch_block(nb);
            }
        }
    }
}

impl Program {
    pub fn compile(self) -> ast::Program {
        ast::Program {
            funcs: self
                .functions
                .into_iter()
                .map(|(x, y)| (x, y.compile()))
                .collect(),
        }
    }
}
