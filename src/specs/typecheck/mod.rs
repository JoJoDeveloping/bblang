use std::rc::Rc;

use error::{Result, TypeError};
use w::GlobalCtx;

use crate::{specs::exec::ExecCtx, utils::string_interner::intern};

use super::{
    checked_ast::expr::ConstDef,
    exec::Value,
    source_ast::{
        SourceConstDef, SourceConstructor, SourceDef, SourceExpr, SourceGenerics, SourceInductive,
        SourceInductives, SourceMatchArm, SourcePolyType, SourceType,
    },
};

pub mod error;
pub mod example;
pub mod exprs;
pub mod types;
pub mod w;

impl GlobalCtx {
    pub fn check_def(&mut self, def: SourceDef) -> Result<Option<ConstDef>> {
        match def {
            SourceDef::Inductives(ind) => self.check_inductives(ind).map(|_| None),
            SourceDef::Const(cnst) => self.check_constant(cnst).map(Some),
        }
    }

    pub fn check_defs(&mut self, defs: Vec<SourceDef>) -> Result<Vec<ConstDef>> {
        let mut res = Vec::new();
        for def in defs {
            match self.check_def(def)? {
                None => {}
                Some(x) => res.push(x),
            }
        }
        Ok(res)
    }
}
