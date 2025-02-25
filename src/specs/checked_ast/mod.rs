use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use types::{PolyType, Type};

use crate::utils::string_interner::{IStr, intern};

use super::typecheck::w::utils::TypeVar;

pub mod expr;
pub mod types;

struct TypePrettify {
    bound: HashMap<TypeVar, IStr>,
    taken: HashSet<IStr>,
    next: usize,
}

impl TypePrettify {
    fn new() -> Self {
        Self {
            bound: HashMap::new(),
            taken: [intern("'"), intern("int"), intern("ptr")]
                .into_iter()
                .collect(),
            next: 0,
        }
    }

    fn bind(&mut self, var: TypeVar, suggest: Option<IStr>) -> IStr {
        self.next += 1;
        let bsuggest = suggest.unwrap_or_else(|| intern("'"));
        let mut suggest = bsuggest;
        {
            let mut att = 1;
            while self.taken.contains(&suggest) {
                suggest = intern(format!("{bsuggest}{att}"));
                att += 1;
            }
        }
        self.taken.insert(suggest);
        self.bound.insert(var, suggest);
        suggest
    }

    fn fmt(&self, var: TypeVar, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bound.get(&var) {
            Some(x) => write!(f, "{x}"),
            None => write!(f, "??@{}", var.number()),
        }
    }
}

impl Type {
    fn prettyprint(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        pretty: &TypePrettify,
        left: bool,
    ) -> std::fmt::Result {
        match self {
            Type::Inductive(istr, items) => {
                write!(f, "{istr}<")?;
                let mut comma = false;
                for item in items {
                    if comma {
                        write!(f, ", ")?;
                    } else {
                        comma = true;
                    }
                    item.prettyprint(f, pretty, false)?;
                }
                write!(f, ">")
            }
            Type::TypeVar(type_var) => pretty.fmt(*type_var, f),
            Type::Arrow(l, r) => {
                if left {
                    write!(f, "(")?;
                }
                l.prettyprint(f, pretty, true)?;
                write!(f, " -> ")?;
                r.prettyprint(f, pretty, false)?;
                if left {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Type::Builtin(b) => match b {
                types::Builtin::Int => write!(f, "int"),
                types::Builtin::Ptr => write!(f, "ptr"),
            },
        }
    }
}

impl Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.binders.names.len() != 0 {
            write!(f, "forall")?;
        }
        let mut pretty = TypePrettify::new();
        for b in &self.binders.names {
            let name = pretty.bind(b.0, b.1);
            write!(f, " {name}")?;
        }
        if self.binders.names.len() != 0 {
            write!(f, ", ")?;
        }
        self.ty.prettyprint(f, &pretty, false)
    }
}
