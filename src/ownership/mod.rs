use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use num_bigint::BigInt;

use crate::{
    specs::{
        InfPointer,
        exec::pointer_origin::PointerOrigins,
        values::{Value, pred_arg::PredArg},
    },
    utils::{disjount_extend::DisjointExtend, indent, string_interner::IStr},
};

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct PredicateName {
    name: IStr,
    args: Vec<PredArg>,
}

#[derive(Debug)]
pub struct OwnershipPredicate {
    output_param: Value,
    owned_preds: HashMap<PredicateName, OwnershipPredicate>,
    raw_owned: RawOwnershipInfo,
    unfold_actions: HashMap<InfPointer, HashSet<PredicateName>>,
}

#[derive(Debug)]
pub struct RawOwnershipInfo {
    allowed_accessible: HashSet<InfPointer>,
    blocks: HashSet<u32>,
}

impl PredicateName {
    pub fn new(name: IStr, args: Vec<PredArg>) -> Self {
        Self { name, args }
    }
}

impl RawOwnershipInfo {
    pub fn new() -> Self {
        Self {
            allowed_accessible: HashSet::new(),
            blocks: HashSet::new(),
        }
    }

    pub fn ensure(&self, p: &InfPointer) -> bool {
        self.allowed_accessible.contains(&p)
    }

    pub fn ensure_block(&self, p: u32) -> bool {
        self.blocks.contains(&p)
    }

    pub fn insert(&mut self, p: &InfPointer) {
        self.allowed_accessible.insert(p.clone());
    }

    pub fn insert_block(&mut self, p: u32) {
        self.blocks.insert(p);
    }

    pub fn remove(&mut self, p: &InfPointer) -> bool {
        self.allowed_accessible.remove(&p)
    }

    pub fn remove_block(&mut self, p: u32) -> bool {
        self.blocks.remove(&p)
    }

    pub(crate) fn leaks(&self) -> bool {
        !self.allowed_accessible.is_empty()
    }
}

impl DisjointExtend for RawOwnershipInfo {
    fn disjoint_extend(&mut self, other: Self) {
        self.allowed_accessible
            .disjoint_extend(other.allowed_accessible);
        self.blocks.disjoint_extend(other.blocks);
    }
}

impl OwnershipPredicate {
    pub fn new() -> Self {
        Self {
            output_param: Value::num_value(&BigInt::ZERO),
            owned_preds: HashMap::new(),
            raw_owned: RawOwnershipInfo::new(),
            unfold_actions: HashMap::new(),
        }
    }

    pub fn set_output(&mut self, output: Value) {
        self.output_param = output;
    }

    pub fn output(&self) -> Value {
        self.output_param.clone()
    }

    fn merge_predicate(&mut self, name: PredicateName) {
        let other = self.owned_preds.remove(&name).unwrap();
        self.unfold_actions.iter_mut().for_each(|(_, vs)| {
            vs.remove(&name);
        });
        self.disjoint_extend(other);
    }

    fn process_trigger(&mut self, p: &InfPointer) {
        for trigger in self
            .unfold_actions
            .remove(p)
            .into_iter()
            .flat_map(|x| x.into_iter())
        {
            self.merge_predicate(trigger);
        }
    }

    pub fn access_memory(&mut self, p: &InfPointer, is_write: bool, do_trigger: bool) -> bool {
        let res = self.access_memory_inner(p, is_write, do_trigger);
        if res && do_trigger {
            self.process_trigger(p);
        }
        res
    }

    fn access_memory_inner(&mut self, p: &InfPointer, is_write: bool, do_trigger: bool) -> bool {
        if self.raw_owned.ensure(p) {
            return true;
        }
        println!("Hitting bad path for Owned({p})!");
        for (name, pred) in self.owned_preds.iter_mut() {
            if !pred.access_memory(p, is_write, do_trigger) {
                continue;
            }
            if is_write {
                let name = name.clone();
                self.merge_predicate(name);
            }
            return true;
        }
        false
    }

    //TODO this is similar to access_memory_inner
    pub fn ensure_block(&mut self, p: u32, is_delete: bool) -> bool {
        if self.raw_owned.ensure_block(p) {
            return true;
        }
        println!("Hitting bad path for Block({p})!");
        for (name, pred) in self.owned_preds.iter_mut() {
            if !pred.ensure_block(p, is_delete) {
                continue;
            }
            if is_delete {
                let name = name.clone();
                self.merge_predicate(name);
            }
            return true;
        }
        false
    }

    pub fn lookup_predicate(&mut self, name: &PredicateName) -> Option<OwnershipPredicate> {
        let res = self.owned_preds.remove(name);
        if res.is_some() {
            self.unfold_actions.iter_mut().for_each(|(_, vs)| {
                vs.remove(name);
            });
        };
        res
    }

    pub fn save_predicate(&mut self, name: PredicateName, body: Self, triggers: PointerOrigins) {
        if triggers.is_immediate() {
            self.disjoint_extend(body);
            return;
        }
        self.owned_preds.insert(name.clone(), body);
        //assert!(x.is_none());
        for trigger in triggers.into_non_immediate_origins() {
            self.unfold_actions
                .entry(trigger)
                .or_default()
                .insert(name.clone());
        }
    }

    pub fn insert(&mut self, p: &InfPointer) {
        self.raw_owned.insert(p);
    }

    pub fn insert_block(&mut self, p: u32) {
        self.raw_owned.insert_block(p);
    }

    /// Removes ownership
    /// Does not trigger unfolding since we don't actually load from that place
    pub fn remove(&mut self, p: &InfPointer) -> bool {
        if !self.access_memory(p, true, false) {
            return false;
        }
        let b = self.raw_owned.remove(p);
        assert!(b);
        true
    }

    pub fn remove_block(&mut self, p: u32) -> bool {
        if !self.ensure_block(p, true) {
            return false;
        }
        let b = self.raw_owned.remove_block(p);
        assert!(b);
        true
    }

    pub fn leaks(&self) -> bool {
        // TODO this is not sufficient, we could be leaking predicates with non-empty footprint
        // but computing this requires walking everything
        // maybe cache this?
        self.raw_owned.leaks()
    }

    pub fn merge_upwards_for_args(&mut self, origins: &HashSet<InfPointer>) {
        origins.iter().for_each(|x| self.process_trigger(x));
    }
}

impl DisjointExtend for OwnershipPredicate {
    fn disjoint_extend(&mut self, other: Self) {
        self.raw_owned.disjoint_extend(other.raw_owned);
        self.owned_preds.disjoint_extend(other.owned_preds);
        for (ptr, preds) in other.unfold_actions.into_iter() {
            self.unfold_actions
                .entry(ptr)
                .or_default()
                .disjoint_extend(preds);
        }
    }
}

impl RawOwnershipInfo {
    pub fn pretty_print(&self, ind: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for p in &self.allowed_accessible {
            indent(ind, f)?;
            write!(f, "Owned({p})\n")?;
        }
        for p in &self.blocks {
            indent(ind, f)?;
            write!(f, "Block({p})\n")?;
        }
        Ok(())
    }
}

impl OwnershipPredicate {
    fn pretty_print(&self, ind: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        indent(ind, f)?;
        write!(f, "output: {}\n", self.output())?;
        indent(ind, f)?;
        write!(f, "raw ownership:\n")?;
        self.raw_owned.pretty_print(ind + 2, f)?;
        indent(ind, f)?;
        write!(f, "predicates:\n")?;
        for (name, body) in &self.owned_preds {
            indent(ind + 1, f)?;
            write!(f, "{}:\n", name)?;
            body.pretty_print(ind + 2, f)?;
        }
        Ok(())
    }
}

impl Display for OwnershipPredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(0, f)
    }
}

impl Display for PredicateName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.name)?;
        for arg in &self.args {
            write!(f, "{}, ", arg)?;
        }
        write!(f, ")")
    }
}
