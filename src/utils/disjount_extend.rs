use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

pub trait DisjointExtend {
    fn disjoint_extend(&mut self, other: Self);
}

impl<K: Eq + Hash> DisjointExtend for HashSet<K> {
    fn disjoint_extend(&mut self, other: Self) {
        let newsz = self.len() + other.len();
        self.extend(other);
        // assert_eq!(newsz, self.len())
    }
}
impl<K: Eq + Hash, V> DisjointExtend for HashMap<K, V> {
    fn disjoint_extend(&mut self, other: Self) {
        let newsz = self.len() + other.len();
        self.extend(other);
        // assert_eq!(newsz, self.len())
    }
}
