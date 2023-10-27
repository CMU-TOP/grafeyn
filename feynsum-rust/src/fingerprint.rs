use std::cmp::Ordering;
use std::collections::BTreeSet;

use crate::types::{BasisIdx, Complex};

#[derive(PartialEq)]
struct Entry<B: BasisIdx>(B, Complex);

impl<B: BasisIdx> Eq for Entry<B> {}

impl<B: BasisIdx> PartialOrd for Entry<B> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // FIXME: bucket magnitude?
        if self.1.norm() < other.1.norm() {
            Some(Ordering::Less)
        } else if self.1.norm() > other.1.norm() {
            Some(Ordering::Greater)
        } else {
            self.0.into_bytes().partial_cmp(&other.0.into_bytes())
        }
    }
}

impl<B: BasisIdx> Ord for Entry<B> {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.1.norm() < other.1.norm() {
            Ordering::Less
        } else if self.1.norm() > other.1.norm() {
            Ordering::Greater
        } else {
            other.0.into_bytes().cmp(&self.0.into_bytes())
        }
    }
}

pub struct Fingerprint<B: BasisIdx> {
    num_entries: usize,
    entries: BTreeSet<Entry<B>>,
}

impl<B: BasisIdx> Fingerprint<B> {
    pub fn new(num_entries: usize) -> Self {
        Self {
            num_entries,
            entries: BTreeSet::new(),
        }
    }

    pub fn insert(&mut self, bidx: B, weight: Complex) {
        self.entries.insert(Entry(bidx, weight));
        if self.entries.len() > self.num_entries {
            self.entries.pop_first();
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (B, Complex)> + '_ {
        // reverse the comparison here so that the output fingerprint is sorted
        // like normal binary numbers, for readability
        self.entries
            .iter()
            .rev()
            .map(|Entry(bidx, weight)| (bidx.clone(), *weight))
    }
}
