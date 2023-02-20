use std::collections::HashSet;

use crate::{column::Computation, structs::Handle};

#[derive(Default, Debug)]
pub(crate) struct ComputationDag {
    nodes: HashSet<Handle>,
    edges: HashSet<(Handle, Handle)>,
}

impl ComputationDag {
    pub fn depends(&mut self, n1: &Handle, n2: &Handle) {
        self.nodes.insert(n1.to_owned());
        self.nodes.insert(n2.to_owned());
        self.edges.insert((n1.clone(), n2.clone()));
    }

    fn sinks(&self) -> Vec<Handle> {
        self.nodes
            .iter()
            .filter(|n| self.outgoing(n).is_empty())
            .cloned()
            .collect()
    }

    fn incoming(&self, n: &Handle) -> HashSet<Handle> {
        self.edges
            .iter()
            .filter(|(_, o)| o == n)
            .map(|(from, _)| from.clone())
            .collect()
    }

    fn outgoing(&self, n: &Handle) -> HashSet<Handle> {
        self.edges
            .iter()
            .filter(|(o, _)| o == n)
            .map(|(_, to)| to.clone())
            .collect()
    }

    pub fn insert_computation(&mut self, c: &Computation) {
        match c {
            Computation::Composite { target, exp } => {
                for from in exp.dependencies() {
                    self.depends(&from, target);
                }
            }
            Computation::Interleaved { target, froms } => {
                for from in froms.iter() {
                    self.depends(from, target);
                }
            }
            Computation::Sorted { froms, tos } => {
                for from in froms.iter() {
                    for to in tos.iter() {
                        self.depends(from, to);
                    }
                }
            }
            Computation::CyclicFrom { target, froms, .. } => {
                for from in froms.iter() {
                    self.depends(from, target);
                }
            }
            Computation::SortingConstraints {
                ats,
                eq,
                delta,
                delta_bytes,
                froms,
                sorted,
                ..
            } => {
                for from in froms {
                    self.depends(from, eq);
                    self.depends(from, delta);
                    for x in sorted.iter().chain(ats.iter()).chain(delta_bytes.iter()) {
                        self.depends(from, x);
                    }
                }
            }
        }
    }

    /// Returns a pseudo-topological sorting, a list of sets of independent columns
    pub fn job_slices(&self) -> Vec<HashSet<Handle>> {
        let mut r = Vec::new();
        let mut visited = HashSet::new();

        let mut current = self.sinks().into_iter().collect::<HashSet<_>>();
        loop {
            r.push(current.clone());
            visited.extend(current.iter().cloned());
            current = current
                .into_iter()
                .flat_map(|n| self.incoming(&n))
                .filter(|n| !visited.contains(n))
                .filter(|n| self.outgoing(n).iter().all(|o| visited.contains(o)))
                .collect();
            if current.is_empty() {
                break;
            }
        }

        r.reverse();
        r
    }
}
