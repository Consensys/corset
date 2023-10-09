use std::collections::HashSet;

use crate::{column::Computation, compiler::ColumnRef};

#[derive(Default, Debug)]
pub(crate) struct ComputationDag {
    nodes: HashSet<ColumnRef>,
    edges: HashSet<(ColumnRef, ColumnRef)>,
}

impl ComputationDag {
    pub fn from_computations<'a, I: Iterator<Item = &'a Computation>>(comps: I) -> ComputationDag {
        let mut r = ComputationDag::default();
        for c in comps {
            r.insert_computation(c);
        }
        r
    }

    pub fn depends(&mut self, n1: &ColumnRef, n2: &ColumnRef) {
        self.nodes.insert(n1.to_owned());
        self.nodes.insert(n2.to_owned());
        self.edges.insert((n1.clone(), n2.clone()));
    }

    fn sinks(&self) -> Vec<ColumnRef> {
        self.nodes
            .iter()
            .filter(|n| self.outgoing(n).is_empty())
            .cloned()
            .collect()
    }

    fn incoming(&self, n: &ColumnRef) -> HashSet<ColumnRef> {
        self.edges
            .iter()
            .filter(|(_, o)| o == n)
            .map(|(from, _)| from.clone())
            .collect()
    }

    fn outgoing(&self, n: &ColumnRef) -> HashSet<ColumnRef> {
        self.edges
            .iter()
            .filter(|(o, _)| o == n)
            .map(|(_, to)| to.clone())
            .collect()
    }

    pub fn insert_computation(&mut self, c: &Computation) {
        match c {
            Computation::Composite { target, exp } => {
                // There is no guarantee here that the target will be included
                // by the dependency mechanism, as it may have none in this
                // computation type (e.g. TARGET = 3 × 4)
                self.nodes.insert(target.clone());
                for from in exp.dependencies() {
                    self.depends(&from, target);
                }
            }
            Computation::Interleaved { target, froms } => {
                for from in froms.iter() {
                    self.depends(from, target);
                }
            }
            Computation::Sorted { froms, tos, .. } => {
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
            Computation::ExoAddition { sources, target }
            | Computation::ExoMultiplication { sources, target } => {
                for source in sources.iter().flat_map(|s| s.dependencies()) {
                    self.depends(&source, target);
                }
            }
            Computation::ExoConstant { .. } => {}
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
    pub fn job_slices(&self) -> Vec<HashSet<ColumnRef>> {
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
