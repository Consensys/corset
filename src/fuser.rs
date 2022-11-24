use log::*;
use petgraph::algo::{dijkstra, has_path_connecting, min_spanning_tree};
use petgraph::graph::{NodeIndex, UnGraph};
use std::collections::{HashMap, HashSet};

use crate::compiler::{Builtin, Constraint, ConstraintSet, Expression, Handle};

type Neighbourhood = HashMap<Handle, HashSet<Handle>>;

// fn dijkstra(g: &Neighbourhood, x: usize, y: usize) -> bool {
//     let mut p = HashSet::new();
//     let mut d = g.keys().map(|x| (x, None)).collect::<HashMap<_, _>>();
//     d[x] = Some(0);

// }

fn find_dependencies(e: &Expression, current: HashSet<Handle>, deps: &mut Neighbourhood) {
    match e {
        Expression::Funcall { func, args } => {
            if (matches!(func, Builtin::IfZero) || matches!(func, Builtin::IfNotZero))
                && args.len() == 3
            {
                let lefts = current
                    .difference(&args[2].dependencies())
                    .cloned()
                    .collect::<HashSet<_>>();
                let rights = current
                    .difference(&args[1].dependencies())
                    .cloned()
                    .collect::<HashSet<_>>();
                trace!("Splitting @{}", &args[0]);
                trace!("{:?}", &current);
                trace!("  <- {:?}", &lefts);
                trace!("  -> {:?}", &rights);
                find_dependencies(&args[1], lefts, deps);
                find_dependencies(&args[2], rights, deps);
            } else {
                for a in args.iter() {
                    find_dependencies(a, current.clone(), deps);
                }
            }
        }
        Expression::List(es) => {
            for e in es {
                find_dependencies(e, current.clone(), deps);
            }
        }
        _ => {
            for c1 in current.iter() {
                for c2 in current.iter() {
                    // deps.update_edge(NodeIndex::from(*c1 as u32), NodeIndex::from(*c2 as u32), ());
                    deps.entry(c1.clone()).or_default().insert(c2.clone());
                    deps.entry(c2.clone()).or_default().insert(c1.clone());
                }
            }
        }
    }
}

pub fn fuse(cs: &mut ConstraintSet) {
    let mut deps = Neighbourhood::new();
    // let mut g = UnGraph::<usize, ()>::new_undirected();

    let all_handles = cs
        .constraints
        .iter()
        .filter_map(|c| {
            if let Constraint::Vanishes { expr, .. } = c {
                Some(expr.dependencies())
            } else {
                None
            }
        })
        .flatten()
        .collect::<HashSet<_>>();

    for i1 in 0..cs.constraints.len() {
        for i2 in 0..i1 {
            if let (Constraint::Vanishes { expr: e1, .. }, Constraint::Vanishes { expr: e2, .. }) =
                (&cs.constraints[i1], &cs.constraints[i2])
            {
                let m1 = e1.dependencies();
                let m2 = e2.dependencies();

                let sd = m1
                    .symmetric_difference(&m2)
                    .cloned()
                    .collect::<HashSet<_>>();

                for v1 in m2.iter() {
                    for v2 in m1.iter() {
                        deps.entry(v1.clone()).or_default().insert(v2.clone());
                        deps.entry(v2.clone()).or_default().insert(v1.clone());
                    }
                }
            }
        }
    }
    for c in cs.constraints.iter() {
        if let Constraint::Vanishes { expr: e, .. } = c {
            find_dependencies(
                e, // all_handles.iter().map(|h| h.id.unwrap()).collect(),
                e.dependencies(),
                &mut deps,
            )
        }
    }

    dbg!(&deps);
    for h1 in all_handles.iter() {
        for h2 in all_handles.iter() {
            if h1.module == h2.module {
                if h1.id.unwrap() > h2.id.unwrap() {
                    if deps[&h1].contains(&h2)
                    // if pathfinding::directed::dijkstra::dijkstra(
                    //     &h1.id.unwrap(),
                    //     |&h| {
                    //         deps.entry(h)
                    //             .or_default()
                    //             .iter()
                    //             .map(|x| (*x, 1))
                    //             .collect::<Vec<_>>()
                    //     },
                    //     |h| *h == h2.id.unwrap(),
                    // )
                    // .is_some()
                    {
                        // debug!("{} {} exclusive", h1, h2);
                    } else {
                        warn!("{} {} may be merged", &h1.name, &h2.name);
                    }
                }
            }
        }
    }
}
