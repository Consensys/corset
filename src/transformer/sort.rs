use anyhow::{bail, Result};
use pairing_ce::ff::PrimeField;

use crate::{
    column::Computation,
    compiler::{Builtin, Constraint, ConstraintSet, Handle, Kind, Magma, Node, Type},
};

use super::create_column;

fn create_sort_constraint(
    cs: &mut ConstraintSet,
    from: &[Handle],
    sorted: &[Handle],
) -> Result<()> {
    if from.len() != sorted.len() {
        bail!("different lengths found while creating sort constraints");
    }
    let module = &from[0].module;

    let signs = std::iter::repeat(true).take(from.len()).collect::<Vec<_>>();
    // the suffix is required, in case a single module contains multiple sorts
    let suffix = format!(
        "{:x}",
        md5::compute(
            sorted
                .iter()
                .map(|t| t.mangled_name())
                .collect::<Vec<_>>()
                .join("_"),
        )
    );

    // Create the columns
    let ats = (0..from.len())
        .map(|i| {
            create_column(
                module,
                &format!("__SRT__at_{i}_{suffix}"),
                cs,
                Kind::Phantom,
                Type::Column(Magma::Boolean),
            )
            .map(|x| x.0)
        })
        .collect::<Result<Vec<_>>>()?;
    let eq = create_column(
        module,
        &format!("__SRT__Eq_{suffix}"),
        cs,
        Kind::Phantom,
        Type::Column(Magma::Boolean),
    )?
    .0;
    let delta = create_column(
        module,
        &format!("__SRT__Delta_{suffix}"),
        cs,
        Kind::Phantom,
        Type::Column(Magma::Integer),
    )?
    .0;
    let delta_bytes = (0..16)
        .map(|i| {
            create_column(
                module,
                &format!("__SRT__Delta_{i}_{suffix}"),
                cs,
                Kind::Phantom,
                Type::Column(Magma::Byte),
            )
            .map(|x| x.0)
        })
        .collect::<Result<Vec<_>>>()?;

    // Create the binarity constraints
    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(&module, "{eq}-binary"),
        domain: None,
        expr: Box::new(Builtin::Mul.call(&[
            Node::from_typed_handle(&eq, Type::Column(Magma::Boolean)),
            Builtin::Sub.call(&[
                Node::from_const(1),
                Node::from_typed_handle(&eq, Type::Column(Magma::Boolean)),
            ]),
        ])),
    });
    for at in ats.iter() {
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new(&module, format!("{at}-binary")),
            domain: None,
            expr: Box::new(Builtin::Mul.call(&[
                Node::from_typed_handle(at, Type::Column(Magma::Boolean)),
                Builtin::Sub.call(&[
                    Node::from_const(1),
                    Node::from_typed_handle(at, Type::Column(Magma::Boolean)),
                ]),
            ])),
        });
    }

    // Create the byte decomposition constraint
    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(&module, format!("{delta}-binary")),
        domain: None,
        expr: Box::new(
            Builtin::Sub.call(&[
                Node::from_handle(&delta),
                Builtin::Add.call(
                    &delta_bytes
                        .iter()
                        .enumerate()
                        .map(|(i, byte)| {
                            Builtin::Mul.call(&[
                                Node::from_const(256_isize.pow(i as u32)),
                                Node::from_handle(byte),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
            ]),
        ),
    });

    // Create the bytehood constraint
    for delta_byte in delta_bytes.iter() {
        cs.constraints.push(Constraint::InRange {
            handle: Handle::new(module, format!("{delta_byte}-byte")),
            exp: Node::from_handle(delta_byte),
            max: pairing_ce::bn256::Fr::from_str("255").unwrap(),
        })
    }

    // Create the @ constraints
    for (i, at) in ats.iter().enumerate() {
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new(module, format!("{at}-0")),
            domain: None,
            expr: Box::new(
                Builtin::Mul.call(&[
                    // ∑_k=0^i-1 @_k = 0...
                    Builtin::Sub.call(&[
                        Node::from_const(1),
                        Builtin::Add.call(
                            &(0..i)
                                .map(|j| Node::from_handle(&ats[j]))
                                .collect::<Vec<_>>(),
                        ),
                    ]),
                    // && @ = 0 ...
                    Builtin::Sub.call(&[Node::from_const(1), Node::from_handle(at)]),
                    // => sorted_i = sorted_i[-1]
                    Builtin::Sub.call(&[
                        Node::from_handle(&sorted[i]),
                        Builtin::Shift.call(&[Node::from_handle(&sorted[i]), Node::from_const(-1)]),
                    ]),
                ]),
            ),
        });
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new(module, format!("{at}-1")),
            domain: None,
            expr: Box::new(
                Builtin::Mul.call(&[
                    // ∑_k=0^i-1 @_k = 0...
                    Builtin::Sub.call(&[
                        Node::from_const(1),
                        Builtin::Add.call(
                            &(0..i)
                                .map(|j| Node::from_handle(&ats[j]))
                                .collect::<Vec<_>>(),
                        ),
                    ]),
                    // && @ ≠ 0
                    Node::from_handle(at),
                    // => sorted_i ≠ sorted_i[-1]
                    {
                        let diff = Builtin::Sub.call(&[
                            Node::from_handle(&sorted[i]),
                            Builtin::Shift
                                .call(&[Node::from_handle(&sorted[i]), Node::from_const(-1)]),
                        ]);
                        let diff_inv = Builtin::Inv.call(&[diff.clone()]);

                        Builtin::Sub
                            .call(&[Node::from_const(1), Builtin::Mul.call(&[diff, diff_inv])])
                    },
                ]),
            ),
        });
    }

    // Create the Eq + ∑@ = 1 (i.e. Eq = 1 XOR ∑@ = 1)
    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(module, format!("Eq_@_{suffix}")),
        domain: None,
        expr: Box::new(
            Builtin::Sub.call(&[
                Node::from_const(1),
                Builtin::Add.call(
                    &vec![Node::from_handle(&eq)]
                        .into_iter()
                        .chain(ats.iter().map(Node::from_handle))
                        .collect::<Vec<_>>(),
                ),
            ]),
        ),
    });

    // Create the Eq[i] = 0 constraint
    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(module, format!("__SRT__Eq_i_{suffix}")),
        domain: None,
        expr: Box::new(
            Builtin::Mul.call(&[
                // Eq = 0
                Builtin::Sub.call(&[Node::from_const(1), Node::from_handle(&eq)]),
                // Δ = ∑ ε_i × @_i × δSorted_i
                Builtin::Sub.call(&[
                    Node::from_handle(&delta),
                    Builtin::Add.call(
                        (0..from.len())
                            .map(|l| {
                                let tgt_diff = Builtin::Sub.call(&[
                                    Node::from_handle(&sorted[l]),
                                    Builtin::Shift.call(&[
                                        Node::from_handle(&sorted[l]),
                                        Node::from_const(-1),
                                    ]),
                                ]);
                                Builtin::Mul.call(&[
                                    if !signs[l] {
                                        Node::from_const(-1)
                                    } else {
                                        Node::from_const(1)
                                    },
                                    Node::from_handle(&ats[l]),
                                    tgt_diff,
                                ])
                            })
                            .collect::<Vec<_>>()
                            .as_ref(),
                    ),
                ]),
            ]),
        ),
    });

    // Add the required computation
    cs.computations.insert_many(
        &ats,
        Computation::SortingConstraints {
            ats: ats.clone(),
            eq,
            delta,
            delta_bytes,
            signs: signs.to_vec(),
            froms: from.to_vec(),
            sorted: sorted.to_vec(),
        },
    )?;

    Ok(())
}

pub fn sorts(cs: &mut ConstraintSet) -> Result<()> {
    for c in cs
        .computations
        .iter()
        .cloned()
        .collect::<Vec<_>>()
        .into_iter()
    {
        if let Computation::Sorted { froms, tos } = c {
            create_sort_constraint(cs, &froms, &tos)?;
        }
    }
    Ok(())
}
