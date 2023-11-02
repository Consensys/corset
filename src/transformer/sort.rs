use anyhow::{anyhow, bail, Context, Result};
use num_bigint::BigInt;

use crate::{
    column::{Column, Computation, Value},
    compiler::{ColumnRef, Constraint, ConstraintSet, Intrinsic, Kind, Magma, Node},
    pretty::{Base, Pretty},
    structs::Handle,
};

fn create_sort_constraint(
    cs: &mut ConstraintSet,
    froms: &[ColumnRef],
    sorted: &[ColumnRef],
    signs: &[bool],
) -> Result<()> {
    if froms.len() != sorted.len() {
        bail!(
            "unable to create {} sorted columns from {} columns",
            sorted.len(),
            froms.len()
        );
    }
    let module = cs.columns.column(&froms[0]).unwrap().handle.module.clone();
    for from in froms {
        let from_col = cs.columns.column(from).unwrap();
        if from_col.handle.module != *module {
            bail!(
                "column {} does not belong to the same module as {}",
                from_col.handle.pretty(),
                froms[0].pretty()
            )
        }
    }

    if signs.is_empty() {
        bail!("no sorting criterion specified")
    }
    if signs.len() > froms.len() {
        bail!("found more sorting orders than columns to sort")
    }

    // the suffix is required, in case a single module contains multiple sorts
    let mut suffix = format!(
        "{:x}",
        &md5::compute(
            sorted
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join("_"),
        )
    );
    suffix.truncate(6);

    // Create the columns
    let ats = (0..signs.len())
        .map(|i| {
            let size = cs.length_multiplier(&froms[0]);
            cs.columns.insert_column_and_register(
                Column::builder()
                    .handle(Handle::new(&module, format!("__SRT__at_{i}_{suffix}")))
                    .kind(Kind::Phantom)
                    .t(Magma::binary())
                    .intrinsic_size_factor(size)
                    .build(),
            )
        })
        .collect::<Result<Vec<_>>>()?;
    let eq_size = cs.length_multiplier(&froms[0]);
    let eq = cs.columns.insert_column_and_register(
        Column::builder()
            .handle(Handle::new(&module, format!("__SRT__Eq_{suffix}")))
            .t(Magma::binary())
            .intrinsic_size_factor(eq_size)
            .kind(Kind::Phantom)
            .padding_value(1)
            .build(),
    )?;
    let delta = cs.columns.insert_column_and_register(
        Column::builder()
            .handle(Handle::new(&module, format!("__SRT__Delta_{suffix}")))
            .kind(Kind::Phantom)
            .intrinsic_size_factor(cs.length_multiplier(&froms[0]))
            .base(Base::Hex)
            .build(),
    )?;
    let delta_bytes = (0..16)
        .map(|i| {
            cs.columns.insert_column_and_register(
                Column::builder()
                    .handle(Handle::new(&module, format!("__SRT__Delta_{i}_{suffix}")))
                    .kind(Kind::Phantom)
                    .t(Magma::byte())
                    .intrinsic_size_factor(cs.length_multiplier(&froms[0]))
                    .build(),
            )
        })
        .collect::<Result<Vec<_>>>()?;

    // Create the binarity constraints
    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(&module, format!("{}-is-binary", cs.handle(&eq).name)),
        domain: None,
        expr: Box::new(Intrinsic::Mul.call(&[
            Node::column().handle(eq.clone()).t(Magma::binary()).build(),
            Intrinsic::Sub.call(&[
                Node::from_const(1),
                Node::column().handle(eq.clone()).t(Magma::binary()).build(),
            ])?,
        ])?),
    });
    for at in ats.iter() {
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new(&module, format!("{}-is-binary", cs.handle(at).name)),
            domain: None,
            expr: Box::new(Intrinsic::Mul.call(&[
                Node::column().handle(at.clone()).t(Magma::binary()).build(),
                Intrinsic::Sub.call(&[
                    Node::from_const(1),
                    Node::column().handle(at.clone()).t(Magma::binary()).build(),
                ])?,
            ])?),
        });
    }

    // Create the byte decomposition constraint
    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(&module, format!("{}-is-binary", cs.handle(&delta).name)),
        domain: None,
        expr: Box::new(
            Intrinsic::Sub.call(&[
                Node::column()
                    .handle(delta.clone())
                    .t(Magma::native())
                    .build(),
                Intrinsic::Add.call(
                    &delta_bytes
                        .iter()
                        .enumerate()
                        .map(|(i, byte)| {
                            Intrinsic::Mul.call(&[
                                Node::from_bigint(BigInt::from(256).pow(i as u32)),
                                Node::column().handle(byte.clone()).t(Magma::byte()).build(),
                            ])
                        })
                        .collect::<Result<Vec<_>>>()?,
                )?,
            ])?,
        ),
    });

    // Create the bytehood constraints
    for delta_byte in delta_bytes.iter() {
        cs.constraints.push(Constraint::InRange {
            handle: Handle::new(&module, format!("{}-is-byte", cs.handle(delta_byte).name)),
            exp: Node::column()
                .handle(delta_byte.clone())
                .t(Magma::byte())
                .build(),
            max: Value::from(256),
        })
    }

    // // Create the @ constraints
    for (i, at) in ats.iter().enumerate() {
        // ∑_k=0^i-1 @_k = 0...
        let sum_ats = if i > 0 {
            Intrinsic::Sub.call(&[
                Node::from_const(1),
                Intrinsic::Add.call(
                    &(0..i)
                        .map(|j| {
                            Node::column()
                                .handle(ats[j].clone())
                                .t(Magma::binary())
                                .build()
                        })
                        .collect::<Vec<_>>(),
                )?,
            ])?
        } else {
            // meaningless branch required for @_0
            Node::from_const(1)
        };

        let sorted_t = cs.columns.column(&sorted[i])?.t;
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new(&module, format!("{at}-0")),
            domain: None,
            expr: Box::new(
                Intrinsic::Mul.call(&[
                    // ∑_k=0^i-1 @_k = 0...
                    sum_ats.clone(),
                    // && @ = 0 ...
                    Intrinsic::Sub.call(&[
                        Node::from_const(1),
                        Node::column().handle(at.clone()).t(Magma::binary()).build(),
                    ])?,
                    // => sorted_i = sorted_i[-1]
                    Intrinsic::Sub.call(&[
                        Node::column().handle(sorted[i].clone()).t(sorted_t).build(),
                        Node::column()
                            .handle(sorted[i].clone())
                            .t(sorted_t)
                            .shift(-1)
                            .build(),
                    ])?,
                ])?,
            ),
        });
        cs.constraints.push(Constraint::Vanishes {
            handle: Handle::new(&module, format!("{at}-1")),
            domain: None,
            expr: Box::new(Intrinsic::Mul.call(&[
                // ∑_k=0^i-1 @_k = 0...
                sum_ats.clone(),
                // && @ ≠ 0
                Node::column().handle(at.clone()).t(Magma::binary()).build(),
                // => sorted_i ≠ sorted_i[-1]
                {
                    let diff = Intrinsic::Sub.call(&[
                        Node::column().handle(sorted[i].clone()).t(sorted_t).build(),
                        Node::column()
                            .handle(sorted[i].clone())
                            .t(sorted_t)
                            .shift(-1)
                            .build(),
                    ])?;
                    let diff_inv = Intrinsic::Inv.call(&[diff.clone()])?;

                    Intrinsic::Sub
                        .call(&[Node::from_const(1), Intrinsic::Mul.call(&[diff, diff_inv])?])?
                },
            ])?),
        });
    }

    // // Create the Eq + ∑@ = 1 (i.e. Eq = 1 XOR ∑@ = 1)
    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(&module, format!("Eq_@_{suffix}")),
        domain: None,
        expr: Box::new(
            Intrinsic::Sub.call(&[
                Node::from_const(1),
                Intrinsic::Add.call(
                    &std::iter::once(Node::column().handle(eq.clone()).t(Magma::binary()).build())
                        .chain(
                            ats.iter()
                                .cloned()
                                .map(|at| Node::column().handle(at).t(Magma::binary()).build()),
                        )
                        .collect::<Vec<_>>(),
                )?,
            ])?,
        ),
    });

    // // Create the Eq[i] = 0 constraint
    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(&module, format!("__SRT__Eq_i_{suffix}")),
        domain: None,
        expr: Box::new(
            Intrinsic::Mul.call(&[
                // Eq = 0
                Intrinsic::Sub.call(&[
                    Node::from_const(1),
                    Node::column().handle(eq.clone()).t(Magma::binary()).build(),
                ])?,
                // Δ = ∑ ε_i × @_i × δSorted_i
                Intrinsic::Sub.call(&[
                    Node::column()
                        .handle(delta.clone())
                        .t(Magma::native())
                        .build(),
                    Intrinsic::Add.call(
                        (0..signs.len())
                            .map(|l| {
                                let sorted_l_t = cs.columns.column(&sorted[l])?.t;
                                let tgt_diff = Intrinsic::Sub.call(&[
                                    Node::column()
                                        .handle(sorted[l].clone())
                                        .t(sorted_l_t)
                                        .build(),
                                    Node::column()
                                        .handle(sorted[l].clone())
                                        .t(sorted_l_t)
                                        .shift(-1)
                                        .build(),
                                ])?;
                                Intrinsic::Mul.call(&[
                                    if !signs[l] {
                                        Node::from_const(-1)
                                    } else {
                                        Node::from_const(1)
                                    },
                                    Node::column()
                                        .handle(ats[l].clone())
                                        .t(Magma::binary())
                                        .build(),
                                    tgt_diff,
                                ])
                            })
                            .collect::<Result<Vec<_>>>()?
                            .as_ref(),
                    )?,
                ])?,
            ])?,
        ),
    });

    // Add the required computation
    cs.computations.insert_many(
        vec![eq.clone(), delta.clone()]
            .into_iter()
            .chain(ats.iter().cloned())
            .chain(delta_bytes.iter().cloned())
            .collect::<Vec<_>>()
            .as_slice(),
        Computation::SortingConstraints {
            ats: ats.clone(),
            eq,
            delta,
            delta_bytes,
            signs: signs.to_vec(),
            froms: froms.to_vec(),
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
        if let Computation::Sorted { froms, tos, signs } = c {
            create_sort_constraint(cs, &froms, &tos, &signs)
                .with_context(|| anyhow!("while creating sort constraints"))?;
        }
    }
    Ok(())
}
