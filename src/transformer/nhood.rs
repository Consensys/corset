use anyhow::{bail, Result};
use owo_colors::OwoColorize;
use std::collections::HashMap;

use crate::{
    column::{Column, Computation},
    compiler::{
        ColumnRef, Constraint, ConstraintSet, Domain, Intrinsic, Kind, Magma, Node, RawMagma,
    },
    pretty::Base,
    structs::Handle,
};

fn process_binarity(column_refs: &[ColumnRef], cs: &mut ConstraintSet) {
    for column_ref in column_refs {
        let handle = cs.handle(column_ref);
        let x = Node::column().handle(column_ref.clone()).build();
        cs.insert_constraint(Constraint::Vanishes {
            handle: Handle::new(handle.module.clone(), format!("{}-binarity", handle.name)),
            domain: None,
            expr: Box::new(
                Intrinsic::Mul
                    .call(&[
                        x.clone(),
                        Intrinsic::Sub
                            .call(&[Node::from_const(1), x.clone()])
                            .unwrap(),
                    ])
                    .unwrap(),
            ),
        })
    }
}

fn process_nhood(
    module: &str,
    handles: &[ColumnRef],
    modulo: usize,
    cs: &mut ConstraintSet,
) -> Result<()> {
    let _aux_id = cs.columns.insert_column_and_register(
        Column::builder()
            .handle(Handle::new(module, format!("AUX_{modulo}_HOOD")))
            .kind(Kind::Phantom)
            .t(Magma::native())
            .build(),
    )?;
    cs.computations.insert(
        &_aux_id,
        Computation::CyclicFrom {
            target: _aux_id.clone(),
            froms: handles.to_vec(),
            modulo,
        },
    )?;

    let interleaving = vec![_aux_id]
        .into_iter()
        .chain(handles.iter().cloned())
        .collect::<Vec<_>>();
    let _intrld_aux_xs_id = cs.columns.insert_column_and_register(
        Column::builder()
            .handle(Handle::new(module, format!("INTRLD_AUX_{modulo}_HOOD")))
            .kind(Kind::Phantom)
            .build(),
    )?;
    cs.computations.insert(
        &_intrld_aux_xs_id,
        Computation::Interleaved {
            target: _intrld_aux_xs_id.clone(),
            froms: interleaving,
        },
    )?;

    let srt_intrld_aux_xs_id = cs.columns.insert_column_and_register(
        Column::builder()
            .handle(Handle::new(module, format!("SRT_INTRLD_AUX_{modulo}_HOOD")))
            .kind(Kind::Phantom)
            .build(),
    )?;
    cs.computations.insert(
        &srt_intrld_aux_xs_id,
        Computation::Sorted {
            froms: vec![_intrld_aux_xs_id.clone()],
            tos: vec![srt_intrld_aux_xs_id.clone()],
            signs: vec![true],
        },
    )?;
    cs.insert_constraint(Constraint::Permutation {
        handle: Handle::new(
            module,
            format!("{} perm. {}", _intrld_aux_xs_id, srt_intrld_aux_xs_id),
        ),
        from: vec![_intrld_aux_xs_id],
        to: vec![srt_intrld_aux_xs_id.clone()],
    });

    let srt_intrld_aux_xs_node = Node::column()
        .handle(srt_intrld_aux_xs_id.clone())
        .kind(Kind::Phantom)
        .base(Base::Dec)
        .t(Magma::byte())
        .build();

    cs.insert_constraint(Constraint::Vanishes {
        handle: Handle::new(module, format!("{modulo}-hood-start")),
        domain: Some(Domain::Set(vec![0])),
        expr: Box::new(srt_intrld_aux_xs_node.clone()),
    });

    cs.insert_constraint(Constraint::Vanishes {
        handle: Handle::new(module, format!("{modulo}-hood-end")),
        domain: Some(Domain::Set(vec![-1])),
        expr: Box::new(
            Intrinsic::Sub.call(&[
                Node::column()
                    .handle(srt_intrld_aux_xs_id.clone())
                    .kind(Kind::Phantom)
                    .base(Base::Dec)
                    .build(),
                Node::from_const(modulo.try_into().unwrap()),
            ])?,
        ),
    });

    cs.insert_constraint(Constraint::Vanishes {
        handle: Handle::new(module, format!("{modulo}-hood-middle")),
        domain: None,
        expr: Box::new(Intrinsic::Mul.call(&[
            // SRT_ILD_[i+1] - SRT_ILD_[i]
            Intrinsic::Sub.call(&[
                srt_intrld_aux_xs_node.clone().shift(1),
                srt_intrld_aux_xs_node.clone(),
            ])?,
            // SRT_ILD_[i+1] - (SRT_ILD_[i] + 1)
            Intrinsic::Sub.call(&[
                srt_intrld_aux_xs_node.clone().shift(1),
                Intrinsic::Add.call(&[srt_intrld_aux_xs_node, Node::from_const(1)])?,
            ])?,
        ])?),
    });

    Ok(())
}

pub fn validate_nhood(cs: &mut ConstraintSet) -> Result<()> {
    let mut binary_columns = Vec::new();
    let mut constrained_columns = HashMap::<String, HashMap<u32, Vec<ColumnRef>>>::new();

    for (h, c) in cs.columns.iter() {
        // only atomic columns (i.e. filled from traces) are of interest here
        if c.kind == Kind::Atomic && c.must_prove {
            match c.t.rm() {
                RawMagma::Binary => binary_columns.push(h.clone()),
                _ => constrained_columns
                    .entry(c.handle.module.to_owned())
                    .or_default()
                    .entry(c.t.bit_size() as u32)
                    .or_default()
                    .push(h.clone()),
            }
        }
    }

    // Binary columns are a special case. As they only generate a single
    // constraint and *do not create new columns* in their module.
    process_binarity(&binary_columns, cs);

    for (module, columns) in dbg!(constrained_columns).iter() {
        for (&bit_size, handles) in columns.iter() {
            if bit_size > 16 {
                bail!(
                    "do you really want to prove a {}-bits integer?",
                    bit_size.yellow().bold()
                );
            }
            let modulo = 2usize.pow(bit_size) - 1;
            process_nhood(module, handles, modulo, cs)?;
            cs.columns.set_min_len(module, modulo);
        }
    }

    Ok(())
}
