use anyhow::Result;
use std::collections::HashMap;

use crate::{
    column::{Column, Computation},
    compiler::{
        ColumnRef, Constraint, ConstraintSet, Domain, Intrinsic, Kind, Magma, Node, RawMagma,
    },
    pretty::Base,
    structs::Handle,
};

fn process_nhood(
    module: &str,
    handles: &[ColumnRef],
    n: u32,
    cs: &mut ConstraintSet,
) -> Result<()> {
    let modulo = 2usize.pow(n);
    let _aux_id = cs.columns.insert_column_and_register(
        Column::builder()
            .handle(Handle::new(module, format!("AUX_2_{}_HOOD", n)))
            .kind(Kind::Phantom)
            .t(Magma::integer(n as usize + 1))
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
            .handle(Handle::new(module, format!("INTRLD_AUX_2_{}_HOOD", n)))
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
            .handle(Handle::new(module, format!("SRT_INTRLD_AUX_2_{}_HOOD", n)))
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
        handle: Handle::new(module, format!("2^{n}-hood-start")),
        domain: Some(Domain::Set(vec![0])),
        expr: Box::new(srt_intrld_aux_xs_node.clone()),
    });

    cs.insert_constraint(Constraint::Vanishes {
        handle: Handle::new(module, format!("2^{n}-hood-end")),
        domain: Some(Domain::Set(vec![-1])),
        expr: Box::new(
            Intrinsic::Sub.call(&[
                Node::column()
                    .handle(srt_intrld_aux_xs_id.clone())
                    .kind(Kind::Phantom)
                    .base(Base::Dec)
                    .build(),
                Node::from_const((modulo - 1).try_into().unwrap()),
            ])?,
        ),
    });

    cs.insert_constraint(Constraint::Vanishes {
        handle: Handle::new(module, format!("2^{n}-hood-middle")),
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
    let mut nibble_columns = HashMap::<String, Vec<ColumnRef>>::new();
    let mut byte_columns = HashMap::<String, Vec<ColumnRef>>::new();

    for (h, c) in cs.columns.iter() {
        // only atomic columns (i.e. filled from traces) are of interest here
        if c.kind == Kind::Atomic {
            match c.t.rm() {
                RawMagma::Nibble => nibble_columns
                    .entry(c.handle.module.to_owned())
                    .or_default()
                    .push(h.clone()),
                RawMagma::Byte => byte_columns
                    .entry(c.handle.module.to_owned())
                    .or_default()
                    .push(h.clone()),
                _ => (),
            }
        }
    }

    for (module, handles) in nibble_columns.iter() {
        process_nhood(module, handles, 4, cs)?;
        cs.columns.set_min_len(module, 2usize.pow(4));
    }

    for (module, handles) in byte_columns.iter() {
        process_nhood(module, handles, 8, cs)?;
        cs.columns.set_min_len(module, 2usize.pow(8));
    }

    Ok(())
}
