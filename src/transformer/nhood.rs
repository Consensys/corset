use anyhow::Result;
use std::collections::HashMap;

use crate::{
    column::Computation,
    compiler::{Constraint, ConstraintSet, Expression, Intrinsic, Kind, Magma, Node, Type},
    pretty::Base,
    structs::Handle,
};

fn process_nhood(module: &str, handles: &[Handle], n: u32, cs: &mut ConstraintSet) -> Result<()> {
    let modulo = 2usize.pow(n);
    let aux_handle = Handle::new(module, format!("AUX_2_{}_HOOD", n));
    let _aux_id = cs.modules.insert_column(
        &aux_handle,
        Type::Column(Magma::Integer),
        true,
        Kind::Phantom,
        false,
        None,
        None,
        Base::Bin,
    );
    cs.computations.insert(
        &aux_handle,
        Computation::CyclicFrom {
            target: aux_handle.clone(),
            froms: handles.to_vec(),
            modulo,
        },
    )?;

    let intrld_aux_xs_handle = Handle::new(module, format!("INTRLD_AUX_2_{}_HOOD", n));
    let interleaving = vec![aux_handle]
        .into_iter()
        .chain(handles.iter().cloned())
        .collect::<Vec<_>>();
    let _intrld_aux_xs_id = cs.modules.insert_column(
        &intrld_aux_xs_handle,
        Type::Column(Magma::Integer),
        true,
        Kind::Interleaved(vec![], Some(interleaving.to_owned())),
        false,
        None,
        None,
        Base::Bin,
    );
    cs.computations.insert(
        &intrld_aux_xs_handle,
        Computation::Interleaved {
            target: intrld_aux_xs_handle.clone(),
            froms: interleaving,
        },
    )?;

    let srt_intrld_aux_xs_handle = Handle::new(module, format!("SRT_INTRLD_AUX_2_{}_HOOD", n));
    let _aux_intrld_srtd_id = cs.modules.insert_column(
        &srt_intrld_aux_xs_handle,
        Type::Column(Magma::Integer),
        true,
        Kind::Phantom,
        false,
        None,
        None,
        Base::Bin,
    );
    cs.computations.insert(
        &srt_intrld_aux_xs_handle,
        Computation::Sorted {
            froms: vec![intrld_aux_xs_handle.to_owned()],
            tos: vec![srt_intrld_aux_xs_handle.to_owned()],
            signs: vec![true],
        },
    )?;

    let srt_intrld_aux_xs_node = Node {
        _e: Expression::Column {
            handle: srt_intrld_aux_xs_handle.to_owned(),
            kind: Kind::Phantom,
            padding_value: None,
            base: Base::Dec,
        },
        _t: Some(Type::Column(Magma::Byte)),
    };
    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(module, format!("2^{n}-hood-start")),
        domain: Some(vec![0]),
        expr: Box::new(srt_intrld_aux_xs_node.clone()),
    });

    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(module, format!("2^{n}-hood-end")),
        domain: Some(vec![-1]),
        expr: Box::new(Intrinsic::Sub.call(&[
            Node::from_expr(Expression::Column {
                handle: srt_intrld_aux_xs_handle.to_owned(),
                kind: Kind::Phantom,
                padding_value: None,
                base: Base::Dec,
            }),
            Node::from_const((modulo - 1).try_into().unwrap()),
        ])?),
    });

    cs.constraints.push(Constraint::Vanishes {
        handle: Handle::new(module, format!("2^{n}-hood-middle")),
        domain: None,
        expr: Box::new(Intrinsic::Mul.call(&[
            // SRT_ILD_[i+1] - SRT_ILD_[i]
            Intrinsic::Sub.call(&[
                Intrinsic::Shift.call(&[srt_intrld_aux_xs_node.clone(), Node::from_const(1)])?,
                srt_intrld_aux_xs_node.clone(),
            ])?,
            // SRT_ILD_[i+1] - (SRT_ILD_[i] + 1)
            Intrinsic::Sub.call(&[
                Intrinsic::Shift.call(&[srt_intrld_aux_xs_node.clone(), Node::from_const(1)])?,
                Intrinsic::Add.call(&[srt_intrld_aux_xs_node, Node::from_const(1)])?,
            ])?,
        ])?),
    });
    cs.update_ids();

    Ok(())
}

pub fn validate_nhood(cs: &mut ConstraintSet) -> Result<()> {
    let mut nibble_columns = HashMap::<String, Vec<Handle>>::new();
    let mut byte_columns = HashMap::<String, Vec<Handle>>::new();

    for (h, c) in cs.modules.iter_cols() {
        // only atomic columns (i.e. filled from traces) are of interest here
        if let (Type::Column(magma), Kind::Atomic) = (c.t, &c.kind) {
            match magma {
                Magma::Nibble => nibble_columns
                    .entry(h.module.to_owned())
                    .or_default()
                    .push(h.clone()),
                Magma::Byte => byte_columns
                    .entry(h.module.to_owned())
                    .or_default()
                    .push(h.clone()),
                _ => (),
            }
        }
    }

    for (module, handles) in nibble_columns.iter() {
        process_nhood(module, handles, 4, cs)?;
        cs.modules.set_min_len(module, 2usize.pow(4));
    }

    for (module, handles) in byte_columns.iter() {
        process_nhood(module, handles, 8, cs)?;
        cs.modules.set_min_len(module, 2usize.pow(8));
    }

    cs.update_ids();
    Ok(())
}
