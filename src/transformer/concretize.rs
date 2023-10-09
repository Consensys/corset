use crate::{
    column::Column,
    compiler::{Constraint, ConstraintSet, Expression, Kind, Magma},
    pretty::Pretty,
    structs::Handle,
};
use log::*;

fn iotaize(h: &Handle, i: usize) -> Vec<Handle> {
    (0..i).map(|i| h.iota(i)).collect()
}

fn concretize_columns(cs: &mut ConstraintSet) {
    // let mut new_columns = Vec::new();
    // for (col_ref, col) in cs.columns.iter() {
    //     let register = cs.columns.register(&col_ref).unwrap();
    //     if register.width() > 1 {
    //         for h in iotaize(&col.handle, register.width()).into_iter() {
    //             new_columns.push(
    //                 Column::builder()
    //                     .handle(h)
    //                     .kind(Kind::Phantom)
    //                     .t(Magma::Native)
    //                     .build(),
    //             );
    //         }
    //     }
    // }
    // for c in new_columns.into_iter() {
    //     cs.columns.insert_column_and_register(c).unwrap();
    // }
}

// fn concretize_constraints(cs: &mut ConstraintSet) {
//     for c in cs.constraints.iter_mut() {
//         match c {
//             Constraint::Vanishes {
//                 handle,
//                 domain,
//                 expr,
//             } => {}
//             Constraint::Plookup {
//                 handle,
//                 including,
//                 included,
//             } => {
//                 *c = Constraint::Plookup {
//                     handle: handle.clone(),
//                     including: including
//                         .iter()
//                         .flat_map(|i| match i.e() {
//                             Expression::Column { .. } => vec![i.clone()],
//                             Expression::ExoColumn { handle, .. } => {
//                                 let column = cs.columns.column(handle).unwrap();

//                                 vec![i.clone()]
//                             }
//                             _ => unreachable!(),
//                         })
//                         .collect(),
//                     included: included.clone(),
//                 }
//             }
//             Constraint::Permutation {
//                 handle,
//                 from,
//                 to,
//                 signs,
//             } => {}
//             Constraint::InRange { handle, exp, max } => {}
//         }
//     }
// }

fn concretize_registers(cs: &mut ConstraintSet) {
    for r in cs.columns.registers.iter_mut() {
        if let Some(value) = r.value_mut() {
            value.iter_mut().for_each(|x| x.bi_to_fr());
        } else {
            error!(
                "{} is empty",
                r.handle
                    .as_ref()
                    .map(|h| h.pretty())
                    .unwrap_or("????".into())
            );
        }
    }
}

pub fn concretize(cs: &mut ConstraintSet) {
    concretize_registers(cs);
}
