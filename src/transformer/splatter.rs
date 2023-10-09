use std::unreachable;

use crate::{
    column::{Column, Computation},
    compiler::{Constraint, Expression, Intrinsic, Kind, Magma, Node},
    pretty::Base,
    structs::Handle,
    ConstraintSet,
};
use anyhow::*;

#[derive(Default)]
struct ProtoAdder {
    width: usize,
}

impl Node {
    fn columnize_constant(&mut self, module: &str, new_constants: &mut Vec<(Column, Computation)>) {
        let value = if let Expression::Const(x, _) = self.e() {
            x
        } else {
            unreachable!()
        };
        let target = Handle::new(module, format!("cst_{}", value));
        new_constants.push((
            // TODO: add magma
            Column::builder()
                .handle(target.clone())
                .kind(Kind::Phantom)
                .base(Base::Hex)
                .build(),
            Computation::ExoConstant {
                value: value.clone(),
                target: target.clone().into(),
            },
        ));

        *self = Node::column().handle(target).kind(Kind::Phantom).build();
    }

    fn do_splatter(
        &mut self,
        module: &str,
        adder: &mut ProtoAdder,
        new_exo_columns: &mut Vec<(Intrinsic, (Handle, Magma), (Node, Node))>,
        new_constants: &mut Vec<(Column, Computation)>,
    ) {
        match self.e_mut() {
            Expression::Funcall { func, args } => {
                for arg in args.iter_mut() {
                    arg.do_splatter(module, adder, new_exo_columns, new_constants);
                }

                if args.iter().any(|a| a.is_exocolumn()) {
                    match func {
                        Intrinsic::Add => {
                            assert!(args.len() == 2);
                            for a in args.iter_mut() {
                                if a.is_constant() {
                                    a.columnize_constant(module, new_constants);
                                }
                            }
                            let added_magma = args.iter().map(|a| a.t().magma()).max().unwrap();
                            adder.width = adder.width.max(added_magma.bit_size());
                            let added_handle =
                                Handle::new(module, format!("{}⊕{}", args[0], args[1]));
                            new_exo_columns.push((
                                *func,
                                (added_handle.clone(), added_magma),
                                (args[0].clone(), args[1].clone()),
                            ));
                            *self = Node::column()
                                .handle(added_handle)
                                .t(added_magma)
                                .base(Base::Hex)
                                .kind(Kind::Phantom)
                                .build();
                        }
                        Intrinsic::Sub => {
                            todo!()
                        }
                        Intrinsic::Mul => {
                            assert!(args.len() == 2);
                            let added_handle =
                                Handle::new(module, format!("{}⊗{}", args[0], args[1]));
                            let added_magma = args.iter().map(|a| a.t().magma()).max().unwrap();
                            new_exo_columns.push((
                                *func,
                                (added_handle.clone(), added_magma),
                                (args[0].clone(), args[1].clone()),
                            ));
                            *self = Node::column()
                                .handle(added_handle)
                                .t(added_magma)
                                .base(Base::Hex)
                                .kind(Kind::Phantom)
                                .build();
                        }
                        Intrinsic::Shift => todo!(),
                        Intrinsic::Inv => todo!(),
                        _ => unreachable!(),
                    }
                }
            }

            Expression::Column { .. } => {}
            Expression::ArrayColumn { .. } => todo!(),

            _ => {}
        }
    }
}

impl ConstraintSet {
    fn make_adder(&mut self, adder: ProtoAdder) {
        self.columns
            .insert_column_and_register(
                Column::builder()
                    .t(Magma::Boolean)
                    .handle(Handle::new("#adder", "op"))
                    .base(Base::Hex)
                    .kind(Kind::Phantom)
                    .build(),
            )
            .unwrap();
        self.columns
            .insert_column_and_register(
                Column::builder()
                    .t(Magma::Integer(adder.width))
                    .handle(Handle::new("#adder", "arg-1"))
                    .base(Base::Hex)
                    .kind(Kind::Phantom)
                    .build(),
            )
            .unwrap();
        self.columns
            .insert_column_and_register(
                Column::builder()
                    .t(Magma::Integer(adder.width))
                    .handle(Handle::new("#adder", "arg-2"))
                    .base(Base::Hex)
                    .kind(Kind::Phantom)
                    .build(),
            )
            .unwrap();
        self.columns
            .insert_column_and_register(
                Column::builder()
                    .t(Magma::Integer(adder.width))
                    .handle(Handle::new("#adder", "result"))
                    .base(Base::Hex)
                    .kind(Kind::Phantom)
                    .build(),
            )
            .unwrap();
        self.columns
            .insert_column_and_register(
                Column::builder()
                    .t(Magma::Boolean)
                    .handle(Handle::new("#adder", "done"))
                    .kind(Kind::Phantom)
                    .build(),
            )
            .unwrap();
    }

    pub(crate) fn splatter(&mut self) {
        let mut new_exo_columns = Vec::new();
        let mut new_constants = Vec::new();
        let mut adder: ProtoAdder = Default::default();
        for i in 0..self.constraints.len() {
            if let Constraint::Vanishes { expr: e, .. } = self.constraints.get_mut(i).unwrap() {
                e.do_splatter(
                    self.columns.module_of(e.dependencies()).unwrap().as_str(),
                    &mut adder,
                    &mut new_exo_columns,
                    &mut new_constants,
                );
            }
        }

        self.make_adder(adder);
        for (new_column, new_computation) in new_constants {
            let id = self.columns.insert_column_and_register(new_column).unwrap();
            self.computations.insert(&id, new_computation).unwrap();
        }
        for (func, (added_handle, added_magma), args) in dbg!(new_exo_columns).into_iter() {
            let added_ref = self
                .columns
                .insert_column_and_register(
                    Column::builder()
                        .handle(added_handle.to_owned())
                        .t(added_magma)
                        .base(Base::Hex)
                        .kind(Kind::Composite(Box::new(())))
                        .build(),
                )
                .unwrap();
            self.computations
                .insert(
                    &added_ref,
                    match func {
                        Intrinsic::Add => Computation::ExoAddition {
                            sources: args.clone().into(),
                            target: added_ref.clone(),
                        },
                        Intrinsic::Sub => todo!(),
                        Intrinsic::Mul => Computation::ExoMultiplication {
                            sources: args.clone().into(),
                            target: added_ref.clone(),
                        },

                        _ => unreachable!(),
                    },
                )
                .unwrap();
            self.constraints.push(Constraint::Plookup {
                handle: Handle::new("add", &added_handle.name),
                including: vec![
                    Node::column()
                        .handle(Handle::new("#adder", "arg-1"))
                        .kind(Kind::Phantom)
                        .build(),
                    Node::column()
                        .handle(Handle::new("#adder", "arg-2"))
                        .kind(Kind::Phantom)
                        .build(),
                    Node::column()
                        .handle(Handle::new("#adder", "result"))
                        .kind(Kind::Phantom)
                        .build(),
                ],
                included: vec![
                    args.0.clone(),
                    args.1.clone(),
                    Node::column()
                        .handle(added_handle)
                        .kind(Kind::Atomic)
                        .build(),
                ],
            })
        }
    }
}

pub fn splatter(cs: &mut ConstraintSet) {
    cs.splatter();
}
