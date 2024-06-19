use std::unreachable;

use crate::{
    column::{Column, Computation, ExoOperation},
    compiler::generator::{ADDER_MODULE, MULER_MODULE},
    compiler::{Constraint, Expression, Intrinsic, Kind, Magma, Node},
    pretty::Base,
    structs::Handle,
    ConstraintSet,
};

#[derive(Default)]
struct ProtoAncillaries {
    adder_bit_width: usize,
    muler_bit_width: usize,
}
impl ProtoAncillaries {
    fn update_width(&mut self, op: ExoOperation, x: usize) {
        match op {
            ExoOperation::Add | ExoOperation::Sub => {
                self.adder_bit_width = self.adder_bit_width.max(x);
            }
            ExoOperation::Mul => {
                self.muler_bit_width = self.adder_bit_width.max(x);
            }
        }
    }
}

impl Node {
    fn dyadize(&mut self) {
        match self.e_mut() {
            Expression::Funcall { func, args } => {
                for a in args.iter_mut() {
                    a.dyadize();
                }
                match func {
                    Intrinsic::Add
                    | Intrinsic::Mul
                    | Intrinsic::VectorAdd
                    | Intrinsic::VectorMul => match args.len() {
                        1 => {
                            *self = args[0].clone();
                        }
                        2 => {}
                        _ => {
                            let mut sub_call: Node = func.call(&args[1..]).unwrap();
                            sub_call.dyadize();
                            *self = func.call(&[args[0].clone(), sub_call]).unwrap()
                        }
                    },

                    Intrinsic::Sub | Intrinsic::VectorSub => {} // TODO: handle nested +/- changing signs
                    _ => {}
                }
            }
            Expression::List(ls) => {
                for l in ls.iter_mut() {
                    l.dyadize();
                }
            }
            _ => {}
        }
    }
    fn columnize_constant(&mut self, module: &str, new_constants: &mut Vec<(Column, Computation)>) {
        let value = if let Expression::Const(x) = self.e() {
            x
        } else {
            unreachable!()
        };
        let target = Handle::new(module, format!("{}", value));
        new_constants.push((
            // TODO: add magma
            Column::builder()
                .handle(target.clone())
                .kind(Kind::Computed)
                .base(Base::Hex)
                .build(),
            Computation::ExoConstant {
                value: value.clone(),
                target: target.clone().into(),
            },
        ));

        *self = Node::column().handle(target).kind(Kind::Computed).build();
    }

    fn do_splatter(
        &mut self,
        module: &str,
        ancillaries: &mut ProtoAncillaries,
        exo_op_columns: &mut Vec<(ExoOperation, (Handle, Magma), (Node, Node))>,
        new_constants: &mut Vec<(Column, Computation)>,
    ) {
        match self.e_mut() {
            Expression::Funcall { func, args } => {
                for arg in args.iter_mut() {
                    arg.do_splatter(module, ancillaries, exo_op_columns, new_constants);
                }

                if args.iter().any(|a| a.is_exocolumn()) {
                    match func {
                        Intrinsic::Add | Intrinsic::Sub | Intrinsic::Mul => {
                            assert!(args.len() == 2);
                            for a in args.iter_mut() {
                                // TODO: probably only necessary for
                                // exo-constants; lookups columnization ought
                                // to work otherwise
                                if a.is_constant() {
                                    a.columnize_constant(module, new_constants);
                                }
                            }

                            let op = (*func).into();
                            let new_magma = args.iter().map(|a| a.t().m()).max().unwrap();
                            ancillaries.update_width(op, new_magma.bit_size());
                            let new_handle =
                                Handle::new(module, format!("{}{}{}", args[0], op, args[1]));

                            exo_op_columns.push((
                                op,
                                (new_handle.clone(), new_magma),
                                (args[0].clone(), args[1].clone()),
                            ));
                            *self = Node::column()
                                .handle(new_handle)
                                .t(new_magma)
                                .base(Base::Hex)
                                .kind(Kind::Computed)
                                .build();
                        }
                        Intrinsic::Inv => {}
                        Intrinsic::Normalize => {
                            println!("Maybe TODO: ?");
                        }
                        Intrinsic::VectorAdd | Intrinsic::VectorSub | Intrinsic::VectorMul => {}
                        _ => unreachable!(),
                    }
                }
            }

            Expression::Column { .. } => {}
            Expression::ArrayColumn { .. } => todo!(),
            Expression::List(ls) => {
                for l in ls.iter_mut() {
                    l.do_splatter(module, ancillaries, exo_op_columns, new_constants);
                }
            }

            _ => {}
        }
    }
}

impl ConstraintSet {
    pub(crate) fn splatter(&mut self) {
        let mut new_exo_columns = Vec::new();
        let mut new_constants = Vec::new();
        let mut ancillaries: ProtoAncillaries = Default::default();

        for i in 0..self.constraints.len() {
            if let Constraint::Vanishes { expr: e, .. } = self.constraints.get_mut(i).unwrap() {
                e.dyadize();
                if let Some(module) = self.columns.module_for(&e.dependencies()).as_ref() {
                    e.do_splatter(
                        module,
                        &mut ancillaries,
                        &mut new_exo_columns,
                        &mut new_constants,
                    );
                }
            }
        }

        // self.make_ancillaries(ancillaries);

        for (new_column, new_computation) in new_constants {
            let id = self.columns.insert_column_and_register(new_column).unwrap();
            self.computations.insert(&id, new_computation).unwrap();
        }

        for (func, (new_handle, new_magma), args) in new_exo_columns.into_iter() {
            if self.columns.by_handle(&new_handle).is_err() {
                // TODO: make a maybe_create of this
                let r = self
                    .columns
                    .insert_column_and_register(
                        Column::builder()
                            .handle(new_handle.to_owned())
                            .t(new_magma)
                            .base(Base::Hex)
                            .kind(Kind::Computed)
                            .build(),
                    )
                    .unwrap();
                self.computations
                    .insert(
                        &r,
                        Computation::ExoOperation {
                            op: func,
                            sources: args.clone().into(),
                            target: r.clone(),
                        },
                    )
                    .unwrap();
            };

            match func {
                ExoOperation::Add | ExoOperation::Sub => {
                    let module = ADDER_MODULE;
                    self.insert_constraint(Constraint::Lookup {
                        handle: Handle::new(module, &new_handle.name),
                        including: vec![
                            Node::column()
                                .handle(Handle::new(module, "op"))
                                .kind(Kind::Computed)
                                .build(),
                            Node::column()
                                .handle(Handle::new(module, "arg-1"))
                                .kind(Kind::Computed)
                                .build(),
                            Node::column()
                                .handle(Handle::new(module, "arg-2"))
                                .kind(Kind::Computed)
                                .build(),
                            Node::column()
                                .handle(Handle::new(module, "result"))
                                .kind(Kind::Computed)
                                .build(),
                            Node::column()
                                .handle(Handle::new(module, "done"))
                                .kind(Kind::Computed)
                                .build(),
                        ],
                        included: vec![
                            Node::from_isize(if func == ExoOperation::Add { 1 } else { 0 }),
                            args.0.clone(),
                            args.1.clone(),
                            Node::column()
                                .handle(new_handle)
                                .kind(Kind::Commitment)
                                .build(),
                            Node::from_isize(1),
                        ],
                    })
                }
                ExoOperation::Mul => {
                    let module = MULER_MODULE;
                    self.insert_constraint(Constraint::Lookup {
                        handle: Handle::new(module, &new_handle.name),
                        including: vec![
                            Node::column()
                                .handle(Handle::new(module, "arg-1"))
                                .kind(Kind::Computed)
                                .build(),
                            Node::column()
                                .handle(Handle::new(module, "arg-2"))
                                .kind(Kind::Computed)
                                .build(),
                            Node::column()
                                .handle(Handle::new(module, "result"))
                                .kind(Kind::Computed)
                                .build(),
                            Node::column()
                                .handle(Handle::new(module, "done"))
                                .kind(Kind::Computed)
                                .build(),
                        ],
                        included: vec![
                            args.0.clone(),
                            args.1.clone(),
                            Node::column()
                                .handle(new_handle)
                                .kind(Kind::Commitment)
                                .build(),
                            Node::from_isize(1),
                        ],
                    })
                }
            }
        }
    }
}

pub fn splatter(cs: &mut ConstraintSet) {
    cs.splatter();
}
