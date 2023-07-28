use anyhow::Result;

use crate::{
    column::{Column, ColumnSet},
    compiler::{
        generator::convert_constraint, Constraint, ConstraintSet, Expression,
        FieldSpecificExpression, Intrinsic, Kind, Magma, Node, RegisterRef,
    },
    structs::{Field, Handle},
};

impl<F: Field> ConstraintSet<F> {
    pub fn spread_registers(&mut self) -> Result<()> {
        assert!(self.field_specific_constraints.is_empty());

        let mut specific_constraints = Vec::new();
        let mut columns = self.columns.clone(); // TODO avoid cloning

        let mut new_specific_constraints = self
            .constraints
            .iter()
            .map(|c| {
                convert_constraint(c.clone(), |agnostic_node| {
                    agnostic_to_specific_node(
                        agnostic_node,
                        &mut columns,
                        &mut specific_constraints,
                    )
                })
            })
            .collect::<Result<Vec<_>>>()?;

        specific_constraints.append(&mut new_specific_constraints);

        self.field_specific_constraints = specific_constraints;
        self.columns = columns;

        dbg!(&self.constraints);
        dbg!(&self.columns);
        dbg!(&self.field_specific_constraints);

        Ok(())
    }
}

fn agnostic_to_specific_node<F: Field>(
    agnostic_node: &Node<Expression<F>, F>,
    columns: &mut ColumnSet<F>,
    specific_constraints: &mut Vec<Constraint<Node<FieldSpecificExpression<F>, F>, F>>,
) -> Result<Node<FieldSpecificExpression<F>, F>> {
    let specific_expression = match agnostic_node.e() {
        Expression::Funcall { func, args } => match func {
            Intrinsic::Add => {
                assert!(args.len() == 2, "TODO handle >= 3 args");
                let a = agnostic_to_specific_node(&args[0], columns, specific_constraints)?;
                let b = agnostic_to_specific_node(&args[1], columns, specific_constraints)?;

                match (a.e(), b.e()) {
                    (
                        FieldSpecificExpression::Column {
                            handle: handle_a,
                            kind: kind_a,
                            padding_value: padding_value_a,
                            base: base_a,
                            fetched: fetched_a,
                        },
                        FieldSpecificExpression::Column {
                            handle: handle_b,
                            kind: kind_b,
                            padding_value: padding_value_b,
                            base: base_b,
                            fetched: fetched_b,
                        },
                    ) => {
                        assert!(padding_value_a == padding_value_b);
                        assert!(base_a == base_b);
                        assert!(fetched_a == fetched_b);

                        let column_a = columns.get_col(handle_a).unwrap();
                        let column_b = columns.get_col(handle_b).unwrap();

                        let bits_of_sum =
                            column_a.t.bits().unwrap().max(column_b.t.bits().unwrap()) + 1; // we avoid overflow with the + 1, TODO create a new 'ADD' that can overflow with undefined behaviour (usefull when we know that there is no overflow)

                        // TODO: A + B devieent une nouvelle colonne, les field registers sont crées automatiquement, a nous de rajouter une computation (TODO creer)

                        let new_handle = column_a.handle.sum(&column_b.handle);

                        // TODO create smallness constraints for each register of the sum column

                        let sum_magma = Magma::from_bits(bits_of_sum);
                        let sum_column = columns
                            .insert_column_and_register(
                                Column::builder()
                                    .kind(Kind::Phantom)
                                    .handle(new_handle)
                                    .t(sum_magma)
                                    .build(),
                            )
                            .unwrap();

                        let sum_repr_count = sum_magma.repr_count::<F>();

                        let mut build_columns =
                            |t: Magma, handle_convertion: &dyn Fn(&Handle, usize) -> Handle| {
                                (0..sum_repr_count)
                                    .map(|i| {
                                        let new_handle =
                                            handle_convertion(sum_column.as_handle(), i);
                                        columns
                                            .insert_column_and_register(
                                                Column::builder()
                                                    .kind(Kind::Phantom)
                                                    .handle(new_handle)
                                                    .t(t)
                                                    .build(),
                                            )
                                            .unwrap()
                                    })
                                    .collect::<Vec<_>>()
                            };

                        let byte_columns = build_columns(Magma::Byte, &Handle::as_byte_handle);
                        let acc_columns =
                            build_columns(Magma::primitive::<F>(), &Handle::as_acc_handle);
                        let overflow_columns =
                            build_columns(Magma::Boolean, &Handle::as_overflow_handle);

                        let counter_column = {
                            let counter_handle = sum_column.as_handle().as_counter_handle();
                            columns
                                .insert_column_and_register(
                                    Column::builder()
                                        .kind(Kind::Phantom)
                                        .handle(counter_handle)
                                        .t(Magma::primitive::<F>())
                                        .build(),
                                )
                                .unwrap()
                        };

                        // for i in 0..sum_repr_count {
                        //     // cf https://github.com/Consensys/zkevm-spec/issues/63
                        //     // ∀ 0 <= c < m,  overflow_ c + x_c + y_c = acc_c + 256^r . overflow_{c + 1}
                        //     let sub_constraint_expr: Node<FieldSpecificExpression<F>, F> =
                        //         Node::_new(FieldSpecificExpression::Funcall {
                        //             func: Intrinsic::Sub,
                        //             args: vec![
                        //                 Node::_new(FieldSpecificExpression::Funcall {
                        //                     func: Intrinsic::Add,
                        //                     args: vec![
                        //                         Node::_new(FieldSpecificExpression::Register(
                        //                             new_registers[i].clone(),
                        //                         )),
                        //                         Node::_new(FieldSpecificExpression::Funcall {
                        //                             func: Intrinsic::Add,
                        //                             args: vec![
                        //                                 Node::_new(
                        //                                     FieldSpecificExpression::Register(
                        //                                         registers_a[i].clone(),
                        //                                     ),
                        //                                 ),
                        //                                 Node::_new(
                        //                                     FieldSpecificExpression::Register(
                        //                                         registers_b[i].clone(),
                        //                                     ),
                        //                                 ),
                        //                             ],
                        //                         }),
                        //                     ],
                        //                 }),
                        //                 Node::_new(FieldSpecificExpression::Funcall {
                        //                     func: Intrinsic::Add,
                        //                     args: vec![
                        //                         Node::_new(FieldSpecificExpression::Register(
                        //                             registers_a[i].clone(),
                        //                         )),
                        //                         Node::_new(FieldSpecificExpression::Register(
                        //                             registers_b[i].clone(),
                        //                         )),
                        //                     ],
                        //                 }),
                        //             ],
                        //         });

                        //     associated_constraints.push(Constraint::Vanishes {
                        //         handle: new_column_ref.as_handle().to_sum_constraint(i),
                        //         domain: None,
                        //         expr: Box::new(sub_constraint_expr),
                        //     });
                        // }

                        // FieldSpecificExpression::ResgisterSeries {
                        //     column: new_column_ref,
                        //     registers: new_registers,
                        //     associated_constraints,
                        //     padding_value: padding_value_a.clone(),
                        //     base: *base_a,
                        //     fetched: *fetched_a,
                        // }
                        todo!()
                    }
                    _ => unreachable!(),
                }
            }
            Intrinsic::Sub => {
                // TODO: create 2 types of sub: one 'real', that can be used in 'A - B + C - D', and one that is only used in 'A - B = 0'
                todo!()
            }
            Intrinsic::Mul => todo!(),
            Intrinsic::Exp => todo!(),
            Intrinsic::Shift => todo!(),
            Intrinsic::Neg => todo!(),
            Intrinsic::Inv => todo!(),
            Intrinsic::IfZero => todo!(),
            Intrinsic::IfNotZero => todo!(),
        },
        Expression::Const(_, _) => todo!(),
        Expression::Column {
            handle,
            kind,
            padding_value,
            base,
            fetched,
        } => {
            let repr_count = agnostic_node.t().magma().repr_count::<F>();
            let registers = (0..repr_count)
                .map(|i| {
                    Node::from(FieldSpecificExpression::Register(RegisterRef {
                        colomn: handle.clone(),
                        index: i,
                    }))
                })
                .collect::<Vec<_>>();
            FieldSpecificExpression::List(registers)
        }
        Expression::ArrayColumn {
            handle,
            domain,
            base,
        } => todo!(),
        Expression::List(l) => FieldSpecificExpression::List(
            l.into_iter()
                .map(|n| agnostic_to_specific_node(n, columns, specific_constraints))
                .flatten()
                .collect(),
        ),
        Expression::Void => FieldSpecificExpression::Void,
    };

    Ok(Node {
        _e: specific_expression,
        _t: Some(agnostic_node.t().clone()),
        dbg: agnostic_node.dbg.clone(),
        _phantom: std::marker::PhantomData,
    })
}
