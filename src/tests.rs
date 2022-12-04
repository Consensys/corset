use crate::compiler::{self, CompileSettings};
use anyhow::*;

#[test]
fn types_declaration() -> Result<()> {
    let inputs = vec![
        ("stdlib", include_str!("stdlib.lisp")),
        ("types", include_str!("../tests/types.lisp")),
    ];

    compiler::make(
        inputs.as_slice(),
        &CompileSettings {
            debug: false,
            allow_dups: true,
        },
    )?;

    Ok(())
}

#[test]
fn defun_too_many_args() {
    let inputs =
        vec![
       ("stdlib", include_str!("stdlib.lisp")), (
        "too-big",
        "(defcolumns X Y) (defun (f A B) (eq A 3) (eq C 5)) (defconstraint asdf () (f X Y))",
    )];

    assert!(compiler::make(
        inputs.as_slice(),
        &CompileSettings {
            debug: false,
            allow_dups: false
        }
    )
    .is_err());
}
