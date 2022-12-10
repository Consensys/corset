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

#[test]
fn defpure_ok() {
    let inputs = vec![
        ("stdlib", include_str!("stdlib.lisp")),
        ("defpurefun", "(defcolumns X Y) (defun (f A B) (eq A B))"),
    ];

    assert!(compiler::make(
        inputs.as_slice(),
        &CompileSettings {
            debug: false,
            allow_dups: false,
        },
    )
    .is_ok());
}

#[test]
fn defpure_ko() {
    let inputs = vec![
        ("stdlib", include_str!("stdlib.lisp")),
        (
            "defpurefun",
            "(defcolumns X Y Z) (defpurefun (f A B) (begin (eq A 3) (eq B Z))) (defconstraint asdf () (f X Y))",
        ),
    ];

    assert!(dbg!(compiler::make(
        inputs.as_slice(),
        &CompileSettings {
            debug: false,
            allow_dups: false
        }
    ))
    .is_err());
}

#[test]
fn huge_const() {
    let inputs = vec![
        ("stdlib", include_str!("stdlib.lisp")),
        (
            "hugeconstant",
            "(defconst A 340282366920938463463374607431768211456)",
        ),
    ];

    assert!(dbg!(compiler::make(
        inputs.as_slice(),
        &CompileSettings {
            debug: false,
            allow_dups: false
        }
    ))
    .is_ok());
}
