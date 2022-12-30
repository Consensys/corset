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
        (
            "defpurefun",
            "(defcolumns x y) (defpurefun (f a b) (eq a b))",
        ),
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
fn defpure_ok_const() -> Result<()> {
    let inputs = vec![
        ("stdlib", include_str!("stdlib.lisp")),
        (
            "defpurefun",
            "(defconst A 123) (defcolumns x) (defpurefun (f a) (eq a A)) (defconstraint asdf () (f x))",
        ),
    ];

    compiler::make(
        inputs.as_slice(),
        &CompileSettings {
            debug: false,
            allow_dups: false,
        },
    )?;
    Ok(())
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

#[test]
fn array_ok() {
    let inputs = vec![
        ("stdlib", include_str!("stdlib.lisp")),
        (
            "arrays",
            "
(defcolumns A (B :ARRAY[3]) (C :ARRAY [5:8]) (D :ARRAY [8:32:5]) (Q :ARRAY [5]))
(defalias qq D)

(defcolumns
  (EXAMPLE1 :ARRAY[2])       ;; EXAMPLE1 is defined over {1, 2}
  (EXAMPLE2 :ARRAY[4:7])     ;; EXAMPLE2 is defined over {4, 5, 6, 7}
  (EXAMPLE3 :ARRAY[2:10:2])  ;; EXAMPLE3 is defined over {2, 4, 6, 8, 10}
  (EXAMPLE4 :ARRAY{1 6 8}))  ;; EXAMPLE4 is defined over {1, 6, 8}

(defconstraint asdf () (eq (nth B 3) (nth C 8)))
(defconstraint fdsa () (eq A (nth D 28)))
(defconstraint fdsa2 () (eq A (nth qq 28)))
",
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

#[test]
fn array_ko() {
    let inputs = vec![
        ("stdlib", include_str!("stdlib.lisp")),
        (
            "arrays",
            "
  (defcolumns (EXAMPLE4{1 6 8}))

  (defconstraint will-fail ()
    (nth EXAMPLE4 2)) ;; 2 ∉ {1, 6, 8}
",
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
fn prime_in_name() {
    let inputs = vec![("prime", "(defcolumns A B C A' B' C')")];

    assert!(dbg!(compiler::make(
        inputs.as_slice(),
        &CompileSettings {
            debug: false,
            allow_dups: false
        }
    ))
    .is_ok());
}
