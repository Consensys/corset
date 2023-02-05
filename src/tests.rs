use crate::compiler::{self, CompileSettings};
use anyhow::*;

fn make(name: &str, source: &str) -> Result<()> {
    let inputs = vec![("stdlib", include_str!("stdlib.lisp")), (name, source)];

    compiler::make(
        inputs.as_slice(),
        &CompileSettings {
            debug: false,
            allow_dups: true,
        },
    )
    .map(|_| ())
}

fn must_run(name: &str, source: &str) {
    let r = make(name, source);
    if let Err(err) = &r {
        eprintln!("{}", err);
    }
    assert!(r.is_ok());
}

fn must_fail(name: &str, source: &str) {
    let r = make(name, source);
    assert!(r.is_err());
}

#[test]
fn types_declaration() -> Result<()> {
    make("type", include_str!("../tests/types.lisp"))
}

#[test]
fn defun_too_many_args() {
    must_fail(
        "too-big",
        "(defcolumns X Y) (defun (f A B) (eq A 3) (eq C 5)) (defconstraint asdf () (f X Y))",
    );
}

#[test]
fn defpure_ok() {
    must_run(
        "defpurefun",
        "(defcolumns x y) (defpurefun (f a b) (eq a b))",
    );
}

#[test]
fn defpure_ok_const() {
    must_run(
        "defpurefun",
        "(defconst A 123) (defcolumns x) (defpurefun (f a) (eq a A)) (defconstraint asdf () (f x))",
    );
}

#[test]
fn defpure_ko() {
    must_fail(
            "defpurefun",
            "(defcolumns X Y Z) (defpurefun (f A B) (begin (eq A 3) (eq B Z))) (defconstraint asdf () (f X Y))",
        )
}

#[test]
fn huge_const() {
    must_run(
        "hugeconstant",
        "(defconst A 340282366920938463463374607431768211456)",
    );
}

#[test]
fn array_ok() {
    must_run(
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
    );
}

#[test]
fn array_ko() {
    must_fail(
        "arrays",
        "(defcolumns (EXAMPLE4{1 6 8}))
         (defconstraint will-fail () (nth EXAMPLE4 2))",
    );
}

#[test]
fn prime_in_name() {
    must_run("quotes in names", "(defcolumns A B C A' B' C')");
}

#[test]
fn ok_let() {
    must_run("let-1", "(defcolumns a b c) (defconstraint test () (let ((x (+ a b)) (y (+ c x)) (z y)) (+ a b c x y z)))");
    must_run(
        "let-2",
        "(defcolumns a b c) (defconstraint test () (let () (+ a b c)))",
    );
    must_run(
        "let-2",
        "(defcolumns a b c) (defconstraint test () (let ((a c) (b c)) (+ a b c)))",
    );
}

#[test]
fn ko_let() {
    must_fail(
        "let-1",
        "(defcolumns a b c) (defconstraint test () (let  (+ a b c x y z)))",
    );

    must_fail(
        "let-2",
        "(defcolumns a b c) (defconstraint test () (let  ((z y))))",
    );

    must_fail(
        "let-3",
        "(defcolumns a b c) (defconstraint test () (let  ((z )) (+ a b c)))",
    );

    must_fail(
        "let-4",
        "(defcolumns a b c) (defconstraint test () (let  ((z 1) (q 3)) (+ a b c) (eq 3 4)))",
    );
}

#[test]
fn array_len() {
    must_run(
        "len ok",
        "(defcolumns (a[5]) b c) (defconstraint test () (eq (len a) 5))",
    );
    must_fail(
        "not an array",
        "(defcolumns (a[5]) b c) (defconstraint test () (eq (len b) 5))",
    );
    must_fail(
        "not an array 2",
        "(defcolumns (a[5]) b c) (defconstraint test () (eq (len (nth a 2)) 5))",
    );
}
