use crate::{transformer::ExpansionLevel, ConstraintSetBuilder};
use anyhow::*;

fn make(name: &str, source: &str) -> Result<()> {
    let mut r = ConstraintSetBuilder::from_sources(false, false);
    r.add_source(source)?;
    r.expand_to(ExpansionLevel::top());

    r.into_constraint_set().map(|_| ())
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
        "(defcolumns X Y) (defun (f A B) (eq! A 3) (eq! C 5)) (defconstraint asdf () (f X Y))",
    );
}

#[test]
fn defpure_ok() {
    must_run(
        "defpurefun",
        "(defcolumns x y) (defpurefun (f a b) (eq! a b))",
    );
}

#[test]
fn defpure_ok_const() {
    must_run(
        "defpurefun",
        "(defconst A 123) (defcolumns x) (defpurefun (f a) (eq! a A)) (defconstraint asdf () (f x))",
    );
}

#[test]
fn defpure_ko() {
    must_fail(
            "defpurefun",
            "(defcolumns X Y Z) (defpurefun (f A B) (begin (eq! A 3) (eq! B Z))) (defconstraint asdf () (f X Y))",
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

(defconstraint asdf () (eq! [B 3] [C 8]))
(defconstraint fdsa () (eq! A [D 28]))
(defconstraint fdsa2 () (eq! A [qq 28]))
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
        "(defcolumns a b c) (defconstraint test () (let  ((z 1) (q 3)) (+ a b c) (eq! 3 4)))",
    );
}

#[test]
fn array_len() {
    must_run(
        "len ok",
        "(defcolumns (a[5]) b c) (defconstraint test () (eq! (len a) 5))",
    );
    must_fail(
        "not an array",
        "(defcolumns (a[5]) b c) (defconstraint test () (eq! (len b) 5))",
    );
    must_fail(
        "not an array 2",
        "(defcolumns (a[5]) b c) (defconstraint test () (eq! (len (nth a 2)) 5))",
    );
}

#[test]
fn global_scope() {
    must_run(
        "global scope ok",
        "(module asdf) (defcolumns a b) (module zxcv) (defcolumns x y) (deflookup test (asdf.a asdf.b) (zxcv.x zxcv.y))",
    );
    must_fail(
        "local scope ok",
        "(module asdf) (defcolumns a b) (module zxcv) (defcolumns x y) (defconstraint test () (= asdf.a zxcv.x))",
    );
}

#[test]
fn definterleave() {
    must_run(
        "definterleave ok",
        "(defcolumns A B (C :array [1:4])) (definterleaved (D :display :hex) (A [C 2] B ))",
    );
    must_fail(
        "cannot specify type in :definterleaved",
        "(defcolumns A (B :byte) (C :array [1:4])) (definterleaved (D :byte) (A [C 2] B ))",
    );
    must_fail(
        "already exists in :definterleaved",
        "(defcolumns A (B :byte) (C :array [1:4])) (definterleaved A (A [C 2] B ))",
    );
}

#[test]
fn defpermutation() {
    must_run(
        "defpermutation ok",
        "(defcolumns A (B :byte ) C (D :array [0:4])) (defpermutation (X (Y :display :hex)) ((+ A) (- [D 2])))",
    );
    must_fail(
        "defpermutation: cardinality mismatch",
        "(defcolumns A (B :byte ) C (D :array [0:4])) (defpermutation (X F (Y :display :hex)) ((+ A) (- [D 2])))",
    );
    must_fail(
        "found sorting column after non-sorting column",
        "(defcolumns A (B :byte ) C (D :array [0:4])) (defpermutation (X Y (Z :display :hex)) ((- A) [D 2] (- C)))",
    );
}

#[test]
fn base_declaration() {
    must_fail(
        "cannot redefine base",
        "(defcolumns (A :display :hex :display :dec))",
    );
}

#[test]
fn computed_range_def() {
    must_run(
        "can use const. expr. in ranges",
        "(defcolumns (A :array[0:(+ 3 2)]))",
    )
}

#[test]
fn complex_range_def() {
    must_run(
        "can use const. expr. in ranges",
        "(defpurefun (f x) (+ 2 x))(defconst A 3 B (f A)) (defcolumns (X :array[(* 2 A):(* 2 B):(+ 1 1)]))",
    )
}

#[test]
fn complex_for() {
    must_run(
        "can use const. expr. in ranges",
        "(defconst A 3 B (* 2 A)) (defcolumns (X :array[A:B])) (defconstraint asdf () (for i [A:(+ (- A 1) (len X))] (vanishes! [X i])))",
    )
}

// #[test]
// fn exo_if() {
//     must_run(
//         "WiP",
//         "(module foobar) (defcolumns A B (C :bool) (D :i32)) (defconstraint pipo () (if (eq! A B) C D))",
//     );

//     // must_run(
//     //     "WiP 2",
//     //     "(module foobar) (defcolumns A B (C :bool) (D :i32)) (defconstraint pipo () (if (eq! A D) C D))",
//     // );
// }
