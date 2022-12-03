use crate::compiler;

#[test]
fn defun_too_many_args() {
    let inputs =
        vec![
       ("stdlib", include_str!("stdlib.lisp")), (
        "too-big",
        "(defcolumns X Y) (defun (f A B) (eq A 3) (eq C 5)) (defconstraint asdf () (f X Y))",
    )];

    assert!(compiler::make(inputs.as_slice()).is_err());
}
