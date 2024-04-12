(defcolumns A B)
;; Ensure A(+1) == A + 1
(defconstraint c1 () (if-not-zero B (eq! (+ A 1) (shift A 1))))
