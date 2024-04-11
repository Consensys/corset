(defcolumns A B C)

(defconst x 2 y -6)

;; Ensure A(+3) == 0
(defconstraint pipo1 () (if-not-zero B (shift A 3)))
;; Ensure A(-4) + C(-4) == 0
(defconstraint pipo2 () (if-not-zero B (shift (+ A C) (+ x y))))
;; Ensure C(+6) == 0
(defconstraint pipo3 () (if-not-zero B (shift C (neg y))))
