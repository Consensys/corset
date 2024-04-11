(defcolumns A B C)

(defconst x 2 y -2)

;; Ensures A(-4) == 0
(defconstraint c1 () (if-not-zero B (shift A (* x y))))
;; Ensures A(-4) + C(-1) == 0
(defconstraint c2 () (if-not-zero B (shift (+ A (shift C 3)) (* x y))))
