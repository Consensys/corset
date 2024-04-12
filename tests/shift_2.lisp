(defcolumns A B)
(defconst x 1 y -2)
;; Ensure A(-1) == B
(defconstraint c2 () (if-not-zero B (eq! B (shift A (+ x y)))))
