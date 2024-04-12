(defcolumns A B)

(defconst y -2)

;; Ensure A(+2) == B
(defconstraint c3 () (if-not-zero B (eq! B (shift A (neg y)))))
