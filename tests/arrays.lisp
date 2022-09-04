(defcolumns A B[3] C[5:8] D[8:32:5])
(defalias qq D)

(defconstraint asdf (eq (ith B 3) (ith C 8)))
(defconstraint fdsa (eq A (ith D 28)))
(defconstraint fdsa2 (eq A (ith qq 28)))
