(defcolumns A B[3] C[5:8] D[8:32:5] Q[5])
(defalias qq D)

(defconstraint asdf () (eq (nth B 3) (nth C 8)))
(defconstraint fdsa () (eq A (nth D 28)))
(defconstraint fdsa2 () (eq A (nth qq 28)))
