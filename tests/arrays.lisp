(defcolumns A B[3] Z[5:8])

(defconstraint asdf (eq (ith B 3) (ith Z 8)))
(defconstraint fdsa (eq A (ith B 2)))
