(defcolumns A B C)

(defconst x 2 y -6)

(defconstraint pipo1 () (shift A C))
(defconstraint pipo2 () (shift A (+ x y)))
(defconstraint pipo3 () (shift A (neg y)))
