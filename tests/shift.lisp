(defcolumns A B C)

(defconst x 2 y -6)

(defconstraint pipo1 () (shift A 3))
(defconstraint pipo2 () (shift (+ A C) (+ x y)))
(defconstraint pipo3 () (shift A (neg y)))
(defconstraint pipo4 () (shift (+ A (shift B 3)) (* x y)))
