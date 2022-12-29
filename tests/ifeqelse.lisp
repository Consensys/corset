(defcolumns (A :BOOLEAN) (B :BOOLEAN))
(defconstraint abc () (if-eq-else A B 4 8))
