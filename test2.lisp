(defcolumns (A :sized-32) (B :sized-32))
(defconstraint cstr1 () (vanishes! (+ A B)))
