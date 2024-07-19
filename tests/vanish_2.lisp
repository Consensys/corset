(defcolumns X)

(defconstraint Constraint ()
  (neq! (if (is-zero 1) X X) X)
)
