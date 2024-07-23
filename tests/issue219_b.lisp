(defcolumns ST X)

(defconstraint constraint-test ()
  (if-not-zero ST (is-not-zero! (if (vanishes! X) 1 1))))
