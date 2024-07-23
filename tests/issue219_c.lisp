(defcolumns ST X)

(defconstraint constraint-test ()
  (if-not-zero ST (vanishes! (- 1 (if (vanishes! X) 1 1)))))
