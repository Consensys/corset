(defcolumns ST X Y)

(defconstraint constraint-test ()
  (if-not-zero ST (vanishes! (- 1 (if (vanishes! X) Y 1)))))
