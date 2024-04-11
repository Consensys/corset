(defcolumns A B)

(defconstraint test1 ()
  (if-not-zero B (is-zero A)))
