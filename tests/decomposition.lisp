(module tests)
(defcolumns A)
(defconstraint test-decomposition () (make-byte-decomposition A 1 4))
