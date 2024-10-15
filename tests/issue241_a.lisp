(defcolumns X)

(defconstraint constraint-1 () (is-not-zero! (if (is-zero 1) 1 1)))
(defconstraint constraint-2 () (eq! (is-not-zero! (if (is-zero X) 1 X)) 0))
(defconstraint constraint-3 () (eq! (or! (is-not-zero! (if (is-zero 0) 1 0)) 0) 0))
(defconstraint constraint-4 () (is-not-zero! (if (is-zero 0) 1 0)))
(defconstraint constraint-5 () (is-not-zero! (if (is-zero X) 1 X)))
