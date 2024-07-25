(defcolumns X ST)

(defconstraint c0 ()
  (if-not-zero ST (vanishes!
                   (if (is-zero (if (is-zero 1) 0 0))
                       X
                       (~and! 1 1)))))

(defconstraint c1 () (if-not-zero ST (vanishes! (if (is-zero 0) X (~and! 1 1)))))

(defconstraint c2 () (if-not-zero ST (vanishes! X)))
