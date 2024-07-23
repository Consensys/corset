(defcolumns X ST)

;; ST ==> x != 1
(defconstraint constraint-6 () (if-not-zero ST (neq! 1 (if (is-zero 0) X X))))
;; ST ==> x != 0
(defconstraint constraint-7 () (if-not-zero ST (neq! 0 (if (is-zero X) X X))))
