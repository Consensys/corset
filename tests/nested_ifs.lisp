(defcolumns A B C D)
(defconst T 4)

(defconstraint foo-0 () (bin-if-zero A B (begin C 4)))
(defconstraint foo-1 () (if-zero A B C))

(defconstraint Something-else ()
  (if-zero A
           (= 1 1)
           (begin
            (if-not-zero B
                         4
                         (+ 2 3))
            (eq B C))))
