(defcolumns A B:BOOLEAN C D:BOOLEAN)
(defconst T 4)

(defconstraint foo-0 () (if-zero A B (begin C 4)))
(defconstraint foo-1 () (if-zero B A C))

(defconstraint Something-else ()
  (if-zero (- A 3)
           (= 1 1)
           (begin
            (if-not-zero (and B D)
                         4
                         (+ 2 3))
            (eq B C))))

(defconstraint bool-num ()
  (if-zero (and A B) 3 4))

(defconstraint bool-bool ()
  (if-zero (and D B) 3 4))
