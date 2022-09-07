(defcolumns A B C)

(defconstraint Something-else () (branch-if-zero-else A
                     (begin
                      (= 1 1))
                     (begin
                      (branch-if-not-zero-else B
                       (begin 4)
                       (begin (+ 2 3))))))
