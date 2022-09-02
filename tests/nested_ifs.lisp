(defcolumns A B C)

(branch-if-zero-else A
                     (begin
                      (= 1 1))
                     (begin
                      (branch-if-not-zero-else B
                       (begin 4)
                       (begin (+ 2 3)))))
