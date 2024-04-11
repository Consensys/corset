(defcolumns A (B :ARRAY[3]))

;; B[1] + 1 == B[2]
(defconstraint c1 () (if-not-zero A (eq! (+ 1 [B 1]) [B 2])))
;; B[2] + 2 == B[3]
(defconstraint c2 () (if-not-zero A (eq! (+ 2 [B 2]) [B 3])))
;; A == B[1] || A == B[2] || A == B[3]
(defconstraint c3 () (any!
                       (eq! A [B 1])
                       (eq! A [B 2])
                       (eq! A [B 3])
                      ))
