(defcolumns A (B :ARRAY[3]) (C :ARRAY [5:8]) (D :ARRAY [8:32:5]) (Q :ARRAY [5]))
(defalias qq D)

(defcolumns
  (EXAMPLE1 :ARRAY[2])       ;; EXAMPLE1 is defined over {1, 2}
  (EXAMPLE2 :ARRAY[4:7])     ;; EXAMPLE2 is defined over {4, 5, 6, 7}
  (EXAMPLE3 :ARRAY[2:10:2])  ;; EXAMPLE3 is defined over {2, 4, 6, 8, 10}
  (EXAMPLE4 :ARRAY{1 6 8}))  ;; EXAMPLE4 is defined over {1, 6, 8}

(defconstraint asdf () (eq (nth B 3) (nth C 8)))
(defconstraint fdsa () (eq A (nth D 28)))
(defconstraint fdsa2 () (eq A (nth qq 28)))
