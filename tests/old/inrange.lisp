(defcolumns A B (C :ARRAY[3]) (D :INTERLEAVED (A B)))

(definrange A 123)
(definrange (nth C 2) 3)
(definrange (+ A B (nth C 2)) 3)
(definrange (* 2 D) 1024)
