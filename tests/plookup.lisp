(defcolumns A B C P Q R)

(deflookup (A C) (Q R))
(deflookup (A B) ((* 2 Q) (+ Q R)))
