(defcolumns A B C P Q R)

(defplookup (A C) (Q R))
(defplookup (A B) ((* 2 Q) (+ Q R)))
