(defcolumns X Y (A :length 2))
(definterleaved (Z) (X Y))
(defconstraint tmp () (vanishes! (- A Z)))
