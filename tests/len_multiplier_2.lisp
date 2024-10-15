(defcolumns (A :length 2) (P :binary@prove :length 2))
(defperspective perp1 P (X Y))
(definterleaved (Z) (perp1/X perp1/Y))

(defconstraint c1 (:perspective perp1) (vanishes! (- A Z)))
