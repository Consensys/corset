(defcolumns A B)
(defconst
  X 2
  Y 6
  Z (+ 3 X (* 2 Y)))

(defconstraint test ()
  (+ A X B Z))
