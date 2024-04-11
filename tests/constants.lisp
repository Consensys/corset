(defcolumns A B)
(defconst
  X 2
  Y 6
  Z (+ 3 X (* 2 Y)))

(defconstraint test ()
  (if-not-zero B (+ A X B Z)))
