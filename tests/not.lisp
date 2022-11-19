(defcolumns A (B :BOOLEAN) C)

(defconstraint test1 ()
  (if-zero (sub 1 B) A))

(defconstraint test2 ()
  (if-zero (not B) A))

(defconstraint test3 ()
  (if-zero (sub 1 C) A))

(defconstraint test3 ()
  (if-not-zero (not C) A))
