(defcolumns A B C D E F G)

(defconstraint pipo () (if-zero A
                                (+ C D)
                                E))

(defconstraint adsf () (= F G))
