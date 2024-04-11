(defcolumns A (B :ARRAY [4:14]))

(defconstraint test1 () (for i {4 5 6} (eq! A [B i])))

(defconstraint test2 () (for i B (eq! A [B i])))

(defconstraint test3 () (for i [10:14]
                          (for j {4 6 8}
                               (for k {1 5} (eq! [B i] [B j])))))

(defconstraint test4 () (for i [5:10] (vanishes! (shift [B i] 1))))
