(defcolumns A (B :ARRAY [1:10]))

(defconstraint test1 () (for i {1 2 3} (eq A (nth B i))))

(defconstraint test2 () (for i [1:5] (eq A (nth B i))))

(defconstraint test3 () (for i [1:5]
                          (for j {2 4 6}
                               (for k {1 5} (eq (nth B i) (nth B j))))))

(defconstraint test4 () (for i [5] (shift (nth B i) 1)))
