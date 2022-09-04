(defcolumns A B[1:10])

(defconstraint test1 (for i {1 2 3} (eq A (ith B i))))

(defconstraint test2 (for i [1:5] (eq A (ith B i))))

(defconstraint test3 (for i [1:5]
                          (for j {2 4 6}
                               (for k {1 5} (eq (ith B i) (ith B j))))))
