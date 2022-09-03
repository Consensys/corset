(defcolumns A B C ACC A_3 ACC_2)

(defconstraint test (eq (ith A 3) B))

(defun (acc k) (ith ACC k))

(defconstraint test-acc (eq (acc 2) B))
