(defcolumns A[1:3] B C ACC[1:2])

(defconstraint test (eq (ith A 3) B))

(defun (acc k) (ith ACC k))

(defconstraint test-acc (eq (acc 2) B))
