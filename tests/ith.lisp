(defcolumns (A:ARRAY[1:3]) B C (ACC :ARRAY [1:2]))

(defconstraint test () (eq (nth A 3) B))

(defun (acc k) (nth ACC k))

(defconstraint test-acc () (eq (acc 2) B))
