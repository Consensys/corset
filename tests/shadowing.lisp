(defcolumns A B C)

(defun (plusB A) (+ A B))

(defconstraint shadow (plusB C))
