(defun (neq a b) (not (eq a b)))

(defun (if-one e0 e1) (* e0 e1))
(defun (not e) (- 1 e))

(defun (bin-if-one-else e0 e1 e2)
    (+ (* (- 1 e0) e2)
       (* e0 e1)))
