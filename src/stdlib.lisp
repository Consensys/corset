(defun (neq a b) (not (eq a b)))

(defun (if-one e0 e1) (* e0 e1))
(defun (if-zero e0 e1) (if-one (not e0) e1))
(defun (not e) (- 1 e))
