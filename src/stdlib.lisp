(defun (+ a b) (add a b))
(defun (= a b) (eq a b))
(defun (- a b) (sub a b))
(defun (* a b) (mul a b))

(defun (neq a b) (not (eq a b)))

(defun (if-one e0 e1) (mul e0 e1))
(defun (if-zero e0 e1) (if-one (not e0) e1))
(defun (not e) (- 1 e))
