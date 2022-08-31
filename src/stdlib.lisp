(defun (neq a b) (not (eq a b)))
(defun (or e0 e1) (not (and (not e0) (not e1))))
(defun (xor e0 e1) (- (+ e0 e1)
                       (* 2 e0 e1)))

(defun (bin-if-one-else e0 e1 e2) (+ (* (- 1 e0) e2)
                                     (* e0 e1)))

(defun (bin-if-zero-else e0 e1 e2) (+ (* (- 1 e0) e1)
                                      (* e0 e2)))

(defun (bin-it-not-zero e0 e1) (* e0 e1))
(defun (bin-if-zero e0 e1) (bin-if-not-zero (not e0) e1))
(defunalias bin-if-one mul)


;; TODO FIXME
(defun (inc e0 offset) (eq (shift e0 1)
                          (+ e0 offset)))

(defun (dec e0 offset) (eq (shift e0 1)
                          (- e0 offset)))

(defun (remains-constant e0) (will-eq e0 e0))
(defun (didnt-change e0) (eq 0 (shift e0 -1)))
(defun (will-eq e0 e1) (eq (shift e0 1) e1))
(defun (was-eq e0 e1) (eq (shift e0 -1) e1))

(defun (if-not-zero-else e0 e1 e2) (+ (if-zero e0 e2)
                                      (* e0 e1)))

(defun (is-zero-binary e0) (if-zero-else e0 1 0))
(defun (is-non-zero-binary e0) (not (if-zero-else e0 1 0)))
(defun (if-zero-else e0 e1 e2) (+ (if-zero-then e0 e1)
                                  (+ (* e0 e2))))
(defun (is-binary e0) (* e0 (- 1 e0)))
