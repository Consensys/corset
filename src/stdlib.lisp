(defunalias sub -)
(defunalias eq -)
(defunalias = -)

(defunalias and *)
(defunalias mul *)

(defunalias add +)

;; Boolean functions
(defun (is-zero e0) (- 1 (* e0 (inv e0))))
(defun (neq a b) (not (eq a b)))
(defun (or e0 e1) (not (and (not e0) (not e1))))
(defun (xor e0 e1) (- (+ e0 e1)
                      (* 2 e0 e1)))
(defun (is-binary e0) (* e0 (- 1 e0)))

;; Chronological forms
(defun (inc e0 offset) (eq (shift e0 1)
                           (+ e0 offset)))
(defun (dec e0 offset) (eq (shift e0 1)
                           (- e0 offset)))
(defun (remains-constant e0) (will-eq e0 e0))
(defun (didnt-change e0) (eq e0 (shift e0 -1)))
(defun (did-change e0) (neq e0 (shift e0 -1)))
(defun (will-eq e0 e1) (eq (shift e0 1) e1))
(defun (was-eq e0 e1) (eq (shift e0 -1) e1))


;; Helpers
(defun (vanishes e0) e0)
(defun (is-not-zero e0) (if-zero e0 1 0))
(defun (if-eq e0 e1 e2) (if-zero (eq e0 e1) e2))
(defun (if-eq-else e0 e1 e2 e3) (if-zero (eq e0 e1) e2 e3))

;; counter constancy constraint
(defun (counter-constancy ct X)
            (if-not-zero ct
                (didnt-change X)))

;; byte decomposition constraint
(defun (byte-decomposition ct acc bytes)
            (if-zero ct
                (eq acc bytes)
                (eq acc (+ (* 256 (shift acc -1)) bytes))))

;; plateau constraints
;; underlying assumptions:
;;  - C is counter constant wrt CT
;;  - X is binary
(defun (plateau-constraint CT X C)
            (if-zero C
                (eq X 1)
                (begin
                    (if-zero CT
                        (vanishes X)
                        (if-eq-else CT C
                            (eq X (+ (shift X -1) 1))
                            (eq (didnt-change X)))))))