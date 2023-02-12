(defunalias = eq)
(defunalias sub -)

(defunalias and +)
(defunalias mul *)

(defunalias add +)

;; Boolean functions
(defpurefun (is-zero e0) (- 1 (* e0 (inv e0))))
(defpurefun (neq a b) (not (eq a b)))
(defpurefun (or e0 e1) (not (and (not e0) (not e1))))
(defpurefun (xor e0 e1) (- (+ e0 e1)
                      (* 2 e0 e1)))
(defpurefun (is-binary e0) (* e0 (- 1 e0)))

;; Chronological forms
(defpurefun (next X) (shift X 1))
(defpurefun (prev X) (shift X -1))
(defpurefun (inc e0 offset) (eq (next e0)
                           (+ e0 offset)))
(defpurefun (dec e0 offset) (eq (next e0)
                           (- e0 offset)))
(defpurefun (remains-constant e0) (will-eq e0 e0))
(defpurefun (didnt-change e0) (eq e0 (prev e0)))
(defpurefun (did-change e0) (neq e0 (prev e0)))
(defpurefun (will-eq e0 e1) (eq (next e0) e1))
(defpurefun (was-eq e0 e1) (eq (prev e0) e1))


;; Helpers
(defpurefun (vanishes e0) e0)
(defpurefun (is-not-zero e0) (if-zero e0 1 0))
(defpurefun (if-eq e0 e1 e2) (if-zero (eq e0 e1) e2))
(defpurefun (if-eq-else e0 e1 e2 e3) (if-zero (eq e0 e1) e2 e3))

;; counter constancy constraint
(defpurefun (counter-constancy ct X)
  (if-not-zero ct
               (didnt-change X)))

;; byte decomposition constraint
(defpurefun (byte-decomposition ct acc bytes)
  (if-zero ct
           (eq acc bytes)
           (eq acc (+ (* 256 (prev acc)) bytes))))

;; plateau constraints
;; underlying assumptions:
;;  - C is counter constant wrt CT
;;  - X is binary
(defpurefun (plateau-constraint CT X C)
  (if-zero C
           (eq X 1)
           (if-zero CT
                    (vanishes X)
                    (if-eq-else CT C
                                (eq X (+ (prev X) 1))
                                (didnt-change X)))))

;; stamp constancy imposes that the column C may only
;; change at rows where the STAMP column changes.
(defpurefun (stamp-constancy STAMP C)
  (if-zero (remains-constant STAMP)
           (remains-constant C)))
