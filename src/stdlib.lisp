(defunalias = eq!)
(defunalias debug-assert debug)

(defunalias if-zero if!)
(defunalias if-not-zero if)

(defpurefun ((force-bool :boolean :nowarn) x) x)

;;
;; Boolean functions
;;
;; !-suffix denotes reverse boolean algebra (i.e. 0 == true)
;; ~-prefix denotes normalized-functions (i.e. output is 0/1)
(defpurefun (aand a b) (* a b))
(defpurefun ((~and :bool) a b) (~ (and a b)))
(defpurefun ((and! :loob) a b) (+ a b))
(defpurefun ((~and! :bool) a b) (~ (and! a b)))

(defpurefun (oor a b) (+ a b))
(defpurefun ((~or :boolean) a b) (~ (or a b)))
(defpurefun ((or! :loob) a b) (* a b))
(defpurefun ((~or! :boolean) a b) (~ (or! a b)))

(defpurefun ((not :boolean) (x :boolean)) (- 1 x))

;; (defunalias ~not not)
;; (defpurefun ((not! :loob) a) (~ a))
;; (defpurefun (~not! a) (~ a))

;; (defpurefun (eq! a b) (- a b))
;; (defpurefun (~eq a b) (~ (eq a b)))

;; Variadic versions of and/or
(defunalias either +)
(defunalias either! *)
(defunalias all *)
(defunalias all! +)

;; Boolean functions
(defpurefun (is-not-zero e0) (* e0 (inv e0)))
(defpurefun (is-zero e0) (- 1 (* e0 (inv e0))))
(defpurefun (neq a b) (not (eq a b)))
(defpurefun (is-binary e0) (* e0 (- 1 e0)))

;; Chronological functions
(defpurefun (next X) (shift X 1))
(defpurefun (prev X) (shift X -1))
(defpurefun (will-inc e0 offset) (will-eq e0 (+ e0 offset)))
(defpurefun (did-inc e0 offset) (eq! e0 (+ (prev e0) offset)))

(defpurefun (will-dec e0 offset) (eq! (next e0) (- e0 offset)))
(defpurefun (did-dec e0 offset) (eq!  e0 (- (prev e0) offset)))

(defpurefun (remains-constant e0) (will-eq e0 e0)) ;; FIXME: new name
(defpurefun (didnt-change e0) (- e0 (prev e0))) ;; FIXME: new name
(defpurefun (did-change e0) (neq e0 (prev e0)))

(defpurefun (will-eq e0 e1) (eq! (next e0) e1))
(defpurefun (was-eq e0 e1) (eq! (prev e0) e1))


;; Helpers
(defpurefun ((vanishes :loob :nowarn) e0) e0)
(defpurefun (if-eq e0 e1 e2) (if! (eq! e0 e1) e2))
(defpurefun (if-eq-else e0 e1 e2 e3) (if! (eq! e0 e1) e2 e3))

;; counter constancy constraint
(defpurefun (counter-constancy ct X)
  (if-not-zero ct
               (didnt-change X)))

;; byte decomposition constraint
(defpurefun (byte-decomposition ct acc bytes)
  (if-zero ct
           (eq! acc bytes)
           (eq! acc (+ (* 256 (prev acc)) bytes))))

;; plateau constraints
(defpurefun (plateau-constraint CT (X :boolean) C)
  (begin (debug-assert (stamp-constancy CT C))
         (if-zero C
                  (eq! X 1)
                  (if-zero CT
                           (vanishes X)
                           (if-eq-else CT C
                                       (eq! X (+ (prev X) 1)) ;; TODO: did-inc
                                       (didnt-change X))))))

;; stamp constancy imposes that the column C may only
;; change at rows where the STAMP column changes.
(defpurefun (stamp-constancy STAMP C)
  (if-zero (remains-constant STAMP)
           (remains-constant C)))
