(defunalias = eq!)
(defunalias debug-assert debug)

(defunalias if-zero if!)
(defunalias if-not-zero if)

(defpurefun ((force-bool :boolean :nowarn) x) x)
(defpurefun ((is-binary :loob :nowarn) e0) (* e0 (- 1 e0)))

;;
;; Boolean functions
;;
;; !-suffix denotes loobean algebra (i.e. 0 == true)
;; ~-prefix denotes normalized-functions (i.e. output is 0/1)
(defpurefun (and a b) (* a b))
(defpurefun ((~and :bool) a b) (~ (and a b)))
(defpurefun ((and! :loob) a b) (+ a b))
(defpurefun ((~and! :bool) a b) (~ (and! a b)))

(defpurefun (or a b) (+ a b))
(defpurefun ((~or :boolean) a b) (~ (or a b)))
(defpurefun ((or! :loob) a b) (* a b))
(defpurefun ((~or! :boolean) a b) (~ (or! a b)))

(defpurefun ((not :boolean :nowarn) (x :boolean)) (- 1 x))

(defpurefun ((eq! :loobean :nowarn) x y) (- x y))

(defpurefun ((eq :boolean :nowarn) (x :boolean) (y :boolean)) (^ (- x y) 2))
(defpurefun ((eq :boolean :nowarn) x y) (~ (- x y)))


;; Variadic versions of and/or
(defunalias any +)
(defunalias any! *)
(defunalias all *)
(defunalias all! +)

;; Boolean functions
(defpurefun (is-not-zero e0) (* e0 (inv e0)))
(defpurefun (is-zero e0) (- 1 (* e0 (inv e0))))
(defpurefun ((neq! :loob :nowarn) a b) (not (~ (eq! a b))))



;; Chronological functions
(defpurefun (next X) (shift X 1))
(defpurefun (prev X) (shift X -1))

;; Ensure that e0 has (resp. will) increase (resp. decrease) of offset
;; w.r.t. the previous (resp. next) row.
(defpurefun (did-inc! e0 offset) (eq! e0 (+ (prev e0) offset)))
(defpurefun (did-dec! e0 offset) (eq!  e0 (- (prev e0) offset)))
(defpurefun (will-inc! e0 offset) (will-eq! e0 (+ e0 offset)))
(defpurefun (will-dec! e0 offset) (eq! (next e0) (- e0 offset)))

;; Ensure (in loobean logic) that e0 remained (resp. will be) constant
;; with regards to the previous (resp. next) row.
(defpurefun (remained-constant! e0) (eq! e0 (prev e0)))
(defpurefun (will-remain-constant! e0) (will-eq! e0 e0))

;; Ensure (in loobean logic) that e0 has changed (resp. will change) its value
;; with regards to the previous (resp. next) row.
(defpurefun (did-change! e0) (neq! e0 (prev e0)))
(defpurefun (will-change! e0) (neq! e0 (next e0)))

;; Ensure (in loobean logic) that e0 was (resp. will be) equal to e1 in the
;; previous (resp. next) row.
(defpurefun (was-eq! e0 e1) (eq! (prev e0) e1))
(defpurefun (will-eq! e0 e1) (eq! (next e0) e1))


;; Helpers
(defpurefun ((vanishes! :loob :nowarn) e0) e0)
(defpurefun (if-eq x val then) (if! (eq! x val) then))
(defpurefun (if-eq-else x val then else) (if! (eq! x val) then else))

;; counter constancy constraint
(defpurefun ((counter-constancy :loob) ct X)
  (if-not-zero ct
               (remained-constant! X)))

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
                  (if! CT
                       (vanishes! X)
                       (if! (eq!  CT C)
                            (did-inc! X 1)
                            (remained-constant! X))))))

;; stamp constancy imposes that the column C may only
;; change at rows where the STAMP column changes.
(defpurefun (stamp-constancy STAMP C)
  (if! (will-remain-constant! STAMP)
       (will-remain-constant! C)))
