(defcolumns (A :bool) B C)

(defperspective set1
  A
  (X Y Z))

(defperspective set2
  B
  (T (U :bool) V))

;; (defcolumns
;;   (ZY :interleaved (Z Y X))
;;   (TU :interleaved (T U)))

;; (defpermutation (ZYs TUs) ((+ ZY) (- TU)))

(defconstraint asdf () (= X T))

(defconstraint pipo () (mul B C))

(defconstraint asdf () (= X B ( set1\A)))

;; (defconstraint pipo2 () (+ ZY 5))

;; (defconstraint pipo3 () (+ ZYs 5))
