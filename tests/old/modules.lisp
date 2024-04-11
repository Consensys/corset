(defcolumns A B)
(defconstraint asdf () (eq A B))
;; (defconstraint asdf () (eq A C))
;; (defcolumns A B)

(module mod2)
(defcolumns A B C)
(defconstraint asdf () (eq A C))
