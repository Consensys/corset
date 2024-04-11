(defcolumns X0 Y0)

(defalias
  X1 X0
  Y1 Y0
  X2 X0
  Y2 Y0)

(defcolumns Z)

;; add
(defconstraint add_x0_y0 () (add X0 Y0))
(defconstraint add_x1_y0 () (add X1 Y0))
(defconstraint add_x2_y0 () (add X2 Y0))
(defconstraint add_x0_y1 () (add X0 Y1))
(defconstraint add_x1_y1 () (add X1 Y1))
(defconstraint add_x2_y1 () (add X2 Y1))
(defconstraint add_x0_y2 () (add X0 Y2))
(defconstraint add_x1_y2 () (add X1 Y2))
(defconstraint add_x2_y2 () (add X2 Y2))
;; daa
(defconstraint daa_x0_y0 () (daa X0 Y0))
(defconstraint daa_x1_y0 () (daa X1 Y0))
(defconstraint daa_x2_y0 () (daa X2 Y0))
(defconstraint daa_x0_y1 () (daa X0 Y1))
(defconstraint daa_x1_y1 () (daa X1 Y1))
(defconstraint daa_x2_y1 () (daa X2 Y1))
(defconstraint daa_x0_y2 () (daa X0 Y2))
(defconstraint daa_x1_y2 () (daa X1 Y2))
(defconstraint daa_x2_y2 () (daa X2 Y2))
;;
(defconstraint eq_x0_z () (if-not-zero X0 (eq! X0 Z)))
(defconstraint eq_x1_z () (if-not-zero X0 (eq! X1 Z)))
(defconstraint eq_x2_z () (if-not-zero X0 (eq! X2 Z)))
;;
(defun (add E R) (+ E R))
;;
(defunalias daa add)
