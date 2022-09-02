(defcolumns G H)
(defalias X G
  Y I
  Z H)
(defcolumns I)

(eq (+ G I H) (+ X Y (fds Y H)))

(defun (sdf E R) (+ E R))

(defunalias fds sdf)
