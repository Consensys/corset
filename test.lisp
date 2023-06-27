(defcolumns A (B :display :hex :display :dec) (C :array [1:4]))
(definterleaved D (A [C 2] B ))

;;(defcolumns (D :interleaved (A [C 2])))
