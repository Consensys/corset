(defcolumns
  A
  (B :NATURAL)
  (C :ARRAY {3 4 5} :BOOLEAN)
  (E :COMP (+ B C))
  (F :INTERLEAVED (B C)))
