(defcolumns
  A
  (B :NATURAL)
  (C :ARRAY {3 4 5} :BOOLEAN)
  (D :SORTED (A B C) :BOOLEAN (A))
  (E :COMP (+ B C))
  (F :INTERLEAVED (B C)))
