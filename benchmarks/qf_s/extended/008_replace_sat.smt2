; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abc"))
(assert (= (str.replace x "b" "x") "axc"))
(check-sat)
