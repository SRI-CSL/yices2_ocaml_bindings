; EXPECT: sat
(set-logic QF_SLIA)
(declare-fun x () String)
(assert (= (str.replace x "b" "x") "axc"))
(assert (not (= x "axc")))
(check-sat)
