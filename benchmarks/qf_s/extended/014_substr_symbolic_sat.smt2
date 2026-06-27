; EXPECT: sat
(set-logic QF_SLIA)
(declare-fun x () String)
(assert (= (str.substr x 1 3) "bcd"))
(check-sat)
