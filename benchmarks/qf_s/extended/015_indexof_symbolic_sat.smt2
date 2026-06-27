; EXPECT: sat
(set-logic QF_SLIA)
(declare-fun x () String)
(assert (= (str.indexof x "bc" 0) 1))
(check-sat)
