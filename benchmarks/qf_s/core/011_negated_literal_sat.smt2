; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (not (= x "abc")))
(check-sat)
