; EXPECT: sat
(set-logic QF_SNIA)
(declare-const x String)
(assert (= x "ab"))
(assert (= (* (str.len x) (str.len x)) 4))
(check-sat)
