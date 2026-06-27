; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= (str.len x) 4))
(check-sat)
