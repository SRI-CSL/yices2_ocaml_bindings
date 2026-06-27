; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= (str.++ x "b") "ab"))
(check-sat)
