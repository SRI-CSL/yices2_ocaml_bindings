; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= (str.++ "a" x) "ab"))
(check-sat)
