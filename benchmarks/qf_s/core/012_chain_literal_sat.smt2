; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x y))
(assert (= y "hi"))
(check-sat)
