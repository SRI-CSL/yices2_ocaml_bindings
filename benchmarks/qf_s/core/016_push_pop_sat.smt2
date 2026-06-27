; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(push 1)
(assert (= (str.++ x "b") "ab"))
(pop 1)
(assert (= x "q"))
(check-sat)
