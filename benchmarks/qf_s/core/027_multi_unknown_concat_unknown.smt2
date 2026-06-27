; EXPECT: unknown
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= (str.++ x y) "ab"))
(check-sat)
