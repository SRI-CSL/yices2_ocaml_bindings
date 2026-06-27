; EXPECT: unknown
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= (str.++ x "b") (str.++ "a" y)))
(check-sat)
