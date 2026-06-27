; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x (str.++ "a" y "d")))
(assert (= y (str.++ "b" "c")))
(assert (str.contains x (str.++ "b" (str.++ "c" ""))))
(check-sat)
