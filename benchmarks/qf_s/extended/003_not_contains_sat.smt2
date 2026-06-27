; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abc"))
(assert (not (str.contains x "d")))
(check-sat)
