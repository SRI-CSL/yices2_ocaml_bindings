; EXPECT: unsat
(set-logic QF_SNIA)
(declare-const x String)
(assert (= x "abc"))
(assert (= (* (str.len x) (str.len x)) 4))
(check-sat)
