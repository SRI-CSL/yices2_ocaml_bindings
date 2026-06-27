; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abc"))
(assert (= (str.len x) 2))
(check-sat)
