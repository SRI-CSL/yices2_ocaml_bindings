; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.contains "" x))
(assert (= (str.len x) 1))
(check-sat)
