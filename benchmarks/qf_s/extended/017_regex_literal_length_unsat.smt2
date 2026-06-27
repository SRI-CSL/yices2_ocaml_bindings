; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (str.to_re "abc")))
(assert (= (str.len x) 2))
(check-sat)
