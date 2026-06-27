; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(assert (str.in_re (str.at x 3) (str.to_re "q")))
(assert (= (str.len x) 3))
(check-sat)
