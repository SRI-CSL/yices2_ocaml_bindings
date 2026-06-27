; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(assert (str.in_re (str.at x 1) (str.to_re "b")))
(assert (not (= (str.at x 1) "b")))
(check-sat)
