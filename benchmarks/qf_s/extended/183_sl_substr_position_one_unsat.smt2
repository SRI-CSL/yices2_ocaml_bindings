; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (= y (str.substr x 1 1)))
(assert (str.in_re y (str.to_re "b")))
(assert (not (= (str.at x 1) "b")))
(check-sat)
