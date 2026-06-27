; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (= y "bc"))
(assert (= y (str.substr x 1 2)))
(assert (not (= (str.at x 2) "c")))
(check-sat)
