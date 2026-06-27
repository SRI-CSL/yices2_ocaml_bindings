; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const c String)
(assert (= c (str.at x 1)))
(assert (str.in_re c (str.to_re "b")))
(assert (not (= (str.at x 1) "b")))
(check-sat)
