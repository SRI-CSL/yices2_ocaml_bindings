; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(assert (>= (str.len x) 2))
(assert (str.in_re (str.at x 1) (str.to_re "")))
(check-sat)
