; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(assert (str.in_re (str.at x 2) (re.range "0" "9")))
(assert (<= (str.len x) 2))
(check-sat)
