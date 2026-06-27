; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const c String)
(assert (= c (str.at x 2)))
(assert (str.in_re c (re.range "a" "z")))
(assert (<= (str.len x) 2))
(check-sat)
