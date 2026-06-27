; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(assert (str.in_re (str.replace_all x "a" "b") (str.to_re "bbb")))
(assert (= (str.len x) 2))
(check-sat)
