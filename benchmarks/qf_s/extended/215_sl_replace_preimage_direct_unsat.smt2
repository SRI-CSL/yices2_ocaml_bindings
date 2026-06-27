; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re (str.replace x "a" "b") (str.to_re "bb")))
(assert (str.in_re x (str.to_re "cc")))
(check-sat)
