; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re (str.replace_all x "ab" "x") (str.to_re "xx")))
(assert (str.in_re x (str.to_re "aba")))
(check-sat)
