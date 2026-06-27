; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace_all x "a" "aa")))
(assert (str.in_re y (str.to_re "aa")))
(assert (str.in_re x (str.to_re "aa")))
(check-sat)
