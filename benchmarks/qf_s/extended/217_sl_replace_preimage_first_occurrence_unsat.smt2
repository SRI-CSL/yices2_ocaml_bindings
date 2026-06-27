; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace x "a" "b")))
(assert (str.in_re y (str.to_re "bab")))
(assert (str.in_re x (str.to_re "baa")))
(check-sat)
