; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (str.to_re "a")))
(assert (str.in_re x (str.to_re "b")))
(check-sat)
