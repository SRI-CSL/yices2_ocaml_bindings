; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (str.to_re "a")))
(assert (not (str.in_re x (str.to_re "b"))))
(check-sat)
