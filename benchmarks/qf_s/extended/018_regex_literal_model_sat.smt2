; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (str.to_re "abc")))
(check-sat)
