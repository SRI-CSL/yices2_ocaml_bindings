; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (not (str.in_re x (str.to_re "a"))))
(assert (= (str.len x) 1))
(check-sat)
