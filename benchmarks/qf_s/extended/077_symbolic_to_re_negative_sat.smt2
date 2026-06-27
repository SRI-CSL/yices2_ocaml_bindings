; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (not (str.in_re x (str.to_re y))))
(assert (= x "ab"))
(assert (= y "ac"))
(check-sat)
