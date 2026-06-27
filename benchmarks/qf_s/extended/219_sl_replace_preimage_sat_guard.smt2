; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x "ab"))
(assert (= y (str.replace x "a" "b")))
(assert (str.in_re y (str.to_re "bb")))
(check-sat)
