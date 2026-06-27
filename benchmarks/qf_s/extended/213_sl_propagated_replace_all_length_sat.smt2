; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace_all x "a" "b")))
(assert (str.in_re y (str.to_re "bb")))
(assert (= (str.len x) 2))
(check-sat)
