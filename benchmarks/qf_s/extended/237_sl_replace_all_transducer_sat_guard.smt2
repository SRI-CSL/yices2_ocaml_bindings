; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x "abx"))
(assert (= y (str.replace_all x "ab" "q")))
(assert (str.in_re y (str.to_re "qx")))
(check-sat)
