; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (= x "abc"))
(assert (= y (str.substr x 1 2)))
(assert (str.in_re y (str.to_re "bc")))
(assert (not (= (str.at x 0) "b")))
(check-sat)
