; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (= y (str.substr x (- 1) 1)))
(assert (= y ""))
(assert (not (= (str.at x 0) "a")))
(check-sat)
