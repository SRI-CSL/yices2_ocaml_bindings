; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const c String)
(assert (= x "abc"))
(assert (= c (str.at x 1)))
(assert (str.in_re c (str.to_re "b")))
(assert (not (= (str.at x 0) "b")))
(check-sat)
