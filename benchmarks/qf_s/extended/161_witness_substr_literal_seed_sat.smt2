; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(assert (= x "abcd"))
(assert (= (str.substr x 1 2) "bc"))
(check-sat)
