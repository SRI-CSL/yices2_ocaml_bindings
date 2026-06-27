; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abcd"))
(assert (= (str.replace x "bc" "XY") "aXYd"))
(check-sat)
