; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(assert (= x "abcd"))
(assert (= (str.indexof x "bc" 0) 1))
(check-sat)
