; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= (str.++ "a" x "c") "abc"))
(check-sat)
