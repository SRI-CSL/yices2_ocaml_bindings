; EXPECT: sat
(set-logic QF_S)
(assert (= (str.++ "a" "b" "c") "abc"))
(check-sat)
