; EXPECT: sat
(set-logic QF_S)
(assert (= (str.to_code "A") 65))
(check-sat)
