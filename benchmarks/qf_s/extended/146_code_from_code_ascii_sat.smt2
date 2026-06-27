; EXPECT: sat
(set-logic QF_S)
(assert (= (str.from_code 65) "A"))
(check-sat)
