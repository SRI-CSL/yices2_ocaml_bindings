; EXPECT: sat
(set-logic QF_S)
(assert (= (str.len "") 0))
(check-sat)
