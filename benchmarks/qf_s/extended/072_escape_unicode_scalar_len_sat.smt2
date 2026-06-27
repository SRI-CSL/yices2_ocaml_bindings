; EXPECT: sat
(set-logic QF_S)
(assert (= (str.len "\u{03BB}") 1))
(check-sat)
