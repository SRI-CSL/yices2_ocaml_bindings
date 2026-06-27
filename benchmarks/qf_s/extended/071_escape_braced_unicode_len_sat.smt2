; EXPECT: sat
(set-logic QF_S)
(assert (= "\u{61}" "a"))
(check-sat)
