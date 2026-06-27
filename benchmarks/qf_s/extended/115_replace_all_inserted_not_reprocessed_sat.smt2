; EXPECT: sat
(set-logic QF_S)
(assert (= (str.replace_all "a" "a" "aa") "aa"))
(check-sat)
