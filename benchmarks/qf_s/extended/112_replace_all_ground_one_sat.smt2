; EXPECT: sat
(set-logic QF_S)
(assert (= (str.replace_all "abc" "b" "x") "axc"))
(check-sat)
