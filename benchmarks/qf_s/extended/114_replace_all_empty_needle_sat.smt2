; EXPECT: sat
(set-logic QF_S)
(assert (= (str.replace_all "abc" "" "x") "abc"))
(check-sat)
