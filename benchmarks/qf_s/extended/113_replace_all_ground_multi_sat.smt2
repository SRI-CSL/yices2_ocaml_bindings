; EXPECT: sat
(set-logic QF_S)
(assert (= (str.replace_all "ababa" "a" "x") "xbxbx"))
(check-sat)
