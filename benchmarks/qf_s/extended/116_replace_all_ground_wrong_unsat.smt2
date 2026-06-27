; EXPECT: unsat
(set-logic QF_S)
(assert (= (str.replace_all "ababa" "a" "x") "ababa"))
(check-sat)
