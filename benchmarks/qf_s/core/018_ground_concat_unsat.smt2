; EXPECT: unsat
(set-logic QF_S)
(assert (= (str.++ "a" "c") "ab"))
(check-sat)
