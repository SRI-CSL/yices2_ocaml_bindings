; EXPECT: unsat
(set-logic QF_S)
(assert (not (= "abc" "abc")))
(check-sat)
