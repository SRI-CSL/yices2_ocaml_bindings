; EXPECT: unsat
(set-logic QF_S)
(assert (distinct "a" "a"))
(check-sat)
