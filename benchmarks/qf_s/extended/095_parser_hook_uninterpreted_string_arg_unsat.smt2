; EXPECT: unsat
(set-logic QF_SLIA)
(declare-fun f (String) Int)
(assert (= (f "a") 1))
(assert (= (f "a") 2))
(check-sat)
