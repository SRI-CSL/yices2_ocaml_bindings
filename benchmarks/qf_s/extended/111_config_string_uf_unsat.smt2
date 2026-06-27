; EXPECT: unsat
(set-logic QF_SLIA)
(declare-fun f (String) String)
(assert (= (f "a") "b"))
(assert (= (str.len (f "a")) 2))
(check-sat)
