; EXPECT: sat
(set-logic QF_SLIA)
(declare-fun f (String) String)
(assert (= (f "a") "b"))
(assert (= (str.len (f "a")) 1))
(check-sat)
