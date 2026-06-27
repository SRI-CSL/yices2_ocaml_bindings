; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= (str.++ "a" x) "bc"))
(check-sat)
