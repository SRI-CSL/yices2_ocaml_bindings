; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= (str.++ "a" x "c") "xbc"))
(check-sat)
