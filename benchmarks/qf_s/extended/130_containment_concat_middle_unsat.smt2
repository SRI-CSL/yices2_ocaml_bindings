; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x (str.++ "p" y "q")))
(assert (not (str.contains x y)))
(check-sat)
