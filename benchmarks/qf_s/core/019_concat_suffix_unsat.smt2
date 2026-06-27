; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= (str.++ x "b") "ac"))
(check-sat)
