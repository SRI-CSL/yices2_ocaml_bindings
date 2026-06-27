; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abc"))
(assert (str.contains x "d"))
(check-sat)
