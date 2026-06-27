; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (str.prefixof y x))
(assert (not (str.contains x y)))
(check-sat)
