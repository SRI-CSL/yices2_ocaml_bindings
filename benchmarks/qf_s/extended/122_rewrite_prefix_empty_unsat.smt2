; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (not (str.prefixof "" x)))
(check-sat)
