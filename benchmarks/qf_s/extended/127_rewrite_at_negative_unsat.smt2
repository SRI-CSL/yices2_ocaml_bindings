; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (not (= (str.at x (- 1)) "")))
(check-sat)
