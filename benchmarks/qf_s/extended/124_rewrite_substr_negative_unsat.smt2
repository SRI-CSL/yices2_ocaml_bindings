; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (not (= (str.substr x (- 1) 2) "")))
(check-sat)
