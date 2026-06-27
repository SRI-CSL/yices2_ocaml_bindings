; EXPECT: unsat
(set-logic QF_S)
(assert (not (= (str.from_code (- 1)) "")))
(check-sat)
