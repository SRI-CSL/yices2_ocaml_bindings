; EXPECT: unsat
(set-logic QF_S)
(assert (not (= (str.to_code "ab") (- 1))))
(check-sat)
