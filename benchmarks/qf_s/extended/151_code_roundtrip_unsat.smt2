; EXPECT: unsat
(set-logic QF_S)
(assert (not (= (str.to_code (str.from_code 65)) 65)))
(check-sat)
