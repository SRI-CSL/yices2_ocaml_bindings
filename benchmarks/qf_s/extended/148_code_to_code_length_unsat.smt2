; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(assert (= (str.len x) 2))
(assert (not (= (str.to_code x) (- 1))))
(check-sat)
