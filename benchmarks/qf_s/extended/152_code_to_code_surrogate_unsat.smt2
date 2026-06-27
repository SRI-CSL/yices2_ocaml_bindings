; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(assert (= (str.len x) 1))
(assert (>= (str.to_code x) 55296))
(assert (<= (str.to_code x) 57343))
(check-sat)
