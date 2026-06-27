; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(assert (= (str.len x) 1))
(assert (< (str.to_code x) 0))
(check-sat)
