; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const c Int)
(assert (= x "A"))
(assert (= c 66))
(assert (= c (str.to_code x)))
(check-sat)
