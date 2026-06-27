; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const n Int)
(assert (= x "abc"))
(assert (= n (+ (str.len x) 2)))
(assert (>= n 5))
(check-sat)
