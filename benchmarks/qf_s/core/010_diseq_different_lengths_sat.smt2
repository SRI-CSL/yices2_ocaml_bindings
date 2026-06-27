; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (not (= x y)))
(assert (= (str.len x) 1))
(assert (= (str.len y) 2))
(check-sat)
