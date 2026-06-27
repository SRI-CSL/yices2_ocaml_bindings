; EXPECT: sat
(set-logic QF_SNRA)
(declare-const x String)
(declare-const r Real)
(assert (= x "z"))
(assert (= (str.len x) 1))
(assert (= (* r r) 4.0))
(check-sat)
