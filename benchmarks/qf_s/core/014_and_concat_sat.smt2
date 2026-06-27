; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (and (= (str.++ "a" x) "ab") (= (str.len x) 1)))
(check-sat)
