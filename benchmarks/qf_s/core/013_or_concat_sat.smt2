; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (or (= (str.++ x "b") "ab") false))
(check-sat)
