; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= (str.++ x "z") "az"))
(check-sat)
(check-sat)
